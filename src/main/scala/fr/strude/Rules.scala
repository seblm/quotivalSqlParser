package fr.strude

import fr.strude.Parser._
import scalaz._
import Scalaz._

case class Rule(func : QuotivalFields => Validation[Void, String],
                precondition : (QuotivalData => Boolean) = (data: QuotivalData) => true) {

  def this(func : QuotivalFields => (Boolean, String),
           precondition : (QuotivalData => Boolean) = (data: QuotivalData) => true) = {
      this((fields : QuotivalFields) => {
        func(fields) match {
          case (true, _) => Void.success
          case (false, message) => message.failure
        }
      }, precondition)
  }


  def checkPreconditions(data : QuotivalData) : Boolean = precondition(data)

  def validate(data : QuotivalData) : Validation[Void, String] = if(checkPreconditions(data)) func (data.fields) else Void.success

}
object Rules {
    type Rules = Set[Rule]

    private val precondProduct = (data: QuotivalData) => data.tableName.equalsIgnoreCase( "Product" )
    private val precondPackage = (data: QuotivalData) => data.tableName.equalsIgnoreCase( "Package" )

    val globalRules = Set(
        // General
        Rule(f => (!f.isEmpty, "Pas de champ renseigné")),
        // marketStatus
        Rule(f => (!(f.contains("marketStatus") && """^[012345]$""".r.findFirstIn(f("marketStatus")) == None), "Le champ marketStatus n'est pas formaté convenablement (1 chiffe)")),
        Rule(f => (if (f.contains("marketStatus") && (f("marketStatus") == "4" || f("marketStatus") == "5" || f("marketStatus") == "1" || f("marketStatus") == "2") && f.contains("offMarketDate")) f("offMarketDate") != "NULL" else true, "marketStatus ,1 2, 4 ou 5 doit peut pas avoir un offMarketDate à NULL")),
        // offMarketDate
        Rule(f => (if (f.contains("offMarketDate") && """(^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2})$""".r.findFirstIn(f("offMarketDate")) == None) f("offMarketDate") == "NULL" else true, "Le champ offMarketDate ne respecte pas le format 'yyyy-mm-dd hh:mm:ss' (ou NULL)")),
        // onMarketDate
        Rule(f => (if (f.contains("onMarketDate") && """^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}$""".r.findFirstIn(f("onMarketDate")) == None) f("onMarketDate") == "NULL" else true, "Le champ onMarketDate ne respecte pas le format 'yyyy-mm-dd hh:mm:ss' (ou NULL)")),
        // refundingRate
        Rule(f => (!(f.contains("refundingRate") && """^[1247NPT]|NULL$""".r.findFirstIn(f("refundingRate")) == None), "Le champ refundingRate n'est pas formaté convenablement [1247NPT]|NULL")),
        // general
        Rule(f => ((for {
            fi <- f.keys
            if (!(List("marketStatus", "refundingRate", "offMarketDate", "onMarketDate", "refundingBase", "actCode", "actCodeName", "publicPrice", "vatRate", "tfr").contains(fi)))
        } yield (false)).isEmpty, "seuls les champs marketStatus, offMarketDate, onMarketDate, refundingRate, actCode, actCodeName, publicPrice, vatRate and tfr sont acceptés pour un package"), precondPackage),
        // refundingRate && refundingBase
        Rule(f => (!(f.contains("refundingRate") && """^[1247NT]|NULL$""".r.findFirstIn(f("refundingRate")) == None), "Le champ refundingRate ne peut pas prendre la valeur 'P' dans un package [1247NT]|NULL"), precondPackage),
        Rule(f => (if (f.contains("refundingBase") && """^[0-9]+(\.[0-9]+)?$""".r.findFirstIn(f("refundingBase")) == None) f("refundingBase") == "NULL" else true, "Le champ refundingBase n'est pas correctement formaté [x(.y)]"), precondPackage),
        Rule(f => (if (f.contains("refundingRate") && f("refundingRate") == "N" && f.contains("refundingBase")) f("refundingBase") == "NULL" else true, "Si le champ refundingRate prend la valeur N alors le refundingBase ne peut pas prendre de valeur"), precondPackage),
        // publicPrice
        Rule(f => (if (f.contains("publicPrice") && """^[0-9]+(\.[0-9]+)?$""".r.findFirstIn(f("publicPrice")) == None) f("publicPrice") == "NULL" else true, "Le champ publicPrice n'est pas correctement formaté [x(.y)]"), precondPackage),
        // actCode && actCodeName
        Rule(f => (if (f.contains("actCode")) f.contains("actCodeName") else true, "Si l'actCode est modifié, alors l'actCodeName doit l'etre aussi"), precondPackage),
        Rule(f => (if (f.contains("actCode") && f.contains("actCodeName")) List(("PH7", "pharmacie 65%"), ("PHN", "pharmacie non remboursable"), ("PH2", "pharmacie 15%"), ("PH4", "pharmacie 30%"), ("PH1", "pharmacie 100%"),
            ("TNS", "traitement nicotinique de substitution"), ("GLU", "aliments sans gluten"), ("DVO", "Orthèses diverses"), ("OPT", "optique"),
            ("MAD", "matériels et appareils de traitements divers"), ("PAN", "pansements"), ("PA", "orthèses"), ("AAD", "autres accessoires traitement à domicile"),
            ("MAC", "materiels et appareils de contention"), ("ARO", "aérosols"), ("AAR", "appareillage assistance respiratoire"), ("PEX", "prothèse externe non orthopédique"),
            ("PII", "prothèse interne inerte"), ("PMH", "préparation magistrale homéopathique"), ("NULL", "NULL")).contains((f("actCode"), f("actCodeName")))
        else true, "Les champ actCode et actCodeName sont mal formatés (soit incohérents soit avec de mauvaises valeurs)"), precondPackage),
        // vatRate
        Rule(f => (if (f.contains("vatRate")) List("2.10", "7.00", "5.50", "19.60", "NULL").contains(f("vatRate")) else true, "Le vat n'a pas les bonnes valeurs"), precondPackage),
        Rule(f => (if (f.contains("vatRate") && f("vatRate") == "NULL" && f.contains("refundingBase")) f("refundingBase") == "NULL" else true, "Le vat ne peut pas prendre la valeur NULL si il y a un refundingBase non nul"), precondPackage),
        Rule(f => (if (f.contains("vatRate") && f("vatRate") == "NULL" && f.contains("publicPrice")) f("publicPrice") == "NULL" else true, "Le vatRate ne peut pas prendre la valeur NULL si il y a un  publicPrice non nul"), precondPackage),
        // tfr
        Rule(f => (if (f.contains("tfr")) """^[01]$""".r.findFirstIn(f("tfr")) == None else true, "Le champ tfr ne peut pas avoir une valeur différente de 0 ou 1."), precondPackage),
        // general
        Rule(f => ((for {
            fi <- f.keys
            if (!(List("marketStatus", "refundingRate", "offMarketDate", "onMarketDate").contains(fi)))
        } yield (false)).isEmpty, "seuls les champs marketStatus, offMarketDate, onMarketDate and refundingRate sont acceptés pour un product"), precondProduct)
    )
}
