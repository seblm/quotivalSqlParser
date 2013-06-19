package fr.strude

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import Parser.QuotivalData

import Rules._

class RulesTest extends FunSuite with ShouldMatchers {

    /////////////
    // General //
    /////////////

    test("check all rules was valid for a single field") {
        val q = QuotivalData("package", Map("publicPrice" -> "4.66"), 10)
        generalRules.map(rule => rule(q.fields)._1).filter(_ == false) should be(Set())
    }

    test("Verifies that a marketStatus cannot have 4 or 5 where there is not offMarketDate") {
        val p = QuotivalData("package", Map("marketStatus" -> "5"), 20)
        val q = QuotivalData("package", Map("marketStatus" -> "4"), 21)
        val r = QuotivalData("product", Map("marketStatus" -> "1"), 22)
        val s = QuotivalData("product", Map("marketStatus" -> "4", "offMarketDate" -> "2013-05-29 00:00:00"), 23)

        generalRules.map(rule => rule(p.fields)._1).filter(_ == false) should have size 1
        generalRules.map(rule => rule(q.fields)._1).filter(_ == false) should have size 1
        generalRules.map(rule => rule(r.fields)._1).filter(_ == false) should have size 0
        generalRules.map(rule => rule(s.fields)._1).filter(_ == false) should have size 0
    }

    test("Verifies that a marketStatus must have 1 number") {
        val p = QuotivalData("package", Map("marketStatus" -> "a"), 30)
        val q = QuotivalData("package", Map("marketStatus" -> "12"), 31)

        generalRules.map(rule => rule(p.fields)._1).filter(_ == false) should have size 1
        generalRules.map(rule => rule(q.fields)._1).filter(_ == false) should have size 1
    }

    test("Verifies that offMarketDate must be 2013-05-29 00:00:00 or NULL") {
        val p = QuotivalData("package", Map("offMarketDate" -> "2013-05-29 00:00:00"), 40)
        val q = QuotivalData("product", Map("offMarketDate" -> "2013-ab-29 00:00:00"), 41)
        val r = QuotivalData("package", Map("offMarketDate" -> "2013-05-29"), 42)
        val s = QuotivalData("package", Map("offMarketDate" -> "NULL"), 43)

        generalRules.map(rule => rule(p.fields)._1).filter(_ == false) should have size 0
        generalRules.map(rule => rule(q.fields)._1).filter(_ == false) should have size 1
        generalRules.map(rule => rule(r.fields)._1).filter(_ == false) should have size 1
        generalRules.map(rule => rule(s.fields)._1).filter(_ == false) should have size 0
    }

    test("Verifies that refundingRate must be in [1247NPT(NULL)]") {
        val p = QuotivalData("package", Map("refundingRate" -> "N"), 50)
        val q = QuotivalData("product", Map("refundingRate" -> "1"), 51)
        val r = QuotivalData("package", Map("refundingRate" -> "6"), 52)
        val s = QuotivalData("package", Map("refundingRate" -> "NULL"), 53)

        generalRules.map(rule => rule(p.fields)._1).filter(_ == false) should have size 0
        generalRules.map(rule => rule(q.fields)._1).filter(_ == false) should have size 0
        generalRules.map(rule => rule(r.fields)._1).filter(_ == false) should have size 1
        generalRules.map(rule => rule(s.fields)._1).filter(_ == false) should have size 0
    }

    /////////////
    // Product //
    /////////////

    test("Verifie qu'un product ne pas avoir les champs ...") {
        val p = QuotivalData("product", Map("refundingRate" -> "P"), 70)
        val q = QuotivalData("product", Map("publicPrice" -> "1.14"), 71)
        val r = QuotivalData("product", Map("marketStauts" -> "4", "offMarketDate" -> "2013-05-29 00:00:00"), 72)

        productRules.map(rule => rule(p.fields)._1).filter(_ == false) should have size 0
        productRules.map(rule => rule(q.fields)._1).filter(_ == false) should have size 1
        productRules.map(rule => rule(r.fields)._1).filter(_ == false) should have size 1
    }

    /////////////
    // Package //
    /////////////

    test("Verifie qu'un package ne peut pas avoir la valeur 'P' dans un package") {
        val p = QuotivalData("package", Map("refundingRate" -> "P"), 70)
        val q = QuotivalData("product", Map("refundingRate" -> "P"), 71)
        val r = QuotivalData("package", Map("refundingRate" -> "T"), 72)
        val s = QuotivalData("package", Map("refundingRate" -> "NULL"), 73)

        packageRules.map(rule => rule(p.fields)._1).filter(_ == false) should have size 1
        productRules.map(rule => rule(q.fields)._1).filter(_ == false) should have size 0
        packageRules.map(rule => rule(r.fields)._1).filter(_ == false) should have size 0
        packageRules.map(rule => rule(s.fields)._1).filter(_ == false) should have size 0
    }

    test("Verifie que si le refundingRate passe à 'N', le refundingBase passe à 'NULL'") {
        val p = QuotivalData("package", Map("refundingRate" -> "N"), 80)
        val q = QuotivalData("package", Map("refundingRate" -> "T"), 81)
        val r = QuotivalData("package", Map("refundingRate" -> "N", "refundingBase" -> "NULL"), 82)
        val s = QuotivalData("package", Map("refundingRate" -> "N", "refundingBase" -> "1"), 83)

        packageRules.map(rule => rule(p.fields)._1).filter(_ == false) should have size 1
        packageRules.map(rule => rule(q.fields)._1).filter(_ == false) should have size 0
        packageRules.map(rule => rule(r.fields)._1).filter(_ == false) should have size 0
        packageRules.map(rule => rule(s.fields)._1).filter(_ == false) should have size 1
    }

    test("Verifie qu'un package ne peut pas avoir un public price mal formaté") {
        val p = QuotivalData("package", Map("publicPrice" -> "1.60"), 90)
        val q = QuotivalData("package", Map("publicPrice" -> "1"), 91)
        val r = QuotivalData("package", Map("publicPrice" -> "ab"), 92)
        val s = QuotivalData("package", Map("publicPrice" -> "1,36"), 93)
        val t = QuotivalData("package", Map("publicPrice" -> "NULL"), 94)

        packageRules.map(rule => rule(p.fields)._1).filter(_ == false) should have size 0
        packageRules.map(rule => rule(q.fields)._1).filter(_ == false) should have size 0
        packageRules.map(rule => rule(r.fields)._1).filter(_ == false) should have size 1
        packageRules.map(rule => rule(s.fields)._1).filter(_ == false) should have size 1
        packageRules.map(rule => rule(t.fields)._1).filter(_ == false) should have size 0
    }

    test("Verifie qu'un package ne peut pas avoir un actCode mal formaté") {
        val p = QuotivalData("package", Map("actCode" -> "PH7", "actCodeName" -> "pharmacie 65%"), 100)
        val q = QuotivalData("package", Map("actCode" -> "PA7", "actCodeName" -> "pharmacie 65%"), 101)
        val r = QuotivalData("package", Map("actCode" -> "NULL", "actCodeName" -> "NULL"), 102)
        val s = QuotivalData("package", Map("actCode" -> "GLU", "actCodeName" -> "aliments sans gluten"), 103)
        val t = QuotivalData("package", Map("actCode" -> "ato", "actCodeName" -> "pharmacie 65%"), 104)

        packageRules.map(rule => rule(p.fields)._1).filter(_ == false) should have size 0
        packageRules.map(rule => rule(q.fields)._1).filter(_ == false) should have size 1
        packageRules.map(rule => rule(r.fields)._1).filter(_ == false) should have size 0
        packageRules.map(rule => rule(s.fields)._1).filter(_ == false) should have size 0
        packageRules.map(rule => rule(t.fields)._1).filter(_ == false) should have size 1
    }

    test("Verifie qu'on ne peut pas changer d'actCode sans changer d'actCodeName") {
        val p = QuotivalData("package", Map("actCode" -> "PH7"), 110)
        val q = QuotivalData("package", Map("actCode" -> "PH7", "actCodeName" -> "pharmacie 65%"), 111)
        val r = QuotivalData("package", Map("actCode" -> "PH7", "actCodeName" -> "pharmacie 100%"), 112)
        val s = QuotivalData("package", Map("actCode" -> "PA", "actCodeName" -> "orthèses"), 113)
        val t = QuotivalData("package", Map("actCode" -> "NULL", "actCodeName" -> "NULL"), 114)

        packageRules.map(rule => rule(p.fields)._1).filter(_ == false) should have size 1
        packageRules.map(rule => rule(q.fields)._1).filter(_ == false) should have size 0
        packageRules.map(rule => rule(r.fields)._1).filter(_ == false) should have size 1
        packageRules.map(rule => rule(s.fields)._1).filter(_ == false) should have size 0
        packageRules.map(rule => rule(t.fields)._1).filter(_ == false) should have size 0
    }

    test("Verifie qu'un package ne peut pas avoir un refundingBase mal formaté") {
        val p = QuotivalData("package", Map("refundingBase" -> "1.60"), 120)
        val q = QuotivalData("package", Map("refundingBase" -> "1"), 121)
        val r = QuotivalData("package", Map("refundingBase" -> "ab"), 122)
        val s = QuotivalData("package", Map("refundingBase" -> "1,36"), 123)
        val t = QuotivalData("package", Map("refundingBase" -> "NULL"), 124)

        packageRules.map(rule => rule(p.fields)._1).filter(_ == false) should have size 0
        packageRules.map(rule => rule(q.fields)._1).filter(_ == false) should have size 0
        packageRules.map(rule => rule(r.fields)._1).filter(_ == false) should have size 1
        packageRules.map(rule => rule(s.fields)._1).filter(_ == false) should have size 1
        packageRules.map(rule => rule(t.fields)._1).filter(_ == false) should have size 0
    }

    test("le vat doit avoir les bonnes valeurs") {
        val p = QuotivalData("package", Map("vatRate" -> "2.10"), 130)
        val q = QuotivalData("package", Map("vatRate" -> "1.45"), 131)
        val r = QuotivalData("package", Map("vatRate" -> "ab"), 132)
        val s = QuotivalData("package", Map("vatRate" -> "2,10"), 133)
        val t = QuotivalData("package", Map("vatRate" -> "NULL", "publicPrice" -> "NULL"), 134)

        packageRules.map(rule => rule(p.fields)._1).filter(_ == false) should have size 0
        packageRules.map(rule => rule(q.fields)._1).filter(_ == false) should have size 1
        packageRules.map(rule => rule(r.fields)._1).filter(_ == false) should have size 1
        packageRules.map(rule => rule(s.fields)._1).filter(_ == false) should have size 1
        packageRules.map(rule => rule(t.fields)._1).filter(_ == false) should have size 1
    }

    test("si le refundingBase et le vat changent, le vat ne peut pas être NULL") {
        val p = QuotivalData("package", Map("refundingBase" -> "1.60", "vatRate" -> "2.10"), 140)
        val q = QuotivalData("package", Map("refundingBase" -> "1.60", "vatRate" -> "5.50"), 141)
        val r = QuotivalData("package", Map("refundingBase" -> "1.60", "vatRate" -> "NULL", "publicPrice" -> "NULL"), 142)
        val s = QuotivalData("package", Map("refundingBase" -> "NULL", "vatRate" -> "NULL", "publicPrice" -> "NULL"), 142)

        packageRules.map(rule => rule(p.fields)._1).filter(_ == false) should have size 0
        packageRules.map(rule => rule(q.fields)._1).filter(_ == false) should have size 0
        packageRules.map(rule => rule(r.fields)._1).filter(_ == false) should have size 1
        packageRules.map(rule => rule(s.fields)._1).filter(_ == false) should have size 0
    }

    test("si le vatRate est modifé à 'NULL', le publicPrice doit l'être aussi") {
        val p = QuotivalData("package", Map("vatRate" -> "2.10"), 150)
        val q = QuotivalData("package", Map("publicPrice" -> "NULL", "vatRate" -> "NULL"), 141)
        val r = QuotivalData("package", Map("publicPrice" -> "1.60", "vatRate" -> "NULL"), 142)

        packageRules.map(rule => rule(p.fields)._1).filter(_ == false) should have size 0
        packageRules.map(rule => rule(q.fields)._1).filter(_ == false) should have size 0
        packageRules.map(rule => rule(r.fields)._1).filter(_ == false) should have size 1
    }
}
