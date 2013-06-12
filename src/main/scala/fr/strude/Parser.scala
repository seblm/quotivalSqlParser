package fr.strude

object Parser {

    case class QuotivalData(tableName: String, fields: Map[String, String], primaryKey: Int)

    def extractQuotivalMetadata(filePath: String): List[QuotivalData] = {
        val lines = io.Source.fromFile(filePath, "UTF-8").getLines()
        lines.map {
            l =>
                val splitSet = l.split(" SET ")
                val splitWhere = splitSet(1).split(" WHERE ")
                QuotivalData(splitSet(0).split("UPDATE ")(1).trim, extractQuotivalFieldsData(splitWhere(0).trim), retrieveId(splitWhere(1).trim))
        }.toList
    }

    private def retrieveId(splitWhere: String): Int = {
        /*try {
          splitWhere(1).split("= ")(1).split(" ;")(0).toInt
        } catch {
          case e: java.lang.NumberFormatException => throw new NumberFormatException("le parsing de l'id sur le fichier est erroné")
        } */
        splitWhere.split("=")(1).split(";")(0).trim.toInt
    }

    private def extractQuotivalFieldsData(fields: String): Map[String, String] = {
        fields.split(",").map {
            param =>
                val keyValueArray = param.split("=")
                (keyValueArray(0).trim, removeQuotes(keyValueArray(1).trim))
        }.toMap
    }

    private def removeQuotes(value: String): String = {
        if (value.takeRight(1) == "'" && value.take(1) == "'") value.drop(1).take(value.length - 2)
        else value
    }
}
