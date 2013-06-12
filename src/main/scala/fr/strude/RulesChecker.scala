package fr.strude

import Parser._
import Rules._

object RulesChecker {

    sealed trait RulesError
    case class Error(errors: Set[String], quotivalData: QuotivalData) extends RulesError
    case object NoError extends RulesError

    def validateDatas(quotivalDatas: Set[QuotivalData]): Set[Error] = {
        val generalFailures = quotivalDatas
                .map(data => checkRules(data, generalRules))
                .filter {
                    case e: Error => true
                    case _ => false
                }.asInstanceOf[Set[Error]]

        val packageFailures = retrieveErrors(quotivalDatas, "package", packageRules)
        val productFailures = retrieveErrors(quotivalDatas, "product", productRules)

        concatQuotivalErrors(generalFailures ++ packageFailures ++ productFailures)
    }

    private def checkRules(quotivalData: QuotivalData, rules: Rules): RulesError = {
        val quotiFailed = for {
            rule <- rules
            ruleRes = rule(quotivalData.fields)
            if ( !ruleRes._1 )
        } yield (ruleRes._2)

        if(quotiFailed.isEmpty) NoError else Error(quotiFailed, quotivalData)
    }

    private def retrieveErrors(quotivalDatas: Set[QuotivalData], tableName: String, rules: Rules): Set[Error] = {
        (for {
            q <- quotivalDatas
            if (q.tableName == tableName)

            rulesErrors = checkRules(q, rules)
            if (rulesErrors match {
                case e: Error => true
                case _ => false
            })
        } yield rulesErrors).asInstanceOf[Set[Error]]
    }

    private def concatQuotivalErrors(set: Set[Error]): Set[Error] = {
        if(set.isEmpty) Set()
        else {
            val mergedSet = concatQuotivalErrors(set.tail)
            val mayBeError = mergedSet.find( error => error.quotivalData == set.head.quotivalData )

            mayBeError match {
                case Some(error) => {
                    val mergedError = error.copy( errors = error.errors ++ set.head.errors )
                    (mergedSet - error) + mergedError
                }
                case _ => mergedSet + set.head
            }
        }
    }
}
