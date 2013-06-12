package fr.strude

import Parser._
import Rules._

object RulesChecker {

    sealed trait RulesError
    case class Errors(errors: Set[String], quotivalData: QuotivalData) extends RulesError
    case object NoErrors extends RulesError

    def validateDatas(quotivalDatas: Set[QuotivalData]): Set[RulesError] = {
        val generalFailures = quotivalDatas
                .map(data => checkRules(data, generalRules))
                .filter {
                    case e: Errors => true
                    case _ => false
                }

        val packageFailures = retrieveErrors(quotivalDatas, "package", packageRules)
        val productFailures = retrieveErrors(quotivalDatas, "product", productRules)

        generalFailures ++ packageFailures ++ productFailures
    }

    private def checkRules(quotivalData: QuotivalData, rules: Rules): RulesError = {
        val quotiFailed = for {
            rule <- rules
            ruleRes = rule(quotivalData.fields)
            if ( !ruleRes._1 )
        } yield (ruleRes._2)

        if(quotiFailed.isEmpty) NoErrors else Errors(quotiFailed, quotivalData)
    }

    private def retrieveErrors(quotivalDatas: Set[QuotivalData], tableName: String, rules: Rules): Set[RulesError] = {
        for {
            q <- quotivalDatas
            if (q.tableName == tableName)

            rulesErrors = checkRules(q, rules)
            if (rulesErrors match {
                case e: Errors => true
                case _ => false
            })
        } yield rulesErrors
    }
}
