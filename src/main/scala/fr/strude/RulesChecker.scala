package fr.strude

import Parser._
import Rules._

object RulesChecker {

    sealed trait RulesError
    case class Error(errors: Set[String], quotivalData: QuotivalData) extends RulesError
    case object NoError extends RulesError

    def validateDatas(quotivalDatas: Set[QuotivalData],
                      rules: Rules = Rules.globalRules): Set[Error] = {
        for {
            q <- quotivalDatas
            messages = for {
              rule <- rules

              result = rule.validate(q)
              (isOk, message) = result
              if (!isOk)
            } yield message

            if(!messages.isEmpty)
        } yield Error(messages, q)
    }
}
