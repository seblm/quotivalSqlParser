package fr.strude.main

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import java.util.Calendar
import fr.strude.{RulesChecker, Parser}
import fr.strude.Parser.QuotivalData

class Main extends FunSuite with ShouldMatchers {

    val now = Calendar.getInstance().getTime
    val testName = s"parsing quotival datas - date : ${now}"

    test(testName) {
        val quotivaldatas: Set[QuotivalData] = Parser.extractQuotivaldatas("src/test/resources/real.sql")

        val result = RulesChecker.validateDatas(quotivaldatas)

        result should have size 0

        result.foreach( error => println(s"${error.quotivalData} : ${error.errors}"))
    }
}
