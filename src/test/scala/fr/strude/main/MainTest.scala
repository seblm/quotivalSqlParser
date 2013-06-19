package fr.strude.main

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import java.util.Calendar
import fr.strude.{RulesChecker, Parser}
import fr.strude.Parser.QuotivalData

class MainTest extends FunSuite with ShouldMatchers {

    val now = Calendar.getInstance().getTime
    val testName = s"parsing quotival datas - date : ${now}"

    test(testName) {
        val quotivaldatas: Set[QuotivalData] = Parser.extractQuotivaldatas("src/test/resources/real.sql")

        val result = RulesChecker.validateDatas(quotivaldatas)

        if(!result.isEmpty) fail(s"Il y a ${result.size} erreurs" )
        //result should be ('empty)

        //result.foreach( error => println(s"${error.quotivalData} : ${error.errors}"))
    }
}
