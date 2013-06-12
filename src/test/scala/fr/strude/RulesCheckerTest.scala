package fr.strude

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import RulesChecker._
import Parser._

class RulesCheckerTest extends FunSuite with ShouldMatchers {

    val goodData = QuotivalData("package", Map("offMarketDate" -> "2013-05-29 00:00:00"), 40)
    val badData = QuotivalData("product", Map("offMarketDate" -> "2013-ab-29 00:00:00"), 41)

    test("validate datas should not return errors") {
        validateDatas(Set(goodData)) should be(Set())
    }

    test("validate datas should return some errors") {
        validateDatas(Set(badData)) should be (Set(Errors(Set("Le champ offMarketDate ne respecte pas le format 'yyyy-mm-dd hh:mm:ss' (ou NULL)"), badData)))
    }

    test("[test after] more complete datas validation") {
        val p = QuotivalData("package", Map("marketStatus" -> "5"), 20)
        val q = QuotivalData("package", Map("marketStatus" -> "4"), 21)
        val r = QuotivalData("product", Map("marketStatus" -> "1"), 22)
        val s = QuotivalData("product", Map("marketStatus" -> "4", "offMarketDate" -> "2013-05-29 00:00:00"), 23)

        val result = validateDatas(Set(p, q, r, s))
        result should have size 2
    }
}