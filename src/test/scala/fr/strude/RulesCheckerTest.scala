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
        validateDatas(Set(badData)) should be (Set(Error(Set("Le champ offMarketDate ne respecte pas le format 'yyyy-mm-dd hh:mm:ss' (ou NULL)"), badData)))
    }

    test("validate datas, with general & package errors, should return only one error") {
        val fields = Map("marketStatus" -> "4", "refundingRate" -> "P")
        val quotivalDatas_withGeneralAndPackageErrors = QuotivalData("package", fields, 12)

        val result = validateDatas(Set(quotivalDatas_withGeneralAndPackageErrors))
        result should have size 1
        result.head.quotivalData should be(quotivalDatas_withGeneralAndPackageErrors)
        result.head.errors should have size 2
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