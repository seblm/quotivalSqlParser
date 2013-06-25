package fr.strude

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import Parser._

class ParserTest extends FunSuite with ShouldMatchers {

  val updateFile = "src/test/resources/update.sql"

  test("extractQuotivalMetadata - should return PRODUCT table name") {
    val result = extractQuotivaldatas(updateFile).head
    result.tableName should be ("Package")
  }

  test("extractQuotivalMetadata - should return tuple of product & fields") {
    val result = extractQuotivaldatas(updateFile).head
    result.tableName should be ("Package")
    result.fields should be (Map("offMarketDate" -> "2013-05-29 00:00:00", "marketStatus" -> "4"))
  }

  test("extractQuotivalMetadata - should return tuple of products, fields and id") {
    extractQuotivaldatas(updateFile).head should be (QuotivalData("Package", Map("offMarketDate" -> "2013-05-29 00:00:00", "marketStatus" -> "4"), 172211))
  }

  test("extractQuotivalMetadata - should return a List of 2 tuples containing each line's data") {
    extractQuotivaldatas(updateFile).take(2) should be (Set(QuotivalData("Package", Map("offMarketDate" -> "2013-05-29 00:00:00", "marketStatus" -> "4"), 172211), QuotivalData("Package", Map("offMarketDate" -> "2013-05-29 00:00:00", "marketStatus" -> "4"), 172212)))
  }

}
