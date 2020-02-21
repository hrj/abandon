package co.uproot.abandon

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GroupByTest extends AnyFlatSpec with Matchers {

  behavior of "GroupBy"
  it should "handle unknown groupBy" in {
    assertThrows [SettingsError] {
      GroupBy(Option("asdf"))
    }
  }
  it should "handle default" in {
    GroupBy(None) shouldBe an[GroupByMonth]
  }
  it should "handle year" in {
    GroupBy(Option("year")) shouldBe an[GroupByYear]
  }
  it should "handle month" in {
    GroupBy(Option("month")) shouldBe an[GroupByMonth]
  }
  it should "handle day" in {
    GroupBy(Option("day")) shouldBe an[GroupByDay]
  }
  it should "handle week" in {
    GroupBy(Option("isoWeek")) shouldBe an[GroupByIsoWeek]
  }
  it should "handle week date" in {
    GroupBy(Option("isoWeekDate")) shouldBe an[GroupByIsoWeekDate]
  }


  behavior of "groupByOp"
  it should "select toIntYYYY for year" in {
    val regCfg = RegisterReportSettings("", None, Nil, GroupBy(Option("year")))
    val date = Date(2016, 12, 21)
    val postGroup = new PostGroup(Nil, null, date, None, None, Nil)

    val str = regCfg.groupOp match {
      case Left(groupBy) => groupBy(postGroup)
      case _ => fail("wrong way")
    }
    str should be(date.toIntYYYY)
  }
  it should "select toIntYYYYMM for month" in {
    val regCfg = RegisterReportSettings("", None, Nil, GroupBy(Option("month")))
    val date = Date(2016, 12, 21)
    val postGroup = new PostGroup(Nil, null, date, None, None, Nil)

    val str = regCfg.groupOp match {
      case Left(groupBy) => groupBy(postGroup)
      case _ => fail("wrong way")
    }
    str should be(date.toIntYYYYMM)
  }
  it should "select toInt for day" in {
    val regCfg = RegisterReportSettings("", None, Nil, GroupBy(Option("day")))
    val date = Date(2016, 12, 21)
    val postGroup = new PostGroup(Nil, null, date, None, None, Nil)

    val str = regCfg.groupOp match {
      case Left(groupBy) => groupBy(postGroup)
      case _ => fail("wrong way")
    }
    str should be(date.toInt)
  }
  it should "select formatISO8601Week for iso week" in {
    val regCfg = RegisterReportSettings("", None, Nil, GroupBy(Option("isoWeek")))
    val date = Date(2016, 12, 21)
    val postGroup = new PostGroup(Nil, null, date, None, None, Nil)

    val str = regCfg.groupOp match {
      case Right(groupBy) => groupBy(postGroup)
      case _ => fail("wrong way")
    }
    str should be(date.formatISO8601Week)
  }
  it should "select formatISO8601WeekDate for iso week date" in {
    val regCfg = RegisterReportSettings("", None, Nil, GroupBy(Option("isoWeekDate")))
    val date = Date(2016, 12, 21)
    val postGroup = new PostGroup(Nil, null, date, None, None, Nil)

    val str = regCfg.groupOp match {
      case Right(groupBy) => groupBy(postGroup)
      case _ => fail("wrong way")
    }
    str should be(date.formatISO8601WeekDate)
  }
}
