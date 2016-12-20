package co.uproot.abandon

import org.scalatest.FlatSpec
import org.scalatest.matchers.Matcher
import org.scalatest.Matchers

class AstTest extends FlatSpec with Matchers {

  "Date" should "convert to int" in {  
    val testDate = Date(2016, 1, 2)
    testDate.toInt should be(20160102)
  }

  "Date" should "convert to int with month resolution" in {
    val testDate = Date(2016, 1, 2)
    testDate.toIntYYYYMM should be(20160100)
  }

  "Date" should "convert to int with year resolution" in {
    val testDate = Date(2016, 1, 2)
    testDate.toIntYYYY should be(20160000)
  }

  it should "be possible to convert from int" in {
    val testDate = Date.fromInt(20160102)
    testDate.year should be(2016)
    testDate.month should be(1)
    testDate.day should be (2)
    testDate.hasMonthResolution should be (true)
    testDate.hasDayResolution should be (true)
  }

  it should "be possible to convert from int with month resolution" in {
    val testDate = Date.fromInt(20160100)
    testDate.year should be(2016)
    testDate.month should be(1)
    testDate.day should be (0)
    testDate.hasMonthResolution should be (true)
    testDate.hasDayResolution should be (false)
  }

  it should "be possible to convert from int with year resolution" in {
    val testDate = Date.fromInt(20160000)
    testDate.year should be(2016)
    testDate.month should be(0)
    testDate.day should be (0)
    testDate.hasDayResolution should be (false)
    testDate.hasMonthResolution should be (false)
  }

  it should "compare correctly" in {
    import scala.util.Sorting

    val dates = List(
        Date(2015, 6, 30),  
        Date(1999, 1, 1),  // it use all year's four digits 
        Date(2014, 1, 2),  // sort based on day
        Date(2014, 1, 3),  
        Date(2015, 2, 28), // sort based on month
        Date(2015, 1, 31), //   (e.g. 28 < 31, but (1,31) < (2, 28))
        Date(2013, 12, 1)) // big month number
        
    val datesArr = dates.toArray

    val refDates = List(
        19990101,
        20131201,
        20140102,
        20140103,
        20150131,
        20150228,
        20150630)

    dates.sortWith(DateOrdering.compare(_, _) < 0).zip(refDates).forall({
        case (date, refDate) => date.toInt == refDate
      }) should be(true)

    // verify that DateOrdering.compare semantic is same as Ordering.compare
    Sorting.quickSort(datesArr)(DateOrdering)

    (datesArr.toList).zip(refDates).forall({
        case (date, refDate) => date.toInt == refDate
      }) should be(true)
  }

  it should "format correctly" in {

    val dates = List(
        Date(2013, 6, 1),
        Date(2014, 1, 10),
        Date(2015, 11,12),
        Date(2015, 12,31))

    val refCompactDates = List(
        "2013,6,1",
        "2014,1,10",
        "2015,11,12",
        "2015,12,31")

    val refYYYYMMDD = List(
        "2013 / 6 / 1",
        "2014 / 1 / 10",
        "2015 / 11 / 12",
        "2015 / 12 / 31")

    val refISO8601ExtDates = List(
        "2013-06-01",
        "2014-01-10",
        "2015-11-12",
        "2015-12-31")

    val refCompactYYYYMMDD = List(
        "2013/6/1",
        "2014/1/10",
        "2015/11/12",
        "2015/12/31")

    val months = List(
      Date(2013, 1, 1),
      Date(2013, 2, 1),
      Date(2013, 3, 1),
      Date(2013, 4, 1),
      Date(2013, 5, 1),
      Date(2013, 6, 1),
      Date(2013, 7, 1),
      Date(2013, 8, 1),
      Date(2013, 9, 1),
      Date(2013, 10, 1),
      Date(2013, 11, 1),
      Date(2013, 12, 1)
    )

    val refMonths = List(
        "2013 jan 1",
        "2013 feb 1",
        "2013 mar 1",
        "2013 apr 1",
        "2013 may 1",
        "2013 jun 1",
        "2013 jul 1",
        "2013 aug 1",
        "2013 sep 1",
        "2013 oct 1",
        "2013 nov 1",
        "2013 dec 1")

    dates.zip(refCompactDates).forall({
        case (date, refDate) => date.formatCompact == refDate
      }) should be(true)

    dates.zip(refCompactYYYYMMDD).forall({
        case (date, refDate) => date.formatCompactYYYYMMDD == refDate
      }) should be(true)

    dates.zip(refISO8601ExtDates).forall({
        case (date, refDate) => date.formatISO8601Ext == refDate
      }) should be(true)

    dates.zip(refYYYYMMDD).forall({
        case (date, refDate) => date.formatYYYYMMDD == refDate
      }) should be(true)

    months.zip(refMonths).forall({
        case (date, refDate) => date.formatYYYYMMMDD == refDate
      }) should be(true)
  }

  it should "format ISO 8601 weeks correctly" in {
    val dates = List(
      (Date(2005, 1, 2),   "2004-W53", "2004-W53-7"),
      (Date(2005, 12, 31), "2005-W52", "2005-W52-6"),
      (Date(2007, 1, 1),   "2007-W01", "2007-W01-1"),
      (Date(2008, 1, 1),   "2008-W01", "2008-W01-2"),
      (Date(2008, 12, 29), "2009-W01", "2009-W01-1"),
      (Date(2008, 12, 31), "2009-W01", "2009-W01-3"),
      (Date(2010, 1, 1),   "2009-W53", "2009-W53-5"),
      (Date(2010, 1, 3),   "2009-W53", "2009-W53-7"),
      (Date(2010, 1, 4),   "2010-W01", "2010-W01-1"))

    dates.forall({
      case (date, refWeek, refWeekDay) => {
        date.formatISO8601Week == refWeek &&
        date.formatISO8601WeekDate == refWeekDay
      }
    }) should be(true)
  }

}
