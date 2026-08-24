package co.uproot.abandon

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HelperTest extends AnyFlatSpec with Matchers {
  "Helper" should "return 0 for maxElseZero on an empty collection" in {
    Helper.maxElseZero(Seq.empty[Int]) should be(0)
  }

  it should "return the single element for maxElseZero on a singleton collection" in {
    Helper.maxElseZero(Seq(42)) should be(42)
    Helper.maxElseZero(Seq(-5)) should be(-5)
    Helper.maxElseZero(Seq(0)) should be(0)
  }

  it should "return the maximum element for maxElseZero on a collection with multiple positive integers" in {
    Helper.maxElseZero(Seq(1, 5, 3, 9, 2)) should be(9)
  }

  it should "return the maximum element for maxElseZero on a collection with multiple negative integers" in {
    Helper.maxElseZero(Seq(-10, -5, -3, -9, -2)) should be(-2)
  }

  it should "return the maximum element for maxElseZero on a collection with mixed integers" in {
    Helper.maxElseZero(Seq(-10, 5, 0, -9, 2)) should be(5)
  }

  it should "return the maximum element for maxElseZero on a collection with duplicate maximums" in {
    Helper.maxElseZero(Seq(3, 7, 7, 2, 5)) should be(7)
  }

  "allUnique" should "return None for empty collections" in {
    Helper.allUnique(Seq.empty[Int]) shouldBe None
    Helper.allUnique(List.empty[String]) shouldBe None
  }

  it should "return None for collections with all unique elements" in {
    Helper.allUnique(Seq(1, 2, 3, 4, 5)) shouldBe None
    Helper.allUnique(List("a", "b", "c")) shouldBe None
    Helper.allUnique(Set(1, 2, 3)) shouldBe None
    Helper.allUnique(Iterator(1, 2, 3, 4)) shouldBe None
  }

  it should "return Some(element) for collections with duplicate elements" in {
    Helper.allUnique(Seq(1, 2, 3, 2, 4)) shouldBe Some(2)
    Helper.allUnique(List("a", "b", "c", "a")) shouldBe Some("a")
    Helper.allUnique(Iterator(1, 2, 1, 3)) shouldBe Some(1)
  }

  it should "return the first non-unique element" in {
    Helper.allUnique(Seq(1, 2, 3, 2, 4, 3)) shouldBe Some(2)
    Helper.allUnique(List("a", "b", "c", "b", "a")) shouldBe Some("b")
  }
  
  "Helper" should "get month numbers for valid full month names" in {
    Helper.getMonthNumber("January") should be(Some(1))
    Helper.getMonthNumber("February") should be(Some(2))
    Helper.getMonthNumber("March") should be(Some(3))
    Helper.getMonthNumber("April") should be(Some(4))
    Helper.getMonthNumber("May") should be(Some(5))
    Helper.getMonthNumber("June") should be(Some(6))
    Helper.getMonthNumber("July") should be(Some(7))
    Helper.getMonthNumber("August") should be(Some(8))
    Helper.getMonthNumber("September") should be(Some(9))
    Helper.getMonthNumber("October") should be(Some(10))
    Helper.getMonthNumber("November") should be(Some(11))
    Helper.getMonthNumber("December") should be(Some(12))
  }

  it should "get month numbers for valid short month names" in {
    Helper.getMonthNumber("Jan") should be(Some(1))
    Helper.getMonthNumber("Feb") should be(Some(2))
    Helper.getMonthNumber("Mar") should be(Some(3))
    Helper.getMonthNumber("Apr") should be(Some(4))
    Helper.getMonthNumber("May") should be(Some(5))
    Helper.getMonthNumber("Jun") should be(Some(6))
    Helper.getMonthNumber("Jul") should be(Some(7))
    Helper.getMonthNumber("Aug") should be(Some(8))
    Helper.getMonthNumber("Sep") should be(Some(9))
    Helper.getMonthNumber("Oct") should be(Some(10))
    Helper.getMonthNumber("Nov") should be(Some(11))
    Helper.getMonthNumber("Dec") should be(Some(12))
  }

  it should "handle case insensitivity correctly" in {
    Helper.getMonthNumber("JANUARY") should be(Some(1))
    Helper.getMonthNumber("jan") should be(Some(1))
    Helper.getMonthNumber("mArCh") should be(Some(3))
    Helper.getMonthNumber("aUg") should be(Some(8))
  }

  it should "return None for invalid month names" in {
    Helper.getMonthNumber("foo") should be(None)
    Helper.getMonthNumber("Janu") should be(None)
    Helper.getMonthNumber("") should be(None)
  }

}
