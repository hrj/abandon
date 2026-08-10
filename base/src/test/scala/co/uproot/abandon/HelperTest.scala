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
}
