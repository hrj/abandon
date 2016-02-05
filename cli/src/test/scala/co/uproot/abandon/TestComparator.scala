package co.uproot.abandon

import org.scalatest.FlatSpec

import java.io.File

object TestComparator extends FlatSpec  {
  
  def txtComparator(first: String, second: String) :Boolean = {
    val srcFirst = scala.io.Source.fromFile(first)
        val txtFirst = try srcFirst.getLines mkString "\n" finally srcFirst.close()

        val srcSecond = scala.io.Source.fromFile(second)
        val txtSecond = try srcSecond.getLines mkString "\n" finally srcSecond.close()

        txtFirst == txtSecond
  }

  def xmlComparator(first: String, second: String) :Boolean = {
      import org.scalatest.StreamlinedXmlEquality._

      //println("xml: " + first + " " + second)

      val xmlFirst = scala.xml.XML.loadFile(first)
      val xmlSecond = scala.xml.XML.loadFile(second)

      xmlFirst === xmlSecond
  }
}

case class TestVec(output: String, reference: String, comparator: (String, String) => Boolean)

case class TestCase(conf: String, args: Array[String], testVec: List[TestVec])
	  
