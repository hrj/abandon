import sbt._
import Keys._

object Dependencies {
  // Versions
  val scalatestVersion = "3.2.16"
  val scallopVersion = "4.1.0"
  val configVersion = "1.4.2"
  val scalaXMLVersion = "2.1.0"
  val scalaParserCombinatorsVersion = "2.3.0"
  val scalaFXVersion = "19.0.0-R30"
  val dirSuiteVersion = "0.31.0"

  // Libraries
  val scalatest = "org.scalatest" %% "scalatest" % scalatestVersion
  val scallop = "org.rogach" %% "scallop" % scallopVersion
  val config = "com.typesafe" % "config" % configVersion
  val scalaXML = "org.scala-lang.modules" %% "scala-xml" % scalaXMLVersion
  val scalaParserCombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % scalaParserCombinatorsVersion
  val scalaFX = "org.scalafx" %% "scalafx" % scalaFXVersion
  // val dirSuite = ("fi.e257.testing" %% "dirsuite" % dirSuiteVersion)
}
