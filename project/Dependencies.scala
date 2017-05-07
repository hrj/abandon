import sbt._
import Keys._

object Dependencies {
  // Versions
  val scalatestVersion = "3.0.3"
  val scallopVersion = "2.1.1"
  val scalaMockVersion = "3.5.0"
  val configVersion = "1.3.1"
  val scalaXMLVersion = "1.0.6"
  val scalaParserCombinatorsVersion = "1.0.6"
  val scalaFXVersion = "8.0.102-R11"
  val dirSuiteVersion = "0.6.0"

  // Libraries
  val scalatest = "org.scalatest" %% "scalatest" % scalatestVersion
  val scalamock = "org.scalamock" %% "scalamock-scalatest-support" % scalaMockVersion
  val scallop = "org.rogach" %% "scallop" % scallopVersion
  val config = "com.typesafe" % "config" % configVersion
  val scalaXML = "org.scala-lang.modules" %% "scala-xml" % scalaXMLVersion
  val scalaParserCombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % scalaParserCombinatorsVersion
  val scalaFX = "org.scalafx" %% "scalafx" % scalaFXVersion
  val dirSuite =  "fi.sn127" %% "utils-testing" % dirSuiteVersion
}
