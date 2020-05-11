import sbt._
import Keys._

object Dependencies {
  // Versions
  val scalatestVersion = "3.1.2"
  val scallopVersion = "3.4.0"
  val scalaMockVersion = "4.4.0"
  val configVersion = "1.4.0"
  val scalaXMLVersion = "1.3.0"
  val scalaParserCombinatorsVersion = "1.1.2"
  val scalaFXVersion = "12.0.1-R17"
  val dirSuiteVersion = "0.31.0"

  // Libraries
  val scalatest = "org.scalatest" %% "scalatest" % scalatestVersion
  val scalamock = "org.scalamock" %% "scalamock" % scalaMockVersion
  val scallop = "org.rogach" %% "scallop" % scallopVersion
  val config = "com.typesafe" % "config" % configVersion
  val scalaXML = "org.scala-lang.modules" %% "scala-xml" % scalaXMLVersion
  val scalaParserCombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % scalaParserCombinatorsVersion
  val scalaFX = "org.scalafx" %% "scalafx" % scalaFXVersion
  val dirSuite = "fi.e257.testing" %% "dirsuite" % dirSuiteVersion
}
