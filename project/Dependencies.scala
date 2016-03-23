import sbt._
import Keys._

object Dependencies {
  // Versions
  val scalatestVersion = "2.2.6"
  val scallopVersion = "1.0.0"
  val configVersion = "1.3.0"
  val scalaXMLVersion = "1.0.5"
  val scalaParserCombinatorsVersion = "1.0.4"
  val scalaFXVersion = "8.0.60-R9"

  // Libraries
  val scalatest = "org.scalatest" %% "scalatest" % scalatestVersion
  val scallop = "org.rogach" %% "scallop" % scallopVersion
  val config = "com.typesafe" % "config" % configVersion
  val scalaXML = "org.scala-lang.modules" %% "scala-xml" % scalaXMLVersion
  val scalaParserCombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % scalaParserCombinatorsVersion
  val scalaFX = "org.scalafx" %% "scalafx" % scalaFXVersion
}
