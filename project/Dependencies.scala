import sbt._
import Keys._

object Dependencies {
  // Versions
  val scalatestVersion = "2.2.6"
  val scallopVersion = "2.0.2"
  val scalaMockVersion = "3.2.2"
  val configVersion = "1.3.0"
  val scalaXMLVersion = "1.0.6"
  val scalaParserCombinatorsVersion = "1.0.4"
  val scalaFXVersion = "8.0.102-R11"

  // Libraries
  val scalatest = "org.scalatest" %% "scalatest" % scalatestVersion
  val scalamock = "org.scalamock" %% "scalamock-scalatest-support" % scalaMockVersion
  val scallop = "org.rogach" %% "scallop" % scallopVersion
  val config = "com.typesafe" % "config" % configVersion
  val scalaXML = "org.scala-lang.modules" %% "scala-xml" % scalaXMLVersion
  val scalaParserCombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % scalaParserCombinatorsVersion
  val scalaFX = "org.scalafx" %% "scalafx" % scalaFXVersion
}
