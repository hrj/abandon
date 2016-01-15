package co.uproot.abandon

import org.scalatest.FlatSpec
import org.scalatest.matchers.Matcher
import org.scalatest.Matchers
import org.scalatest.Inside
import java.lang.Exception

import org.scalatest.StreamlinedXmlEquality._

import TestHelper._
import ParserHelper._
		
class ComplexProcessTest extends FlatSpec with Matchers with Inside {
	"Abandon" should "handle simple xml test case without configuration" in {

	  val (parseError, astEntries, processedFiles) = Processor.parseAll(Seq("testCases/small.ledger"))
	  assert(!parseError)

	  val xmlSettings = XmlExportSettings(None, Seq("balSheet12.txt"))
	  val settings = Settings(Nil, Nil, Nil, Nil, ReportOptions(Nil), Seq(xmlSettings), None)

	  val appState = Processor.process(astEntries,settings.accounts)
	  //TODO: Processor.checkConstaints(appState, settings.eodConstraints)

	  val xmlBalance = Reports.xmlBalanceExport(appState, xmlSettings)
	  val xmlJournal = Reports.xmlTxnExport(appState, xmlSettings)

	  val refXMLBalance = scala.xml.XML.loadFile("testCases/refSmallBalance.xml")
	  val refXMLJournal = scala.xml.XML.loadFile("testCases/refSmallJournal.xml")
	  
	  //val prettyPrinter = new scala.xml.PrettyPrinter(1024,2)
	  //println(prettyPrinter.format(xmlData))
	  //println(prettyPrinter.format(refXML))
	  
	  assert(xmlBalance === refXMLBalance)
	  assert(xmlJournal === refXMLJournal)
	}
}
