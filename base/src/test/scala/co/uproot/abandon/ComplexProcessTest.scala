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

		val quiet = true
		val (parseError, scope, processedFiles) = Processor.parseAll(Seq("testCases/small.ledger"), quiet)
	  assert(!parseError)

	  val xmlBalSettings = XmlExportSettings(BalanceType, None, Seq("not-used.xml"), true)
	  val xmlTxnSettings = XmlExportSettings(JournalType, None, Seq("not-used.xml"), true)
	  val settings = Settings(Nil, Nil, Nil, Nil, ReportOptions(Nil), Seq(xmlBalSettings), None, quiet, None)

	  val appState = Processor.process(scope,settings.accounts, None)
	  //TODO: Processor.checkConstaints(appState, settings.eodConstraints)

	  val xmlBalance = Reports.xmlExport(appState, xmlBalSettings, settings.txnFilters)
	  val xmlJournal = Reports.xmlExport(appState, xmlTxnSettings, settings.txnFilters)

	  val refXMLBalance = scala.xml.XML.loadFile("testCases/refSmallBalance.xml")
	  val refXMLJournal = scala.xml.XML.loadFile("testCases/refSmallJournal.xml")
	  
	  //val prettyPrinter = new scala.xml.PrettyPrinter(1024,2)
	  //println(prettyPrinter.format(xmlJournal))
	  //println(prettyPrinter.format(refXMLJournal))
	  
	  assert(xmlBalance === refXMLBalance)
	  assert(xmlJournal === refXMLJournal)
	}
}
