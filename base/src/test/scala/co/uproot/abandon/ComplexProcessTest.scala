package co.uproot.abandon

import org.scalatest.matchers.Matcher
import org.scalatest.Inside
import java.lang.Exception

import org.scalatest.StreamlinedXmlEquality._

import TestHelper._
import ParserHelper._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ComplexProcessTest extends AnyFlatSpec with Matchers with Inside {
	"Abandon" should "handle simple xml test case without configuration" in {

		val quiet = true
		val (parseError, scope, processedFiles) = Processor.parseAll(Seq("tests/small.ledger"), quiet)
	  assert(!parseError)

	  val xmlBalSettings = BalanceExportSettings(XMLType, None, Seq("not-used.xml"), None, true, Nil)
	  val xmlJournalSettings = JournalExportSettings(XMLType, None, Seq("not-used.xml"), None)
	  val settings = Settings(Nil, Nil, Nil, Nil, ReportOptions(Nil), Seq(xmlBalSettings), None, quiet, None, None)

	  val appState = Processor.process(scope,settings.accounts, None)
	  //TODO: Processor.checkConstaints(appState, settings.eodConstraints)

	  val xmlBalance = Reports.xmlExport(appState, xmlBalSettings, settings.txnFilters)
	  val xmlJournal = Reports.xmlExport(appState, xmlJournalSettings, settings.txnFilters)

	  val refXMLBalance = scala.xml.XML.loadFile("tests/refSmallBalance.xml")
	  val refXMLJournal = scala.xml.XML.loadFile("tests/refSmallJournal.xml")

	  //val prettyPrinter = new scala.xml.PrettyPrinter(1024,2)
	  //println(prettyPrinter.format(xmlJournal))
	  //println(prettyPrinter.format(refXMLJournal))

	  assert(xmlBalance === refXMLBalance)
	  assert(xmlJournal === refXMLJournal)
	}
}
