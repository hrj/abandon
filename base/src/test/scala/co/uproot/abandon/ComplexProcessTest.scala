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
	  val xmlData = Reports.xmlExport(appState, xmlSettings)

	  val prettyPrinter = new scala.xml.PrettyPrinter(1024,2)
	  val refXML = scala.xml.XML.loadFile("testCases/refSmall.xml")
	  
	  //println(prettyPrinter.format(xmlData))
	  //println(prettyPrinter.format(refXML))
	  
	  assert(xmlData === refXML)
	}
}
