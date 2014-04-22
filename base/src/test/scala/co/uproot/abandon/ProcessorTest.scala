package co.uproot.abandon

import org.scalatest.FlatSpec
import org.scalatest.matchers.Matcher
import org.scalatest.Matchers
import scala.util.parsing.input.PagedSeqReader
import scala.collection.immutable.PagedSeq
import org.scalatest.Inside
import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import collection.JavaConverters._
import SettingsHelper._

class ProcessorTest extends FlatSpec with Matchers with Inside {

  private def reader(s: String) = new PagedSeqReader(PagedSeq.fromStrings(collection.immutable.Seq(s)))
  private def mkScanner(r: PagedSeqReader) = new AbandonParser.lexical.Scanner(r)
  private def scanner(s: String) = mkScanner(reader(s))

  private val expenseAccount = AccountName(Seq("Expense"))
  private val cashAccount = AccountName(Seq("Cash"))
  private val bankAccount = AccountName(Seq("Bank", "Current"))

  private def nlit(n: BigDecimal) = NumericLiteralExpr(n)

  "Processor" should "parse Balance Export in ledger Format" in {
    val testInput = """
    2013/1/1
      Expense       -(200 + 40)
      Cash
    """
    val parseResult = AbandonParser.abandon(scanner(testInput))
    inside(parseResult) {
      case AbandonParser.Success(result, _) =>
        inside(result) {
        case List(txnGroup) =>
           val astEntries = Seq[co.uproot.abandon.ASTEntry](txnGroup)
           val appState = Processor.process(astEntries)

           val eodConstraints = Seq[co.uproot.abandon.Constraint]()
           val reports = Seq[co.uproot.abandon.ReportSettings]()
           val reportOptions = co.uproot.abandon.ReportOptions
           val exports = Seq[co.uproot.abandon.ExportSettings](LedgerExportSettings(None,Seq("balSheet12.txt")))
           val file = new java.io.File("")


           val settings = Settings(Seq(""), eodConstraints, reports, ReportOptions(Seq("")), exports, Some(file))

           exports.map { exportSettings =>
              exportSettings match {
                case balSettings: LedgerExportSettings =>
                  val ledgerRep = Reports.ledgerExport(appState,settings,balSettings)
                  inside(ledgerRep) {
                    case List(LedgerExportData(date,txns)) =>
                      date should be(Date(2013, 1, 1))
                      inside(txns) 
                      {
                        case List(LedgerExportEntry(acc1, expr1), LedgerExportEntry(acc2, expr2)) =>
                        acc1 should be (cashAccount)
                        acc2 should be (expenseAccount)
                        expr1 should be (240)
                        expr2 should be (-240)
                      }
                  }
              case _ => ""
              }
           }
        }
    }
  }


 }