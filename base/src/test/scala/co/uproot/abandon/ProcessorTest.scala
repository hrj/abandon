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

  "parser" should "parse Balance Export in ledger Format" in {
   /*val conf = """
     exports += {
        title = "Balance Sheet"
        type = ledger
        outFiles = [balSheet13.txt]
     }
     """ */
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
           var str = Seq[co.uproot.abandon.ASTEntry]()
           str :+= txnGroup
           val appState = Processor.process(str)
           val file = new java.io.File("examples/sim/accounts.conf")
           val confFile = file + ""
           val parseResult = AbandonParser.abandon(scanner(testInput))

           val config = ConfigFactory.parseFile(file).resolve()
           val inputs = config.getStringList("inputs").asScala.map(Processor.mkRelativeFileName(_, confFile))
           val reports = config.getConfigList("reports").asScala.map(makeReportSettings(_))
           val reportOptions = config.optConfig("reportOptions")
           val isRight = reportOptions.map(_.optStringList("isRight")).flatten.getOrElse(Nil)
           val exportConfigs = config.optConfigList("exports").getOrElse(Nil)
           val exports = exportConfigs.map(makeExportSettings)
           val eodConstraints = config.optConfigList("eodConstraints").getOrElse(Nil).map(makeEodConstraints(_))
           val settings = Settings(inputs, eodConstraints, reports, ReportOptions(isRight), exports, Some(file))

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