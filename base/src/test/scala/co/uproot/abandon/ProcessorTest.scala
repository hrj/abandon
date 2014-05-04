package co.uproot.abandon

import org.scalatest.FlatSpec
import org.scalatest.matchers.Matcher
import org.scalatest.Matchers
import org.scalatest.Inside
import java.lang.Exception

import TestHelper._

class ProcessorTest extends FlatSpec with Matchers with Inside {

  "Processor" should "export a simple transaction in ledger Format" in {
    val testInput = """
    2013/1/1
      Expense       -(200 + 40)
      Cash
    """
    val parseResult = AbandonParser.abandon(scanner(testInput))
    inside(parseResult) {
      case AbandonParser.Success(result, _) =>
        val astEntries = result
        val appState = Processor.process(astEntries)
        val exports = Seq(LedgerExportSettings(None, Seq("balSheet12.txt"), false, Nil))

        val settings = Settings(Nil, Nil, Nil, ReportOptions(Nil), exports, None)

        exports.foreach { exportSettings =>
          exportSettings match {
            case balSettings: LedgerExportSettings =>
              val ledgerRep = Reports.ledgerExport(appState, settings, balSettings)
              inside(ledgerRep) {
                case Seq(LedgerExportData(date, txns)) =>
                  date should be(Date(2013, 1, 1))
                  inside(txns) {
                    case Seq(LedgerExportEntry(acc1, expr1), LedgerExportEntry(acc2, expr2)) =>
                      acc1 should be (cashAccount)
                      acc2 should be (expenseAccount)
                      expr1 should be (240)
                      expr2 should be (-240)
                  }
              }
          }
        }
    }
  }

  "Processor" should "not export a transaction with zero value when showZeroAmount is False" in {
    val testInput = """
    2013/1/1
      Expense       -(200 + 40)
      Cash
      Bank           0000
    """
    val parseResult = AbandonParser.abandon(scanner(testInput))
    inside(parseResult) {
      case AbandonParser.Success(result, _) =>
        val astEntries = result
        val appState = Processor.process(astEntries)
        val exports = Seq(LedgerExportSettings(None, Seq("balSheet12.txt"), false, Nil))

        val settings = Settings(Nil, Nil, Nil, ReportOptions(Nil), exports, None)

        exports.foreach { exportSettings =>
          exportSettings match {
            case balSettings: LedgerExportSettings =>
              val ledgerRep = Reports.ledgerExport(appState, settings, balSettings)
              inside(ledgerRep) {
                case Seq(LedgerExportData(date, txns)) =>
                  date should be(Date(2013, 1, 1))
                  inside(txns) {
                    case Seq(LedgerExportEntry(acc1, expr1), LedgerExportEntry(acc2, expr2)) =>
                      acc1 should be (cashAccount)
                      acc2 should be (expenseAccount)
                      expr1 should be (240)
                      expr2 should be (-240)
                  }
              }
          }
        }
    }
  }

  "Processor" should "export a transaction with zero value when showZeroAmount is True" in {
    val testInput = """
    2013/1/1
      Expense       -200
      Cash
      Bank:Current           0000
    """
    val parseResult = AbandonParser.abandon(scanner(testInput))
    inside(parseResult) {
      case AbandonParser.Success(result, _) =>
        val astEntries = result
        val appState = Processor.process(astEntries)
        val exports = Seq(LedgerExportSettings(None, Seq("balSheet12.txt"), true, Nil))

        val settings = Settings(Nil, Nil, Nil, ReportOptions(Nil), exports, None)

        exports.foreach { exportSettings =>
          exportSettings match {
            case balSettings: LedgerExportSettings =>
              val ledgerRep = Reports.ledgerExport(appState, settings, balSettings)
              inside(ledgerRep) {
                case Seq(LedgerExportData(date, txns)) =>
                  date should be(Date(2013, 1, 1))
                  inside(txns) {
                    case Seq(LedgerExportEntry(acc1, expr1), LedgerExportEntry(acc2, expr2), LedgerExportEntry(acc3, expr3)) =>
                      acc1 should be (bankAccount)
                      acc2 should be (cashAccount)
                      acc3 should be (expenseAccount)
                      expr1 should be (0)
                      expr2 should be (200)
                      expr3 should be (-200)
                  }
              }
          }
        }
    }
  }

  "Processor" should "export no transaction" in {
    val testInput = """
    """
    val parseResult = AbandonParser.abandon(scanner(testInput))
    inside(parseResult) {
      case AbandonParser.Success(result, _) =>
        val astEntries = result
        val appState = Processor.process(astEntries)
        val exports = Seq(LedgerExportSettings(None, Seq("balSheet12.txt"), false, Nil))

        val settings = Settings(Nil, Nil, Nil, ReportOptions(Nil), exports, None)

        exports.foreach { exportSettings =>
          exportSettings match {
            case balSettings: LedgerExportSettings =>
              val ledgerRep = Reports.ledgerExport(appState, settings, balSettings)
              ledgerRep should be (Nil)
          }
        }
    }
  }

  "Processor" should "export transaction with closing balance" in {
    val testInput = """
      2013/1/1
      Expense       4000
      Income           -1000
      Equity    10000
      Assets -13000
    """
    val parseResult = AbandonParser.abandon(scanner(testInput))
    inside(parseResult) {
      case AbandonParser.Success(result, _) =>
        val astEntries = result
        val appState = Processor.process(astEntries)
        val source = Seq("Income", "Expense")
        val destination = "Equity"
        val closure = Seq(ClosureExportSettings(source, destination))
        val exports = Seq(LedgerExportSettings(None, Seq("balSheet12.txt"), false, closure))

        val settings = Settings(Nil, Nil, Nil, ReportOptions(Nil), exports, None)

        exports.foreach { exportSettings =>
          exportSettings match {
            case balSettings: LedgerExportSettings =>
              val ledgerRep = Reports.ledgerExport(appState, settings, balSettings)
              inside(ledgerRep) {
                case Seq(LedgerExportData(date, txns), LedgerExportData(date1, txns1)) =>
                  date should be(Date(2013, 1, 1))
                  inside(txns) {
                    case Seq(LedgerExportEntry(acc1, expr1), LedgerExportEntry(acc2, expr2), LedgerExportEntry(acc3, expr3), LedgerExportEntry(acc4, expr4)) =>
                      acc1 should be (assetsAccount)
                      acc2 should be (equityAccount)
                      acc3 should be (expenseAccount)
                      acc4 should be (incomeAccount)
                      expr1 should be (-13000)
                      expr2 should be (10000)
                      expr3 should be (4000)
                      expr4 should be (-1000)
                  }
                  date1 should be(Date(2013, 1, 1))
                  inside(txns1) {
                    case Seq(LedgerExportEntry(acc1, expr1), LedgerExportEntry(acc2, expr2), LedgerExportEntry(acc3, expr3)) =>
                      acc1 should be (expenseAccount)
                      acc2 should be (incomeAccount)
                      acc3 should be (equityAccount)
                      expr1 should be (-4000)
                      expr2 should be (1000)
                      expr3 should be (3000)
                  }
              }
          }
        }

    }

  }

  "Processor" should "show Exception when 'source' contains same 'accountName'" in {
    val testInput = """
      2013/1/1
      Expense       4000
      Income           -1000
      Equity    10000
      Assets -13000
    """
    val parseResult = AbandonParser.abandon(scanner(testInput))
    inside(parseResult) {
      case AbandonParser.Success(result, _) =>
        val astEntries = result
        val appState = Processor.process(astEntries)
        val source = Seq("Income", "Expense")
        val destination = "Equity"
        val source1 = Seq("Income", "Expense")
        val destination1 = "Equity"
        val closure1 = Seq(ClosureExportSettings(source, destination))
        val closure2 = Seq(ClosureExportSettings(source1, destination1))
        val closure = closure1 ++ closure2
        val exports = Seq(LedgerExportSettings(None, Seq("balSheet12.txt"), false, closure))

        val settings = Settings(Nil, Nil, Nil, ReportOptions(Nil), exports, None)

        exports.foreach { exportSettings =>
          exportSettings match {
            case balSettings: LedgerExportSettings =>
              val ledgerRep = intercept[InputError] {
                Reports.ledgerExport(appState, settings, balSettings)
              }
          }
        }

    }

  }
  "Processor" should "show Exception when 'destination' is not Present" in {
    val testInput = """
      2013/1/1
      Expense       14000
      Income        -1000
      Assets       -13000
    """
    val parseResult = AbandonParser.abandon(scanner(testInput))
    inside(parseResult) {
      case AbandonParser.Success(result, _) =>
        val astEntries = result
        val appState = Processor.process(astEntries)
        val source = Seq("Income", "Expense")
        val destination = "Equity"
        val closure = Seq(ClosureExportSettings(source, destination))
        val exports = Seq(LedgerExportSettings(None, Seq("balSheet12.txt"), false, closure))

        val settings = Settings(Nil, Nil, Nil, ReportOptions(Nil), exports, None)

        exports.foreach { exportSettings =>
          exportSettings match {
            case balSettings: LedgerExportSettings =>
              val ledgerRep = intercept[MissingDestinationError] {
                Reports.ledgerExport(appState, settings, balSettings)
              }
          }
        }

    }

  }
  "Processor" should "show Exception when 'destination' is present in 'source'" in {
    val testInput = """
      2013/1/1
      Expense       14000
      Income        -1000
      Equity       -13000
    """
    val parseResult = AbandonParser.abandon(scanner(testInput))
    inside(parseResult) {
      case AbandonParser.Success(result, _) =>
        val astEntries = result
        val appState = Processor.process(astEntries)
        val source = Seq("Income", "Equity")
        val destination = "Equity"
        val closure = Seq(ClosureExportSettings(source, destination))
        val exports = Seq(LedgerExportSettings(None, Seq("balSheet12.txt"), false, closure))

        val settings = Settings(Nil, Nil, Nil, ReportOptions(Nil), exports, None)

        exports.foreach { exportSettings =>
          exportSettings match {
            case balSettings: LedgerExportSettings =>
              val ledgerRep = intercept[SourceDestinationClashError] {
                Reports.ledgerExport(appState, settings, balSettings)
              }
          }
        }

    }

  }
}
