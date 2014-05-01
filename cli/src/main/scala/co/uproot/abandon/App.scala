package co.uproot.abandon

import org.rogach.scallop.{ ScallopConf, stringListConverter }
import Helper.{ Zero, filterByType, maxElseZero }
import java.io.FileWriter

final class ReportWriter(settings: Settings, outFiles: Seq[String]) {
  val writesToScreen = outFiles.contains("-") || outFiles.isEmpty
  val filePaths = outFiles.filterNot(_ equals "-").map(settings.getConfigRelativePath(_))
  val fileWriters = filePaths.map(path => new FileWriter(path))
  def startCodeBlock() = {
    fileWriters foreach { fileWriter =>
      fileWriter.write("```\n")
    }
  }

  def endCodeBlock() = {
    fileWriters foreach { fileWriter =>
      fileWriter.write("```\n")
    }
  }

  def printHeading(head: String) = {
    fileWriters foreach { fileWriter =>
      fileWriter.write(head + "\n\n")
    }

    if (writesToScreen) {
      println(head + "\n" + ("─" * head.length) + "\n")
    }
  }

  def println(s: String*) = {
    fileWriters foreach { fileWriter =>
      s.foreach(str => fileWriter.write(str))
      fileWriter.write('\n')
    }

    if (writesToScreen) {
      s.foreach { print }
      Console.println()
    }
  }

  def printXml(x: xml.Node) = {
    val sb = new StringBuilder
    val pp = new xml.PrettyPrinter(120, 2)
    pp.format(x, sb)
    val res = sb.toArray

    fileWriters foreach { fileWriter =>
      fileWriter.write(res)
    }

    if (writesToScreen) {
      println(sb.toString)
    }
  }

  def close = {
    fileWriters foreach { fileWriter =>
      fileWriter.close
    }
    Console.flush
  }
}

object AbandonApp extends App {
  def printRegReport(reportWriter: ReportWriter, regReport: Seq[RegisterReportGroup]) = {
    regReport foreach { reportGroup =>
      reportWriter.println(reportGroup.groupTitle)
      reportGroup.entries foreach { e =>
        reportWriter.println("   " + e.render)
      }
    }
  }

  def exportAsLedger(reportWriter: ReportWriter, ledgerRep: Seq[LedgerExportData]) = {
    ledgerRep foreach { reportGroup =>
      reportWriter.println(reportGroup.date.formatYYYYMMDD)
      val formatStr = "%-" + (reportGroup.maxNameLength + 4) + "s %" + (reportGroup.maxAmountWidth + 2) + ".2f"
      reportGroup.ledgerEntries foreach { e =>
        val render = formatStr format (e.accountName, e.amount)
        reportWriter.println("   " + render)
      }
    }
  }
  def printBookReport(reportWriter: ReportWriter, bookReportSettings: BookReportSettings, bookReport: Seq[RegisterReportGroup]) = {
    val txnIndent = " " * 49

    reportWriter.println("Account Name: " + bookReportSettings.account + "\n")

    val maxNameLength = maxElseZero(bookReport.flatMap(_.entries.flatMap(_.txns.flatMap(_.parentOpt.get.children.map(_.name.fullPathStr.length)))))
    reportWriter.startCodeBlock()

    bookReport foreach { reportGroup =>
      reportWriter.println(reportGroup.groupTitle)
      reportGroup.entries foreach { e =>
        e.txns foreach { txn =>
          val parent = txn.parentOpt.get
          reportWriter.println(("%20.2f %20.2f        %s") format (txn.resultAmount, txn.delta, parent.dateLineStr))
          // println(txnIndent + parent.dateLineStr)
          val otherTxns = parent.children.filterNot(_.name equals txn.name)
          parent.groupComments.foreach { groupComment =>
            reportWriter.println(txnIndent + "  ; " + groupComment)
          }
          otherTxns.foreach { otherTxn =>
            val commentStr = otherTxn.commentOpt.map("  ; " + _).getOrElse("")
            reportWriter.println((txnIndent + "  %-" + maxNameLength + "s %20.2f %s") format (otherTxn.name, otherTxn.delta, commentStr))
          }
          reportWriter.println()
        }
      }
    }
    reportWriter.endCodeBlock()
  }
  try {
    val settingsResult = SettingsHelper.getCompleteSettings(args)
    settingsResult match {
      case Left(errorMsg) => Console.err.println("Error: " + errorMsg)
      case Right(settings) =>
        val (parseError, astEntries, processedFiles) = Processor.parseAll(settings.inputs)
        if (!parseError) {
          val appState = Processor.process(astEntries)
          Processor.checkConstaints(appState, settings.eodConstraints)
          settings.exports.foreach { exportSettings =>
            val reportWriter = new ReportWriter(settings, exportSettings.outFiles)
            exportSettings match {
              case balSettings: LedgerExportSettings =>
                val ledgerRep = Reports.ledgerExport(appState, settings, balSettings)
                exportAsLedger(reportWriter, ledgerRep)
              case xmlSettings: XmlExportSettings =>
                val xmlData = Reports.xmlExport(appState, exportSettings)
                reportWriter.printXml(xmlData)
            }
            reportWriter.close
          }
          settings.reports.foreach { reportSettings =>
            val reportWriter = new ReportWriter(settings, reportSettings.outFiles)

            println()
            reportWriter.filePaths foreach { filePath =>
              println(s"Writing ${reportSettings.title} to: $filePath")
            }
            if (reportWriter.writesToScreen) {
              println()
            }

            reportWriter.printHeading(reportSettings.title)
            reportSettings match {
              case balSettings: BalanceReportSettings =>
                val (leftEntries, rightEntries, totalLeft, totalRight) = Reports.balanceReport(appState, settings, balSettings)
                val left = leftEntries.map(_.render)
                val right = rightEntries.map(_.render)

                val lines = left.zipAll(right, "", "")
                val maxLeftLength = maxElseZero(left.map(_.length))

                def renderBoth(l: String, r: String) = "%-" + (maxLeftLength + 2) + "s%s" format (l, r)
                val balRender = lines.map { case (left, right) => renderBoth(left, right) }
                reportWriter.println(balRender.mkString("\n"))
                val totalLine = renderBoth(totalLeft, totalRight)
                reportWriter.println("─" * maxElseZero((balRender :+ totalLine).map(_.length)))
                reportWriter.println(totalLine)
              case regSettings: RegisterReportSettings =>
                val regReport = Reports.registerReport(appState, regSettings)
                printRegReport(reportWriter, regReport)
              case bookSettings: BookReportSettings =>
                val bookReport = Reports.bookReport(appState, bookSettings)
                printBookReport(reportWriter, bookSettings, bookReport)
            }
            reportWriter.close
          }
        }
    }
  } catch {
    case a: AssertionError  => printErr("Error: " + a.getMessage)
    case i: InputError      => printErr("Input error: " + i.getMessage)
    case i: ConstraintError => printErr("Constraint Failed: " + i.getMessage)
  }

  def printErr(msg: String) = {
    println(Console.RED + Console.BOLD + msg + Console.RESET)
  }
}
