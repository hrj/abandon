package co.uproot.abandon

import org.rogach.scallop.{ ScallopConf, stringListConverter }
import Helper.{ Zero, filterByType, maxElseZero }
import java.io.FileWriter

final class ReportWriter(settings: Settings, outFiles: Seq[String]) {
  val writesToScreen = outFiles.contains("-") || outFiles.isEmpty

  val filePaths = outFiles.filterNot(_ equals "-").map(settings.getConfigRelativePath(_))
  val fileWriters = filePaths.map(path => new FileWriter(path))

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

  def printBookReport(reportWriter: ReportWriter, bookReportSettings: BookReportSettings, bookReport: Seq[RegisterReportGroup]) = {
    val txnIndent = " " * 49

    reportWriter.println("Account Name: " + bookReportSettings.account + "\n")

    val maxNameLength = maxElseZero(bookReport.flatMap(_.entries.flatMap(_.txns.flatMap(_.parentOpt.get.children.map(_.name.fullPathStr.length)))))
    reportWriter.println("```")

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
    reportWriter.println("```")
  }

  try {
    val settingsResult = SettingsHelper.getCompleteSettings(args)
    settingsResult match {
      case Left(errorMsg) => println("Error:", errorMsg)
      case Right(settings) =>

        val (parseError, astEntries, processedFiles) = Processor.parseAll(settings.inputs)
        if (!parseError) {
          val appState = Processor.process(astEntries)
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
    case a: AssertionError => println("Error: " + a.getMessage)
    case i: InputError     => println("Input error: " + i.getMessage)
  }

}
