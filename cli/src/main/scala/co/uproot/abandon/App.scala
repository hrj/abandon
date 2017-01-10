package co.uproot.abandon

import java.io.FileWriter
import java.nio.file.{Files, Paths}

import co.uproot.abandon.Helper.maxElseZero
import org.rogach.scallop.exceptions.ScallopException

final class ReportWriter(settings: Settings, outFiles: Seq[String]) {
  val writesToScreen = outFiles.contains("-") || outFiles.isEmpty
  val filePaths = outFiles.filterNot(_ equals "-").map(settings.getConfigRelativePath(_))
  val fileWriters = filePaths.map(pathStr =>  {
      val path = Paths.get(pathStr).normalize
      val parentPath = path.getParent
      if (parentPath != null) {
        if (!Files.exists(parentPath)) {
          if (!settings.quiet) {
            Console.println("Creating directory: " + parentPath)
          }
          Files.createDirectories(parentPath)
        }
      }


      try {
        new FileWriter(pathStr)
      } catch {
        case fnf : java.io.FileNotFoundException => throw new SettingsError("Could not write to " + pathStr + ". " + fnf.getMessage)
      }
  })

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
      Console.println(head + "\n" + ("─" * head.length) + "\n")
    }
  }

  def println(s: String*) = {
    fileWriters foreach { fileWriter =>
      s.foreach(str => fileWriter.write(str))
      fileWriter.write('\n')
    }

    if (writesToScreen) {
      s.foreach(print(_))
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
      Console.println(sb.toString)
    }
  }

  def close = {
    fileWriters foreach { fileWriter =>
      fileWriter.close
    }
    Console.flush
  }
}

object CLIApp {
  def printBalReport(reportWriter: ReportWriter, balanceReport: BalanceReport) = {
    val left = balanceReport.leftEntries.map(_.render)
    val right = balanceReport.rightEntries.map(_.render)

    val lines = left.zipAll(right, "", "")
    val maxLeftLength = maxElseZero(left.map(_.length))

    def renderBoth(l: String, r: String) = "%-" + (maxLeftLength + 2) + "s%s" format (l, r)
    val balRender = lines map {
      case (left, right) => renderBoth(left, right)
      case _ => ???
    }
    reportWriter.println(balRender.mkString("\n"))
    val totalLine = renderBoth(balanceReport.totalLeft, balanceReport.totalRight)
    reportWriter.println("─" * maxElseZero((balRender :+ totalLine).map(_.length)))
    reportWriter.println(totalLine)
  }

  def printRegReport(reportWriter: ReportWriter, regReport: Seq[RegisterReportGroup]) = {
    regReport foreach { reportGroup =>
      reportWriter.println(reportGroup.groupTitle)
      reportGroup.entries foreach { e =>
        reportWriter.println("   " + e.render)
      }
    }
  }

  def exportAsLedger(reportWriter: ReportWriter, ledgerRep: Seq[LedgerExportData], txnFilterTxt: List[String]) = {

    if (txnFilterTxt.nonEmpty) {
      reportWriter.println("; ACTIVE FILTER")
      txnFilterTxt.foreach { line => reportWriter.println("; " + line) }
      reportWriter.println(";")
    }

    ledgerRep foreach { reportGroup =>
      reportWriter.println(reportGroup.date.formatCompactYYYYMMDD)
      val formatStr = "%-" + (reportGroup.maxNameLength + 4) + "s %s"
      reportGroup.ledgerEntries foreach { e =>
        val render = formatStr format (e.accountName, e.amount.toString())
        reportWriter.println("   " + render)
      }
      reportWriter.println("")
    }
  }

  def printBookReport(reportWriter: ReportWriter, bookReportSettings: BookReportSettings, bookReport: Seq[RegisterReportGroup]) = {
    val txnIndent = " " * 49

    reportWriter.println("Account Name: " + bookReportSettings.account + "\n")

    val maxNameLength = maxElseZero(bookReport.flatMap(_.entries.flatMap(_.txns.flatMap(_.parentOpt match {
      case Some(parent) => parent.children.map(_.name.fullPathStr.length)
      case _ => ???
    }))))
    reportWriter.startCodeBlock()

    bookReport foreach { reportGroup =>
      reportWriter.println(reportGroup.groupTitle)
      reportGroup.entries foreach { e =>
        e.txns foreach { txn =>
          val maybeParent = txn.parentOpt
          maybeParent match {
            case Some(parent) => {
              reportWriter.println(("%20.2f %20.2f        %s") format(txn.resultAmount, txn.delta, parent.dateLineStr))
              // println(txnIndent + parent.dateLineStr)
              val otherTxns = parent.children.filterNot(_.name equals txn.name)
              parent.groupComments.foreach { groupComment =>
                reportWriter.println(txnIndent + "  ; " + groupComment)
              }
              otherTxns.foreach { otherTxn =>
                val commentStr = otherTxn.commentOpt.map("  ; " + _).getOrElse("")
                reportWriter.println((txnIndent + "  %-" + maxNameLength + "s %20.2f %s") format(otherTxn.name, otherTxn.delta, commentStr))
              }
              reportWriter.println()
            }
            case None => ??? // What do we do in the event we have no parent?
          }
        }
      }
    }
    reportWriter.endCodeBlock()
  }

  def buildId: String = {
    "Base: " + BaseBuildInfo.version + " [" + BaseBuildInfo.builtAtString + "];" +
      "CLI: " + CliBuildInfo.version + " [" + CliBuildInfo.builtAtString + "];"
  }

  private def runApp(cliConf: AbandonCLIConf) {
    val settingsResult = SettingsHelper.getCompleteSettings(cliConf, buildId)
    settingsResult match {
      case Left(errorMsg) => throw new SettingsError(errorMsg)
      case Right(settings) =>
        val (parseError, astEntries, processedFiles) = Processor.parseAll(settings.inputs, settings.quiet)
        if (!parseError) {
          val txnFilters = None
          val appState = Processor.process(astEntries,settings.accounts, settings.txnFilters)
          Processor.checkConstaints(appState, settings.constraints)
          settings.exports.foreach { exportSettings =>
            val reportWriter = new ReportWriter(settings, exportSettings.outFiles)
            if (!settings.quiet) {
              println()
              reportWriter.filePaths foreach { filePath =>
                println(s"Exporting to: $filePath")
              }
            }
            exportSettings match {
              case balSettings: LedgerExportSettings =>
                val ledgerRep = Reports.ledgerExport(appState, settings, balSettings)
                exportAsLedger(reportWriter, ledgerRep, FilterStackHelper.getFilterWarnings(settings.txnFilters, " "))
              case xmlSettings: XmlExportSettings =>
                val xmlData = Reports.xmlExport(appState, xmlSettings, settings.txnFilters)
                reportWriter.printXml(xmlData)
              case _ => ???
            }
            reportWriter.close
          }
          settings.reports.foreach { reportSettings =>
            val reportWriter = new ReportWriter(settings, reportSettings.outFiles)
            if (!settings.quiet) {
              println()
              reportWriter.filePaths foreach { filePath =>
                println(s"Writing ${reportSettings.title} to: $filePath")
              }
              if (reportWriter.writesToScreen) {
                println()
              }
            }

            reportWriter.printHeading(reportSettings.title)

            if (settings.txnFilters.nonEmpty) {
              reportWriter.println("ACTIVE FILTER")
              FilterStackHelper.getFilterWarnings(settings.txnFilters, "  ").foreach { line => reportWriter.println(line)}
              reportWriter.println("")
            }

            reportSettings match {
              case balSettings: BalanceReportSettings =>
                val balanceReport = Reports.balanceReport(appState, settings, balSettings)
                printBalReport(reportWriter, balanceReport)
              case regSettings: RegisterReportSettings =>
                val regReport = Reports.registerReport(appState, regSettings)
                printRegReport(reportWriter, regReport)
              case bookSettings: BookReportSettings =>
                val bookReport = Reports.bookReport(appState, bookSettings)
                printBookReport(reportWriter, bookSettings, bookReport)
              case _ => ???
            }

            reportWriter.close
          }
        } else {
          throw new InputError("Couldn't parse input")
        }
      case _ => ???
    }
  }

  /**
    * This runs the app, and throws errors and exceptions in case of malfunction.
    * Could be used for testing with assertThrows[ ... ]
    *
    * @param args cli arguments
    */
  def run(args: Array[String]): Unit = {
    val cliConf = new AbandonCLIConf(args)
    cliConf.verify()

    if (cliConf.version.supplied) {
      println("Version: " + CliBuildInfo.version + " [" + CliBuildInfo.builtAtString + "]")
    } else {
      runApp(cliConf)
    }
  }


  val SUCCEEDED = 0
  val FAILED = 1

  def mainStatus(args: Array[String]): Int = {
    try {
        co.uproot.abandon.CLIApp.run(args)
      SUCCEEDED
    } catch {
      case ex: AssertionError =>
        printErrAndFail("Internal Assertion Error (please report a bug):" + ex.getMessage, Some(ex))

      case ex: ConstraintError =>
        printErrAndFail("Constraint Failed: " + ex.getMessage, None)

      case ex: InputError =>
        printErrAndFail("Input error: " + ex.getMessage, None)

      case ex: NotImplementedError =>
        printErrAndFail("Some functionality has not yet been implemented. " +
          "We intend to implement it eventually. More details:\n" + ex.getMessage, None)

      case ex: ScallopException =>
        printErrAndFail("CLI Argument error: " + ex.getMessage, None)

      case ex: SettingsError =>
        printErrAndFail("Configuration error: " + ex.getMessage, None)

      case t: Exception =>
        printErrAndFail("Unexpected error", Some(t))

      case t: Error =>
        printErrAndFail("Unexpected error", Some(t))
    }
  }

  def printErrAndFail(msg: String, exOpt: Option[Throwable]): Int = {
    println(Console.RED + Console.BOLD + msg + Console.RESET)
    exOpt match {
      case Some(ex) => ex.printStackTrace(Console.out)
      case None =>
    }
    FAILED
  }

  def main(args: Array[String]): Unit = {
    System.exit(co.uproot.abandon.CLIApp.mainStatus(args))
  }
}
