package co.uproot.abandon

import co.uproot.abandon.Helper.maxElseZero
import co.uproot.abandon.web.WebAPI
import org.limium.picoserve.{Handler, Server}
import org.limium.picoserve.Server.{ByteResponse, StringResponse}
import org.rogach.scallop.exceptions.{Help, ScallopException, Version}

import scala.NotImplementedError
import scala.xml.Elem

object CLIApp {
  private def printBalReport(reportWriter: ReportWriter, balanceReport: BalanceReport): Unit = {
    val left = balanceReport.leftEntries.map(_.render)
    val right = balanceReport.rightEntries.map(_.render)

    val lines = left.zipAll(right, "", "")
    val maxLeftLength = maxElseZero(left.map(_.length))

    def renderBoth(l: String, r: String) = "%-" + (maxLeftLength + 2) + "s%s" format (l, r)
    val balRender = lines map { (left, right) => renderBoth(left, right) }
    reportWriter.println(balRender.mkString("\n"))
    val totalLine = renderBoth(balanceReport.totalLeft, balanceReport.totalRight)
    reportWriter.println("─" * maxElseZero((balRender :+ totalLine).map(_.length)))
    reportWriter.println(totalLine)
  }

  private def printRegReport(reportWriter: ReportWriter, regReport: Seq[RegisterReportGroup]): Unit = {
    regReport foreach { reportGroup =>
      reportWriter.println(reportGroup.groupTitle)
      reportGroup.entries foreach { e =>
        reportWriter.println("   " + e.render)
      }
    }
  }

  private def exportAsXML(reportWriter: ReportWriter, ledgerData: Seq[LedgerExportData], txnFilters: Option[TxnFilterStack]): Unit = {
    if (ledgerData.length > 1) {
      throw new NotImplementedError("Balance report with closures exported as XML is not implemented yet.")
    }

    val data = ledgerData.headOption match {
      case Some(head) =>
        head.ledgerEntries.map(e => e.accountName -> e.amount).toMap
      case None => ???
    }
    val accountsByPathLengths = data.groupBy(_._1.fullPath.length)
    val maxPathLength = maxElseZero(accountsByPathLengths.keys)
    // val topLevelAccounts = accountsByPathLengths.get(1).getOrElse(Map())
    def mkTreeLevel(prefix: Seq[String], n: Int): Seq[AccountTreeState] = {
      if (n <= maxPathLength) {
        val children = (n to maxPathLength).flatMap(i => accountsByPathLengths.get(i).getOrElse(Map()).keys.map(_.fullPath).filter(_.startsWith(prefix)).map(_.drop(prefix.length))).toSet
        val (directChildren, inferredChildren) = children.partition(_.length == 1)
        val directChildrenNames = directChildren.map(_.head)
        val inferredChildrenNames = inferredChildren.map(_.head) diff directChildrenNames
        val inferredChildrenTrees = inferredChildrenNames.toSeq.map(x => AccountTreeState(AccountName(prefix :+ x), Helper.Zero, mkTreeLevel(prefix :+ x, n + 1)))
        val directChildrenTrees = directChildren.toSeq.map(x => AccountTreeState(AccountName(prefix ++ x), data(AccountName(prefix ++ x)), mkTreeLevel(prefix ++ x, n + 1)))
        (inferredChildrenTrees ++ directChildrenTrees)
      } else {
        Nil
      }
    }
    val tree = AccountTreeState(AccountName(Nil), Helper.Zero, mkTreeLevel(Nil, 1))
    val balance: Elem =
      <abandon>
        {
        txnFilters match {
          case Some(txnFilter) => {
            <info>
              { txnFilter.xmlDescription() }
            </info>
          }
          case None => ;
        }
        }
        <balance>
          {tree.toXML}
        </balance>
      </abandon>

    reportWriter.printXml(balance)
  }

  private def exportAsLedger(reportWriter: ReportWriter, ledgerRep: Seq[LedgerExportData], txnFilterTxt: List[String]): Unit = {

    if (txnFilterTxt.nonEmpty) {
      reportWriter.println("; ACTIVE FILTER")
      txnFilterTxt.foreach { line => reportWriter.println("; " + line) }
      reportWriter.println(";")
    }

    ledgerRep foreach { reportGroup =>
      reportWriter.println(reportGroup.date.formatCompactYYYYMMDD)
      val formatStr = "%-" + (reportGroup.maxNameLength + 4) + "s %s%s"
      reportGroup.ledgerEntries foreach { e =>
        val render = formatStr format (e.accountName, e.amount.toString(), e.comment.map("  ; " + _).getOrElse(""))
        reportWriter.println("   " + render)
      }
      reportWriter.println("")
    }
  }

  private def printBookReport(reportWriter: ReportWriter, bookReportSettings: BookReportSettings, bookReport: Seq[RegisterReportGroup]): Unit = {
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

  private def buildId: String = {
    "Base: " + BaseBuildInfo.version + " [" + BaseBuildInfo.builtAtString + "];" +
      "CLI: " + CliBuildInfo.version + " [" + CliBuildInfo.builtAtString + "];"
  }

  private def processInput(settings: Settings): (AppState, Set[String]) = {
    val (parseError, astEntries, processedFiles) = Processor.parseAll(settings.inputs, settings.quiet)
    if (!parseError) {
      SettingsHelper.ensureInputProtection(processedFiles, settings)
      // val txnFilters = None
      val appState = Processor.process(astEntries, settings.accounts, settings.txnFilters)
      Processor.checkConstraints(appState, settings.constraints)
      (appState, processedFiles)
    } else {
      throw new InputError("Couldn't parse input")
    }
  }

  private def runApp(cliConf: AbandonCLIConf):Unit = {
    val settingsResult = SettingsHelper.getCompleteSettings(cliConf, buildId)
    settingsResult match {
      case Left(errorMsg) => throw new SettingsError(errorMsg)
      case Right(settings) =>
        val (appState, processedFiles) = processInput(settings)
        settings.exports.foreach { exportSettings =>
          val reportWriter = new ReportWriter(settings, exportSettings.outFiles)
          if (!settings.quiet) {
            println()
            reportWriter.filePaths foreach { filePath =>
              println(s"Exporting to: $filePath")
            }
          }
          exportSettings match {
            case balSettings: BalanceExportSettings =>
              val balanceExp = Reports.balanceExport(appState, settings, balSettings)
              balSettings.exportFormat match {
                case LedgerType =>
                  exportAsLedger(reportWriter, balanceExp, FilterStackHelper.getFilterWarnings(settings.txnFilters, " "))
                case XMLType =>
                  exportAsXML(reportWriter, balanceExp, settings.txnFilters)
              }
            case journalSettings: JournalExportSettings =>
              val xmlData = Reports.xmlExport(appState, journalSettings, settings.txnFilters)
              reportWriter.printXml(xmlData)
            case _ => ???
          }
          reportWriter.close()
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

          reportWriter.close()
        }

        cliConf.webStartDate.foreach(startDateStr =>
          val startDate = Helper.parseDate(startDateStr).get
          startWebServer(settings, startDate, appState, processedFiles)
        )
    }
  }

  private def startWebServer(settings: Settings, startDate: Date, initialAppState: AppState, initialProcessedFiles: Set[String]): Unit = {
    println(Helper.info(s"Starting web server with start date: ${startDate.formatYYYYMMMDD}"))
    val filterDescription = settings.txnFilters.map(_.description())

    var fullReportBytes = WebAPI.makeReport(startDate, initialAppState, filterDescription)

    val server = Server.builder()
      .port(9000)
      .GET("/api/", _ => {
        val msg = String(fullReportBytes)
        new StringResponse(200, msg, java.util.Map.of("Content-type", java.util.List.of("application/json")))
      })
      .handle(staticHandler)
      .build()

    println(Helper.info("Visit http://localhost:9000/ for web UI"))

    server.start()

    FileWatcher().watch(initialProcessedFiles, () => {
      val (appState, newProcessedFiles) = processInput(settings)
      fullReportBytes = WebAPI.makeReport(startDate, appState, filterDescription)
      Some(newProcessedFiles)
    })

    var done = false
    while (!done) {
      val line = io.StdIn.readLine("Type [Q] or [quit] to exit: ")
      done = (line == "Q") || (line.toLowerCase == "quit")
    }
    println("Exiting")

  }

  private val staticHandler = Handler("/", "GET", request => {
    val path = request.getPath()
    val resName = "/frontend/build/" + (if (path == "/") "index.html" else path.tail)
    val resourceStream = this.getClass.getResourceAsStream(resName)
    if (resourceStream != null) {
      val bytes = resourceStream.readAllBytes()
      val contentType = if (resName.endsWith(".js")) {
        "application/javascript"
      } else if (resName.endsWith(".html")) {
        "text/html"
      } else if (resName.endsWith(".css")) {
        "text/css"
      } else "text/plain"

      new ByteResponse(200, bytes, java.util.Map.of("Content-type", java.util.List.of(contentType)))
    } else {
      new StringResponse(404, """{ "error": "not found"} """)
    }
  })

  /**
    * This runs the app, and throws errors and exceptions in case of malfunction.
    * Could be used for testing with assertThrows[ ... ]
    *
    * @param args cli arguments
    */
  def run(args: Array[String]): Unit = {
    val cliConf = new AbandonCLIConf(args.toIndexedSeq)
    cliConf.version("Version: " + CliBuildInfo.version + " [" + CliBuildInfo.builtAtString + "]")

    try {
      cliConf.verify()
      runApp(cliConf)
    } catch {
      case Help(_) =>
      case Version =>
    }
  }


  val SUCCEEDED = 0
  val FAILED = 1

  def mainStatus(args: Array[String]): Int = {
    try {
      run(args)
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

  private def printErrAndFail(msg: String, exOpt: Option[Throwable]): Int = {
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
