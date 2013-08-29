package co.uproot.abandon

import org.rogach.scallop.{ ScallopConf, stringListConverter }
import Helper.{ Zero, filterByType, maxElseZero }

object AbandonApp extends App {
  def printRegReport(regReport: Seq[RegisterReportGroup]) = {
    regReport foreach { reportGroup =>
      println(reportGroup.groupTitle)
      reportGroup.entries foreach { e =>
        println("   " + e.render)

      }
    }
  }

  def printBookReport(bookReportSettings: BookReportSettings, bookReport: Seq[RegisterReportGroup]) = {
    val txnIndent = " " * 49

    println(bookReportSettings.account)

    val maxNameLength = maxElseZero(bookReport.flatMap(_.entries.flatMap(_.txns.flatMap(_.parentOpt.get.children.map(_.name.fullPathStr.length)))))
    bookReport foreach { reportGroup =>
      println(reportGroup.groupTitle)
      reportGroup.entries foreach { e =>

        e.txns foreach { txn =>
          val parent = txn.parentOpt.get
          println(("%20.2f %20.2f        %s") format (txn.resultAmount, txn.delta, parent.dateLineStr))
          // println(txnIndent + parent.dateLineStr)
          val otherTxns = parent.children.filterNot(_.name equals txn.name)
          parent.groupComments.foreach { groupComment =>
            println(txnIndent + "  ; " + groupComment)
          }
          otherTxns.foreach { otherTxn =>
            val commentStr = otherTxn.commentOpt.map("  ; " + _).getOrElse("")
            println((txnIndent + "  %-" + maxNameLength + "s %20.2f %s") format (otherTxn.name, otherTxn.delta, commentStr))
          }
          println()
        }
      }
    }
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
            println("\n\n" + reportSettings.title)
            println("─" * reportSettings.title.length)
            reportSettings match {
              case balSettings: BalanceReportSettings =>
                val (leftEntries, rightEntries, totalLeft, totalRight) = Reports.balanceReport(appState, settings, balSettings)
                val left = leftEntries.map(_.render)
                val right = rightEntries.map(_.render)

                val lines = left.zipAll(right, "", "")
                val maxLeftLength = maxElseZero(left.map(_.length))

                def renderBoth(l: String, r: String) = "%-" + (maxLeftLength + 2) + "s%s" format (l, r)
                val balRender = lines.map { case (left, right) => renderBoth(left, right) }
                println(balRender.mkString("\n"))
                val totalLine = renderBoth(totalLeft, totalRight)
                println("─" * maxElseZero((balRender :+ totalLine).map(_.length)))
                println(totalLine)
              case regSettings: RegisterReportSettings =>
                val regReport = Reports.registerReport(appState, regSettings)
                printRegReport(regReport)
              case bookSettings: BookReportSettings =>
                val bookReport = Reports.bookReport(appState, bookSettings)
                printBookReport(bookSettings, bookReport)
            }
          }
        }
    }
  } catch {
    case a: AssertionError => println("Error: " + a.getMessage)
    case i: InputError     => println("Input error: " + i.getMessage)
  }

}
