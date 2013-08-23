package co.uproot.abandon

import org.rogach.scallop.{ ScallopConf, stringListConverter }
import Helper.{ Zero, filterByType, maxElseZero }

object AbandonApp extends App {
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
                regReport foreach {
                  case RegisterReportGroup(groupTitle, entries) =>
                    println(groupTitle)
                    entries.map(_.render) foreach { e => println("   " + e) }
                }
            }
          }
        }
    }
  } catch {
    case a: AssertionError => println("Error: " + a.getMessage)
    case i: InputError     => println("Input error: " + i.getMessage)
  }

}
