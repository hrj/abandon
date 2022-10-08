package co.uproot.abandon

import javafx.scene.control.TreeView
import scalafx.Includes.*
import scalafx.scene.layout.HBox
import scalafx.scene.control.ListView
import scalafx.scene.layout.Priority
import scalafx.scene.input.KeyEvent
import scalafx.scene.control.ListCell

object BalanceUIReport extends UIReport {

  def mkBalanceReport(appState: AppState, settings: Settings, reportSettings: BalanceReportSettings) = {
    val balReport = Reports.balanceReport(appState, settings, reportSettings)

    val padLength = Math.max(balReport.leftEntries.length, balReport.rightEntries.length) + 1
    val left = balReport.leftEntries.padTo(padLength, BalanceReportEntry(None, "")) :+ BalanceReportEntry(None, balReport.totalLeft)
    val right = balReport.rightEntries.padTo(padLength, BalanceReportEntry(None, "")) :+ BalanceReportEntry(None, balReport.totalRight)

    class BalanceView(entries: Seq[BalanceReportEntry]) extends ListView(entries) {
      hgrow = Priority.Always
      onKeyTyped = { (e: KeyEvent) =>
        // println ("Typed key", e.character, e.code, jfxKeyCode.ENTER, e.delegate.code)
        // if (e.code equals jfxKeyCode.ENTER) {
        if (e.character equals "\r") {
          val selectedItems = selectionModel().getSelectedItems()
          val selectedAccountNames = selectedItems.flatMap(_.accName.toIterable)
          val selectedAccountPatterns = selectedAccountNames.map("^" + _.fullPathStr + ".*")
          val regSettings =
            RegisterReportSettings(
              selectedAccountNames.map(_.fullPathStr).mkString(","),
              Some(selectedAccountPatterns.toSeq),
              Nil,
              GroupByMonth()
            )
          CurrReports.addReport(appState, settings, regSettings, canClose = true)
        }
      }
      cellFactory = { (v: ListView[BalanceReportEntry]) =>
        val delegate = new javafx.scene.control.ListCell[BalanceReportEntry]() {
          override def updateItem(t: BalanceReportEntry, empty: Boolean) = {
            super.updateItem(t, empty)
            setText(if (t != null) t.render else null)
          }
        }
        new ListCell(delegate)
      }
    }

    new HBox {
      def getWidth = width
      hgrow = Priority.Always
      styleClass += styleClassName
      children = Seq(
        new BalanceView(left),
        new BalanceView(right)
      )
    }
  }

}
