package co.uproot.abandon

import scalafx.Includes._
import scalafx.scene.control.TreeView
import scalafx.scene.control.TreeItem
import scalafx.scene.input.KeyEvent
import scalafx.scene.control.TreeCell
import scalafx.stage.Stage
import scalafx.scene.Scene
import scalafx.scene.control.ScrollPane
import scalafx.stage.Modality

object RegUIReport extends UIReport {
  private def getNestedTxns(item: TreeItem[RegisterReportEntry]): Seq[DetailedPost] = {
    item.getValue.txns ++ item.children.flatMap(getNestedTxns(_))
  }

  def mkRegisterReport(appState: AppState, reportSettings: RegisterReportSettings) = {
    val registers = Reports.registerReport(appState, reportSettings)
    val registerItems = registers.map { rg =>
      new TreeItem(RegisterReportEntry(null, Nil, rg.groupTitle)) {
        children = rg.entries.map(new TreeItem(_))
        expanded = true
      }
    }
    val reportRoot = new TreeItem(RegisterReportEntry(null, Nil, "Register report (account, change during period, balance at end of period)")) {
      children = registerItems
      expanded = true
    }
    new TreeView(reportRoot) {
      styleClass += styleClassName
      onKeyTyped = { e: KeyEvent =>
        if (e.character equals "\r") {
          val selectedItemOpt = selectionModel().getSelectedItems().headOption
          selectedItemOpt foreach { selectedItem =>
            val heading =
              if (selectedItem.getValue.accountName == null) {
                selectedItem.getValue.render
              } else {
                selectedItem.getParent.getValue.render + " / " + selectedItem.getValue.accountName
              }

            val txStage = new Stage() {
              scene = new Scene(800, 600) {
                root = new ScrollPane {
                  content = TxnUIReport.mkTxnView(getNestedTxns(selectedItem))
                }
                stylesheets += "default_theme.css"
              }
              initModality(Modality.ApplicationModal)
              title = "Transactions for " + heading
            }
            txStage.show
          }
        }
      }
      cellFactory = { v =>
        val delegate = new javafx.scene.control.TreeCell[RegisterReportEntry]() {
          override def updateItem(t: RegisterReportEntry, empty: Boolean) = {
            super.updateItem(t, empty)
            setText(if (t != null) t.render else null)
          }
        }
        new TreeCell(delegate)
      }
    }
  }

}
