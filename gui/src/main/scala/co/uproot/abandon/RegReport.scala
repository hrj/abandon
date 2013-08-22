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

object RegReport extends Report {
  private def getNestedTxns(item: TreeItem[RegisterReportEntry]):Seq[DetailedTransaction] = {
    item.getValue.txns ++ item.children.flatMap(getNestedTxns(_))
  }

  def mkRegisterReport(appState: AppState, reportSettings: RegisterReportSettings) = {
    val registers = Reports.registerReport(appState, reportSettings)
    val registerItems = registers.map { r =>
      new TreeItem(RegisterReportEntry(Nil, r._1)) {
        children = r._2.map(new TreeItem(_))
        expanded = true
      }
    }
    val reportRoot = new TreeItem(RegisterReportEntry(Nil, "Register report (account, delta, total)")) {
      children = registerItems
      expanded = true
    }
    new TreeView(reportRoot) {
      style = fontStyle
      onKeyTyped = { e: KeyEvent =>
        if (e.character equals "\r") {
          val selectedItemOpt = selectionModel().getSelectedItems().headOption
          selectedItemOpt foreach { selectedItem =>
            // println(e)
            val txStage = new Stage() {
              scene = new Scene(800, 600) {
                root = new ScrollPane {
                  content = TxnReport.mkTxnView(getNestedTxns(selectedItem))
                }
              }
              initModality(Modality.APPLICATION_MODAL)
              title = "Transactions"
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
