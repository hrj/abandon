package co.uproot.abandon

import scalafx.Includes._
import scalafx.application.Platform
import scalafx.scene.Scene
import scalafx.scene.control.{ScrollPane, TreeCell, TreeItem, TreeView}
import scalafx.scene.input.KeyEvent
import scalafx.stage.{Modality, Stage}

object RegUIReport extends UIReport {
  private def getNestedTxns(item: TreeItem[RegisterReportEntry]): Seq[DetailedPost] = {
    item.getValue.txns ++ item.children.flatMap(getNestedTxns(_))
  }
  private def getTxContent(selectedItem: TreeItem[RegisterReportEntry]) = {
    new ScrollPane {
      content = TxnUIReport.mkTxnView(getNestedTxns(selectedItem))
    }
  }

  var selectedItem: TreeItem[RegisterReportEntry] = null
  var txStage: Stage = null
  var trxShown: Boolean = false

  def mkRegisterReport(appState: AppState, reportSettings: RegisterReportSettings) = {
    val registers = Reports.registerReport(appState, reportSettings)
    val registerItems = registers.map { rg =>
      new TreeItem(RegisterReportEntry(Nil, rg.groupTitle)) {
        children = rg.entries.map(new TreeItem(_))
        expanded = true
      }
    }
    val reportRoot = new TreeItem(RegisterReportEntry(Nil, "Register report (account, delta, total)")) {
      children = registerItems
      expanded = true
    }
    new TreeView(reportRoot) {
      styleClass += styleClassName
      onKeyTyped = { e: KeyEvent =>
        if (e.character.equals("\r") && !selectionModel().isEmpty) {
          // println(e)
          selectedItem = selectionModel().getSelectedItem
          txStage = new Stage() {
            scene = new Scene(800, 600) {
              root = getTxContent(selectedItem)
              stylesheets += "default_theme.css"
            }
            initModality(Modality.ApplicationModal)
            title = "Transactions"
          }
          txStage.onCloseRequest = () => trxShown = false
          txStage.show
          trxShown = true
        }
      }
      cellFactory = { _ =>
        val delegate = new javafx.scene.control.TreeCell[RegisterReportEntry]() {
          private def refreshTrxs(t: RegisterReportEntry) = {
            if (trxShown && selectedItem.getValue == t) {
              selectedItem =
                if (selectedItem.getValue.render == reportRoot.getValue.render) reportRoot
                else registerItems.find(item => item.getValue == t).getOrElse(selectedItem)
              Platform.runLater(() => {
                txStage.delegate.scene().root() = getTxContent(selectedItem)
              })
            }
          }
          override def updateItem(t: RegisterReportEntry, empty: Boolean) = {
            super.updateItem(t, empty)
            if (t == null) {
              setText(null)
            } else {
              setText(t.render)
              refreshTrxs(t)
            }
          }
        }
        new TreeCell(delegate)
      }
    }
  }
}
