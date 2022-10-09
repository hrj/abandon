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

  case class SelectionFilter(itemName: String, parentNameOpt: Option[String]) {
    def heading = parentNameOpt.map(_ + " / ").getOrElse("") + itemName

    def isMatching(entry: TreeItem[RegisterReportEntry]): Boolean = {
      parentNameOpt match {
        case Some(pn) =>
          itemName == entry.getValue.accountName && pn == entry.getParent.getValue.render
        case None =>
          itemName == entry.getValue.render
      }
    }

  }

  private var transactionViewRegTitle: String = _
  private var selectedFilterOpt: Option[SelectionFilter] = None

  private val txnRootSP: ScrollPane = new ScrollPane { }
  private val txStage = new Stage() {
    scene = new Scene(800, 600) {
      root = txnRootSP
      stylesheets += "default_theme.css"
    }
    initModality(Modality.ApplicationModal)
    onCloseRequest = () => selectedFilterOpt = None
  }

  private def showTransactions(selectedItemOpt: Option[javafx.scene.control.TreeItem[RegisterReportEntry]]):Unit = {
    val txns = selectedItemOpt.map(si => getNestedTxns(si)).getOrElse(Nil)
    txnRootSP.content = TxnUIReport.mkTxnView(txns)
    txStage.title = "Transactions for " + selectedFilterOpt.get.heading
    txStage.show()
  }

  private def findMatchingEntry(entry: TreeItem[RegisterReportEntry], filter: SelectionFilter):Option[javafx.scene.control.TreeItem[RegisterReportEntry]] = {
    if(filter.isMatching(entry)) {
      Some(entry.delegate)
    } else {
      entry.children.map(c => findMatchingEntry(c, filter)).find(_.isDefined).getOrElse(None)
    }
  }

  def mkRegisterReport(title: String, appState: AppState, reportSettings: RegisterReportSettings) = {
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

    if (title == transactionViewRegTitle) {
      // update transactions view
      selectedFilterOpt foreach {sf => showTransactions(findMatchingEntry(reportRoot, sf))}
    }

    new TreeView(reportRoot) {
      styleClass += styleClassName
      onKeyTyped = { (e: KeyEvent) =>
        if (e.character equals "\r") {
          val selectedItemOpt = selectionModel().getSelectedItems().headOption
          selectedFilterOpt = selectedItemOpt map {selectedItem =>
            if (selectedItem.getValue.accountName == null) {
              SelectionFilter(selectedItem.getValue.render, None)
            } else {
              SelectionFilter(selectedItem.getValue.accountName, Option(selectedItem.getParent.getValue.render))
            }
          }
          transactionViewRegTitle = title
          showTransactions(selectedItemOpt)
        }
      }
      val callback: TreeView[RegisterReportEntry] => TreeCell[RegisterReportEntry] = { (v: TreeView[RegisterReportEntry]) =>
        val delegate = new javafx.scene.control.TreeCell[RegisterReportEntry]() {
          override def updateItem(t: RegisterReportEntry, empty: Boolean) = {
            super.updateItem(t, empty)
            setText(if (t != null) t.render else null)
          }
        }
        new TreeCell(delegate)
      }

      cellFactory = callback
    }
  }

}
