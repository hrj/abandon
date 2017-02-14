package co.uproot.abandon

import scala.util.matching.Regex
import scalafx.Includes._
import scalafx.application.Platform
import scalafx.scene.Scene
import scalafx.scene.control.{ScrollPane, TreeCell, TreeItem, TreeView}
import scalafx.scene.input.KeyEvent
import scalafx.stage.{Modality, Stage}

object RegUIReport extends UIReport {

  case class TxnItem(txnGroup: String, reportEntry: RegisterReportEntry)

  implicit class TxnItemMatcher(t: TxnItem) {
    val accountMatcher: Regex = """[^\s]+""".r
    def matches(item: TreeItem[TxnItem]): Boolean = {
      isMatchingParent(item) || isMatchingTxnGroup(item)
    }
    private def isMatchingParent(item: TreeItem[TxnItem]) = {
      t.reportEntry.txns.isEmpty && t.reportEntry.render == item.getValue.reportEntry.render
    }
    private def isMatchingTxnGroup(item: TreeItem[TxnItem]) = {
      t.reportEntry.txns.nonEmpty && t.txnGroup == item.getValue.txnGroup &&
        accountName(t.reportEntry) == accountName(item.getValue.reportEntry)
    }
    private def accountName(entry: RegisterReportEntry): String = {
      accountMatcher.findFirstIn(t.reportEntry.render).getOrElse(t.reportEntry.render)
    }
  }

  private def getNestedTxns(item: TreeItem[TxnItem]): Seq[DetailedPost] = {
    filterSelectedDates(item.getValue.reportEntry.txns) ++ item.children.flatMap(getNestedTxns(_))
  }
  def filterSelectedDates(txns: Seq[DetailedPost]): Seq[DetailedPost] = {
    if (selectedTxnDates.isEmpty) txns
    else txns.filter(txn => selectedTxnDates.contains(txn.date))
  }
  private def getSelectedTxnDates(item: TreeItem[TxnItem]): Seq[Date] = {
    item.getValue.reportEntry.txns.groupBy(_.date).unzip._1.toSeq ++ item.children.flatMap(getSelectedTxnDates(_))
  }
  private def getTxContent(selectedItem: TreeItem[TxnItem]) = {
    new ScrollPane {
      content = TxnUIReport.mkTxnView(getNestedTxns(selectedItem))
    }
  }

  var selectedItem: TreeItem[TxnItem] = _
  var selectedTxnDates: Seq[Date] = Nil
  var txStage: Stage = _

  def mkRegisterReport(appState: AppState, reportSettings: RegisterReportSettings) = {
    val registers = Reports.registerReport(appState, reportSettings)
    val registerItems = registers.map { rg =>
      new TreeItem(TxnItem(rg.groupTitle, RegisterReportEntry(Nil, rg.groupTitle))) {
        children = rg.entries.map(entry => new TreeItem(TxnItem(rg.groupTitle, entry)))
        expanded = true
      }
    }
    val rootTitle = "Register report (account, delta, total)"
    val reportRoot = new TreeItem(TxnItem(rootTitle, RegisterReportEntry(Nil, rootTitle))) {
      children = registerItems
      expanded = true
    }
    new TreeView(reportRoot) {
      styleClass += styleClassName
      onKeyTyped = { e: KeyEvent =>
        if (e.character.equals("\r") && !selectionModel().isEmpty) {
          // println(e)
          selectedItem = selectionModel().getSelectedItem
          selectedTxnDates = getSelectedTxnDates(selectedItem)
          txStage = new Stage() {
            scene = new Scene(800, 600) {
              root = getTxContent(selectedItem)
              stylesheets += "default_theme.css"
            }
            initModality(Modality.ApplicationModal)
            title = "Transactions"
          }
          txStage.show
        }
      }
      cellFactory = { _ =>
        val delegate = new javafx.scene.control.TreeCell[TxnItem]() {
          override def updateItem(t: TxnItem, empty: Boolean): Unit = {
            super.updateItem(t, empty)
            if (t == null) {
              setText(null)
            } else {
              setText(t.reportEntry.render)
              refreshTrxs(t)
            }
          }
        }
        new TreeCell(delegate)
      }
      private def refreshTrxs(t: TxnItem) = {
        if (txStage != null && txStage.showing() && (t matches selectedItem)) {
          // update the selected item
          selectedItem =
            if (t matches reportRoot) reportRoot
            else registerItems.find(t matches _).getOrElse(selectedItem)
          Platform.runLater(() => {
            txStage.delegate.scene().root() = getTxContent(selectedItem)
          })
        }
      }
    }
  }
}
