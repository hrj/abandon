package co.uproot.abandon

import scalafx.scene.layout.VBox
import scalafx.scene.control.Label
import scalafx.geometry.Insets
import Helper._

import scalafx.Includes._
object TxnReport extends Report {
  def mkTxnGroupViews(txns: Seq[DetailedTransaction]) = {
    val groups = txns.map(t => new RefWrap(t.parentOpt.get)).distinct.map(_.t)
    val sortedGroups = groups.sortBy(_.date)(DateOrdering)

    sortedGroups.map(grp => new VBox {
      val grpLabel = {
        new Label(grp.dateLineStr) {
          styleClass += "txn-date-line"
        }
      }
      val grpCommentLabels = grp.groupComments.map(
        c => new Label("  ;" + c) {
          styleClass += "txn-comment"
        })

      val maxNameLength = maxElseZero(txns.flatMap(_.parentOpt.get.children.map(_.name.fullPathStr.length)))
      val childLabels = grp.children.map { c =>
        val commentStr = c.commentOpt.map("  ; " + _).getOrElse("")
        ("  %-" + maxNameLength + "s %20.2f %s") format (c.name, c.delta, commentStr)
      }.map(new Label(_))
      padding = Insets(10, 10, 10, 10)
      children = (grpLabel +: grpCommentLabels) ++ childLabels
    })
  }

  def mkTxnView(txns: Seq[DetailedTransaction]) = {
    new VBox {
      styleClass += styleClassName
      children = mkTxnGroupViews(txns)
    }
  }
}
