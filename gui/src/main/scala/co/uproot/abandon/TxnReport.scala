package co.uproot.abandon

import scalafx.Includes._
import scalafx.scene.layout.VBox
import scalafx.scene.control.Label
import scalafx.geometry.Insets
import Helper._

object TxnReport extends Report {

  def mkTxnView(txns: Seq[DetailedTransaction]) = {
    new VBox {
      styleClass += styleClassName
      val maxNameLength = maxElseZero(txns.flatMap(_.parent.get.children.map(_.name.fullPathStr.length)))
      content = txns.map(t => new VBox {
        val grp = t.parent.get
        val grpLabel = {
          val annotationStr = grp.annotationOpt.map(" (" + _ + ")").getOrElse("")
          val payeeStr = grp.payeeOpt.map(" " + _).getOrElse("")
          new Label(
            s"${t.date.formatYYYYMMMDD}$annotationStr$payeeStr") {
            styleClass += "txn-date-line"
          }
        }
        val grpCommentLabels = grp.groupComments.map(
          c => new Label("  ;" + c) {
            styleClass += "txn-comment"
          })
        val childLabels = grp.children.map { c =>
          val commentStr = c.commentOpt.map("  ; " + _).getOrElse("")
          ("  %-" + maxNameLength + "s %20.2f %s") format (c.name, c.delta, commentStr)
        }.map(new Label(_))
        padding = Insets(10, 10, 10, 10)
        content = (grpLabel +: grpCommentLabels) ++ childLabels
      })
    }
  }
}
