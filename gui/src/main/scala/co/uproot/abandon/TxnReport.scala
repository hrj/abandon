package co.uproot.abandon

import scalafx.Includes._
import scalafx.scene.layout.VBox
import scalafx.scene.control.Label
import scalafx.geometry.Insets
import Helper._

object TxnReport extends Report {

  def mkTxnView(txns:Seq[DetailedTransaction]) = {
    new VBox {
      style = fontStyle
      val maxNameLength = maxElseZero(txns.flatMap(_.parent.get.children.map(_.name.fullPathStr.length)))
      content = txns.map(t => new VBox {
        val grp = t.parent.get
        val dateLabel = new Label(s"""${t.date.formatYYYYMMDD} ${grp.payeeOpt.getOrElse("")}""") {style="-fx-font-weight:bold"}
        val grpCommentLabels = grp.groupComments.map(c => new Label("  ;"+c) {style="-fx-font-weight:bold"})
        val childLabels = grp.children.map(c=>
          ("  %-"+maxNameLength+"s %20.2f %s") format (c.name, c.delta, c.commentOpt.map("  ; " + _).getOrElse(""))
        ).map(new Label(_))
        padding = Insets(10, 10, 10, 10)
        content = (dateLabel +: grpCommentLabels) ++ childLabels
      })
    }
  }
}
