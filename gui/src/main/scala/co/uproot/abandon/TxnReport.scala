package co.uproot.abandon

import scalafx.scene.layout.VBox
import scalafx.scene.control.Label
import scalafx.geometry.Insets
import Helper._

// The sole purpose of this class is to ensure that equality is checked using references
final class GroupWrap(val tg: TxnGroup) {
  override def hashCode() = {
    tg.hashCode
  }

  override def equals(that:Any) = {
    that match {
      case thatGroup: GroupWrap  =>
        tg eq thatGroup.tg
      case _ => false
    }
  }
}

import scalafx.Includes._
object TxnReport extends Report {
  def mkTxnGroupViews(txns: Seq[DetailedTransaction]) = {
    val maxNameLength = maxElseZero(txns.flatMap(_.parent.get.children.map(_.name.fullPathStr.length)))

    val groups = txns.map(t => new GroupWrap(t.parent.get)).distinct.map(_.tg)
    val sortedGroups = groups.sortBy(_.date)(DateOrdering)

    sortedGroups.map(grp => new VBox {
      val grpLabel = {
        val annotationStr = grp.annotationOpt.map(" (" + _ + ")").getOrElse("")
        val payeeStr = grp.payeeOpt.map(" " + _).getOrElse("")
        new Label(
          s"${grp.date.formatYYYYMMMDD}$annotationStr$payeeStr") {
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

  def mkTxnView(txns: Seq[DetailedTransaction]) = {
    new VBox {
      styleClass += styleClassName
      content = mkTxnGroupViews(txns)
    }
  }
}
