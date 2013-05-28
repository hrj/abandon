package co.uproot.abandon

import Helper.{ Zero, maxElseZero }

case class BalanceReportEntry(accName: Option[AccountName], render: String)
case class RegisterReportEntry(txns: Seq[DetailedTransaction], render: String)

object Reports {

  def balanceReport(state: AppState, settings: Settings, reportSettings: BalanceReportSettings) = {
    def show(width: Int, a: AccountTreeState, maxNameLength: Int, treePrefix: String = "", isLastChild: Boolean = false, isParentLastChild: Boolean = false, prefix: Option[String] = None, forceIndent: Option[Int] = None): Seq[BalanceReportEntry] = {
      val indent = forceIndent.getOrElse(a.name.depth)
      val amountIsZero = a.amount equals Zero
      val renderableChildren = a.childrenNonZero
      val onlyChildren = (renderableChildren == 1) && amountIsZero

      val myPrefix = treePrefix + (
        if (indent <= 1) {
          ""
        } else if (isLastChild && !(prefix.isDefined && !isParentLastChild)) {
          " └╴"
        } else {
          " ├╴"
        })

      val childTreePrefix = (
        if (onlyChildren) {
          treePrefix
        } else if (isLastChild) {
          treePrefix + "   "
        } else {
          if (indent <= 1) {
            treePrefix
          } else {
            treePrefix + " │ "
          }
        })

      val children = a.childStates
      val lastChildIndex = children.length - 1
      val renderedChildren = children.sortBy(_.name.toString).zipWithIndex.flatMap {
        case (c, i) =>
          show(width, c, maxNameLength, childTreePrefix, i == lastChildIndex, isLastChild || (prefix.isDefined && isParentLastChild),
            if (onlyChildren) Some(prefix.map(_ + ":").getOrElse("") + a.name.name) else None,
            if (onlyChildren) Some(indent) else None
          )
      }
      lazy val selfRender = (
        BalanceReportEntry(Some(a.name),
          ("%" + width + ".2f   %-" + maxNameLength + "s") format (
            a.total, myPrefix + (prefix.map(_ + ":").getOrElse("") + a.name.name)
          )
        )
      )
      if (renderableChildren == 0) {
        if (amountIsZero) {
          Nil
        } else {
          Seq(selfRender)
        }
      } else if (onlyChildren) {
        renderedChildren
      } else {
        selfRender +: renderedChildren
      }
    }

    val filteredAccountTree = state.accState.filterAndClone(reportSettings.isAccountMatching).mkTree
    assert(reportSettings.accountMatch.isDefined || (filteredAccountTree.total equals Zero), "The Account tree doesn't balance!")

    val rightAccNames = settings.reportOptions.isRight
    val (rightAccs, leftAccs) = filteredAccountTree.childStates.partition(at => rightAccNames.contains(at.name.name))

    val leftAmountWidth = maxElseZero(leftAccs.map(_.total.toBigInt.toString.length)) + 5
    val rightAmountWidth = maxElseZero(rightAccs.map(_.total.toBigInt.toString.length)) + 5

    val leftMaxNameLength = maxElseZero(leftAccs.map(_.maxNameLength))
    val rightMaxNameLength = maxElseZero(rightAccs.map(_.maxNameLength))

    val leftRender = leftAccs.sortBy(_.name.toString).flatMap(show(leftAmountWidth, _, leftMaxNameLength))
    val rightRender = rightAccs.sortBy(_.name.toString).flatMap(show(rightAmountWidth, _, rightMaxNameLength))

    val leftTotal = leftAccs.map(_.total).sum
    val rightTotal = rightAccs.map(_.total).sum
    val total = leftTotal + rightTotal
    val totalStr =
      if (total equals Zero) {
        "Zero"
      } else {
        total.toString
      }
    (leftRender, rightRender, "%" + leftAmountWidth + ".2f" format leftTotal, "%" + rightAmountWidth + ".2f = %s" format (rightTotal, totalStr))
  }

  private def formatMonth(monthYear: Int) = {
    val year = monthYear / 100
    val month = monthYear % 100
    s"$year / ${Helper.monthLabels(month - 1)}"
  }

  def registerReport(state: AppState, reportSettings: RegisterReportSettings) = {
    val filteredState = state.accState.filterAndClone(reportSettings.isAccountMatching)

    var amounts = Map[AccountName, BigDecimal]()
    var groupState = new AccountState(amounts, Nil)

    filteredState.txns.groupBy(d => d.date.month + d.date.year * 100).toSeq.sortBy(_._1).map {
      case (month, txnGroup) =>
        val prevAmounts = amounts
        txnGroup foreach { txn =>
          groupState.updateAmount(txn.name, txn.delta, txn.date)
        }
        amounts = groupState.amounts
        val diff = amounts.filter { a =>
          val prevOpt = prevAmounts.get(a._1)
          prevOpt.map(_ != a._2).getOrElse(true)
        }.map { d =>
          val txns = txnGroup.filter(_.name equals d._1)
          (d._1, txns.foldLeft(Zero)(_ + _.delta), d._2, txns)
        }
        val sortedDiff = diff.toSeq.sortBy(_._1.toString)
        (
          formatMonth(month),
          sortedDiff.map(d => RegisterReportEntry(d._4, "%-50s %20.2f %20.2f" format (d._1, d._2, d._3)))
        )
    }
  }

}
