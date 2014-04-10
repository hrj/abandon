package co.uproot.abandon

import Helper.{ Zero, maxElseZero, sumDeltas }

case class BalanceReportEntry(accName: Option[AccountName], render: String)
case class RegisterReportEntry(txns: Seq[DetailedTransaction], render: String)
case class RegisterReportGroup(groupTitle: String, entries: Seq[RegisterReportEntry])

object Reports {

  def balanceReport(state: AppState, settings: Settings, reportSettings: BalanceReportSettings) = {
    def show(width: Int, a: AccountTreeState, maxNameLength: Int, treePrefix: String = "", isLastChild: Boolean = false, isParentLastChild: Boolean = false, prefix: Option[String] = None, forceIndent: Option[Int] = None): Seq[BalanceReportEntry] = {
      val indent = forceIndent.getOrElse(a.name.depth)
      val amountIsZero = a.amount equals Zero
      val hideAccount = (!reportSettings.showZeroAmountAccounts) && amountIsZero
      val renderableChildren = a.childrenNonZero
      val onlyChildren = (renderableChildren == 1) && hideAccount

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
      val selfAmount = if(a.amount !=0 && !a.childStates.isEmpty ) { " (" + a.amount + ")" } else { "" }
      lazy val selfRender = (
        BalanceReportEntry(Some(a.name),
          ("%" + width + ".2f   %-" + maxNameLength + "s") format (
            a.total, myPrefix + (prefix.map(_ + ":").getOrElse("") + a.name.name ) + selfAmount  
          )
        )
      )
      if (renderableChildren == 0) {
        if (hideAccount) {
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

    val filteredAccountTree = state.accState.mkTree(reportSettings.isAccountMatching)
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
  
  def registerReport(state: AppState, reportSettings: RegisterReportSettings) : Seq[RegisterReportGroup] = {
    val txnGroups = state.accState.txnGroups.filter(_.children.exists(c => reportSettings.isAccountMatching(c.name.fullPathStr)))
    val monthlyGroups = txnGroups.groupBy(d => d.date.month + d.date.year * 100).toSeq.sortBy(_._1)

    var reportGroups = Seq[RegisterReportGroup]()
    var groupState = new AccountState()

    monthlyGroups.foreach {
      case (month, monthlyGroup) =>
        monthlyGroup foreach { g =>
          groupState.updateAmounts(g)
        }
        val matchingNames = monthlyGroup.flatMap(_.children.map(_.name)).toSet.filter(name => reportSettings.isAccountMatching(name.fullPathStr))
        val amounts = groupState.amounts
        val matchingAmounts = amounts.filter { case (accountName, amount) => matchingNames.contains(accountName)}

        val totalDeltasPerAccount = matchingAmounts.map { case (accountName, amount) =>
          val myTxns = monthlyGroup.flatMap(_.children).filter(_.name equals accountName)
          val render = "%-50s %20.2f %20.2f" format (accountName, sumDeltas(myTxns), amount)
          (accountName, myTxns, render)
        }

        val sortedTotalDeltasPerAccount = totalDeltasPerAccount.toSeq.sortBy(_._1.toString)

        reportGroups :+= RegisterReportGroup(
          formatMonth(month),
          sortedTotalDeltasPerAccount.map { case (accountName, txns, render) => RegisterReportEntry(txns, render) }
        )
    }
    reportGroups
  }

  def bookReport(state: AppState, reportSettings: BookReportSettings) : Seq[RegisterReportGroup] = {
    val monthlyGroups = state.accState.txnGroups.groupBy(d => d.date.month + d.date.year * 100).toSeq.sortBy(_._1)
    var reportGroups = Seq[RegisterReportGroup]()
    var groupState = new AccountState()

    monthlyGroups.foreach {
      case (month, monthlyGroup) =>
        monthlyGroup foreach { g =>
          groupState.updateAmounts(g)
        }
        val groupAmounts = monthlyGroup.flatMap(_.children.map(_.name)).toSet
        val amounts = groupState.amounts
        val matchingAmounts = amounts.filter { case (accountName, amount) => groupAmounts.contains(accountName)}

        val totalDeltasPerAccount = matchingAmounts.map { case (accountName, amount) =>
          val myTxns = monthlyGroup.flatMap(_.children).filter(_.name equals accountName)
          val render = "%-50s %20.2f %20.2f" format (accountName, myTxns.foldLeft(Zero)(_ + _.delta), amount)
          (accountName, myTxns, render)
        }

        val sortedTotalDeltasPerAccount = totalDeltasPerAccount.toSeq.sortBy(_._1.toString)

        reportGroups :+= RegisterReportGroup(
          formatMonth(month),
          sortedTotalDeltasPerAccount.map { case (accountName, txns, render) => RegisterReportEntry(txns, render) }
        )
    }
    reportGroups
  }
  def otherExport(state: AppState, exportSettings: ExportSettings) : Seq[RegisterReportGroup] = {
    val sortedGroups = state.accState.txnGroups.sortBy(_.date.toInt)
    var reportGroups = Seq[RegisterReportGroup]()
    var groupState = new AccountState()
   sortedGroups.foreach {
    case (monthlyGroup) =>
         sortedGroups foreach { g =>
          groupState.updateAmounts(g)
      }
     }
     val groupAmounts = sortedGroups.flatMap(_.children.map(_.name)).toSet
     val amounts = groupState.amounts
     val matchingAmounts = amounts.filter { case (accountName, amount) => groupAmounts.contains(accountName)}
     val totalDeltasPerAccount = matchingAmounts.map { case (accountName, amount) =>
     val myTxns = sortedGroups.flatMap(_.children).filter(_.name equals accountName)
     val render = "%-50s %20.2f" format (accountName, myTxns.foldLeft(Zero)(_ + _.delta))
          (accountName, myTxns, render)
     }
     val sortedTotalDeltasPerAccount = totalDeltasPerAccount.toSeq.sortBy(_._1.toString)
     var date1 = "";
     sortedGroups.map { txnGroup =>
         date1 = txnGroup.date.formatYYYYMMDD
     } 
     reportGroups :+= RegisterReportGroup(
        date1,
        sortedTotalDeltasPerAccount.map { case (accountName, txns, render) => RegisterReportEntry(txns, render) }
       ) 
    reportGroups
  } 

  def xmlExport(state: AppState, exportSettings: ExportSettings) : xml.Node = {
    val sortedGroups = state.accState.txnGroups.sortBy(_.date.toInt)
    <abandon><transactions>{
      sortedGroups.map {txnGroup =>
        <txnGroup date={txnGroup.date.formatCompact}>
          { txnGroup.payeeOpt.map(payee => <payee>{payee}</payee>).getOrElse(xml.Null)}
          { txnGroup.annotationOpt.map(annotation => <annotation>{annotation}</annotation>).getOrElse(xml.Null)}
          { txnGroup.groupComments.map{comment => <comment>{comment}</comment>} }
          { txnGroup.children.map(txn=>
              <txn name={txn.name.fullPathStr} delta={txn.delta.toString}>{
                 txn.commentOpt.map{comment => <comment>{comment}</comment>}.getOrElse(xml.Null)
              }</txn>
            )
          }
        </txnGroup>
      }
    }</transactions></abandon>
  } 
}