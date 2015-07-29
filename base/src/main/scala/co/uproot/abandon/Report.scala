package co.uproot.abandon

import Helper.{ Zero, maxElseZero, sumDeltas }

case class BalanceReportEntry(accName: Option[AccountName], render: String)
case class RegisterReportEntry(txns: Seq[DetailedPost], render: String)
case class RegisterReportGroup(groupTitle: String, entries: Seq[RegisterReportEntry])
case class LedgerExportEntry(accountName: AccountName, amount: BigDecimal)
case class LedgerExportData(date: Date, ledgerEntries: Seq[LedgerExportEntry]) {
  val maxNameLength = maxElseZero(ledgerEntries.map(_.accountName.toString.length))
  val maxAmountWidth = maxElseZero(ledgerEntries.map(_.amount.toString.length))
}

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
            if (onlyChildren) Some(indent) else None)
      }
      val selfAmount = if (a.amount != 0 && !a.childStates.isEmpty) { " (" + a.amount + ")" } else { "" }
      lazy val selfRender =
        BalanceReportEntry(Some(a.name),
          ("%" + width + ".2f   %-" + maxNameLength + "s") format (
            a.total, myPrefix + (prefix.map(_ + ":").getOrElse("") + a.name.name) + selfAmount))

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

  def registerReport(state: AppState, reportSettings: RegisterReportSettings): Seq[RegisterReportGroup] = {
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
        val matchingAmounts = amounts.filter { case (accountName, amount) => matchingNames.contains(accountName) }

        val totalDeltasPerAccount = matchingAmounts.map {
          case (accountName, amount) =>
            val myTxns = monthlyGroup.flatMap(_.children).filter(_.name equals accountName)
            val render = "%-50s %20.2f %20.2f" format (accountName, sumDeltas(myTxns), amount)
            (accountName, myTxns, render)
        }

        val sortedTotalDeltasPerAccount = totalDeltasPerAccount.toSeq.sortBy(_._1.toString)

        reportGroups :+= RegisterReportGroup(
          formatMonth(month),
          sortedTotalDeltasPerAccount.map { case (accountName, txns, render) => RegisterReportEntry(txns, render) })
    }
    reportGroups
  }

  def bookReport(state: AppState, reportSettings: BookReportSettings): Seq[RegisterReportGroup] = {
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
        val matchingAmounts = amounts.filter { case (accountName, amount) => groupAmounts.contains(accountName) }

        val totalDeltasPerAccount = matchingAmounts.map {
          case (accountName, amount) =>
            val myTxns = monthlyGroup.flatMap(_.children).filter(_.name equals accountName)
            val render = "%-50s %20.2f %20.2f" format (accountName, myTxns.foldLeft(Zero)(_ + _.delta), amount)
            (accountName, myTxns, render)
        }

        val sortedTotalDeltasPerAccount = totalDeltasPerAccount.toSeq.sortBy(_._1.toString)

        reportGroups :+= RegisterReportGroup(
          formatMonth(month),
          sortedTotalDeltasPerAccount.map { case (accountName, txns, render) => RegisterReportEntry(txns, render) })
    }
    reportGroups
  }

  /** Checks whether there are duplicate entries in closure sources.
    * Throws an exception when a duplicate is found
    */
  private def checkSourceNames(closures: Seq[ClosureExportSettings], accountNames: Seq[String]) {
    var uniqueNames = Set[String]()
    closures foreach { closure =>
      val srcEntries = accountNames.filter { name => closure.sources.exists(name matches _) }
      srcEntries foreach { srcName =>
        if (uniqueNames.contains(srcName)) {
          throw new InputError("Found duplicate source entry in closures: " + srcName)
        } else {
          uniqueNames += srcName
        }
      }
    }
  }

  /** Returns a Seq of LedgerExportData
    * Each instance of LedgerExportData represents a transaction.
    * If there are no transactions, this function returns Nil
    * If a separate closure transaction is requested, this function returns two instances of LedgerExportData.
    * Else, a single instance of LedgerExportData is returned.
    */
  def ledgerExport(state: AppState, settings: Settings, reportSettings: LedgerExportSettings): Seq[LedgerExportData] = {
    if (reportSettings.accountMatch.isDefined) {
      throw new NotImplementedError("Filtering of accounts is not yet implemented in ledger export. See https://github.com/hrj/abandon/issues/11")
    }
    val sortedGroup = state.accState.txnGroups.sortBy(_.date.toInt)
    if (sortedGroup.isEmpty) {
      Nil
    } else {
      val latestDate = sortedGroup.last.date
      val accAmounts = state.accState.amounts.toSeq
      val amounts =
        if (reportSettings.showZeroAmountAccounts) {
          accAmounts
        } else {
          accAmounts.filter(_._2 != Zero)
        }
      val entries = amounts.map {
        case (accountName, amount) => LedgerExportEntry(accountName, amount)
      }
      val sortedByName = entries.sortBy(_.accountName.toString)
      val balanceEntry = LedgerExportData(latestDate, sortedByName)

      checkSourceNames(reportSettings.closure, amounts.map(_._1.fullPathStr))

      val closureEntries = reportSettings.closure map { closure =>
        val srcEntries = amounts.filter { name => closure.sources.exists(name._1.fullPathStr matches _) }
        val srcClosure = srcEntries.map {
          case (accountName, amount) => LedgerExportEntry(accountName, -amount)
        }
        val srcClosureSorted = srcClosure.sortBy(_.accountName.toString)

        val destEntry =
          amounts.find { case (name, amount) => name.fullPathStr == closure.destination } match {
            case Some(entry) => entry
            case None =>
              val message = s"While exporting to ledger formt, didn't find a matching destination account named: ${closure.destination}"
              throw new MissingDestinationError(message)
          }
        if (srcEntries contains destEntry) {
          val message = s"Destination clashed with one of the sources: $destEntry"
          throw new SourceDestinationClashError(message)
        } else {
          val destClosure = destEntry match {
            case (accountName, amount) =>
              val srcTotal = srcClosure.map(_.amount).sum
              LedgerExportEntry(accountName, -(srcTotal))
          }
          LedgerExportData(latestDate, srcClosureSorted :+ destClosure)
        }

      }
      balanceEntry +: closureEntries
    }
  }

  def xmlExport(state: AppState, exportSettings: ExportSettings): xml.Node = {
    <abandon><transactions>{
      val sortedGroups = state.accState.txnGroups.sortBy(_.date.toInt)
      sortedGroups.map { txnGroup =>
        <txnGroup date={ txnGroup.date.formatCompact }>
          { txnGroup.payeeOpt.map(payee => <payee>{ payee }</payee>).getOrElse(xml.Null) }
          { txnGroup.annotationOpt.map(annotation => <annotation>{ annotation }</annotation>).getOrElse(xml.Null) }
          { txnGroup.groupComments.map { comment => <comment>{ comment }</comment> } }
          {
            txnGroup.children.map(txn =>
              <txn name={ txn.name.fullPathStr } delta={ txn.delta.toString }>{
                txn.commentOpt.map { comment => <comment>{ comment }</comment> }.getOrElse(xml.Null)
              }</txn>)
          }
        </txnGroup>
      }
    }</transactions></abandon>
  }
}
