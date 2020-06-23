package co.uproot.abandon

import co.uproot.abandon.Helper.{Zero, maxElseZero, sumDeltas}

import scala.xml.Elem

case class BalanceReportEntry(accName: Option[AccountName], render: String)
case class BalanceReport(leftEntries:Seq[BalanceReportEntry], rightEntries:Seq[BalanceReportEntry], totalLeft:String, totalRight:String)

case class RegisterReportEntry(accountName: String, txns: Seq[DetailedPost], render: String)
case class RegisterReportGroup(groupTitle: String, entries: Seq[RegisterReportEntry])

case class LedgerExportEntry(accountName: AccountName, amount: BigDecimal, comment: Option[String] = None)
case class LedgerExportData(date: Date, ledgerEntries: Seq[LedgerExportEntry]) {
  val maxNameLength = maxElseZero(ledgerEntries.map(_.accountName.toString.length))
  val maxAmountWidth = maxElseZero(ledgerEntries.map(_.amount.toString.length))
}

case class ClosureAccumulator(amounts: Seq[(AccountName, BigDecimal)], closureEntries: List[LedgerExportData])

object Reports {

  def balanceReport(state: AppState, settings: Settings, reportSettings: BalanceReportSettings):BalanceReport = {
    def show(width: Int, a: AccountTreeState, maxNameLength: Int, treePrefix: String = "", isLastChild: Boolean = false, isParentLastChild: Boolean = false, prefix: Option[String] = None, forceIndent: Option[Int] = None): Seq[BalanceReportEntry] = {
      val indent = forceIndent.getOrElse(a.name.depth)
      val amountIsZero = a.amount equals Zero
      val hideAccount = (!reportSettings.showZeroAmountAccounts) && amountIsZero
      val renderableChildren = a.childrenNonZero
      val onlyChildren = (renderableChildren.length == 1) && hideAccount

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

      val children = a.childStates.sortBy(_.name.toString)
      val lastRenderable =
        if (!hideAccount) children.lastOption
        else renderableChildren.sortBy(_.name.toString).lastOption
      val renderedChildren = children.flatMap { c =>
          show(width, c, maxNameLength, childTreePrefix, lastRenderable.contains(c), isLastChild || (prefix.isDefined && isParentLastChild),
            if (onlyChildren) Some(prefix.map(_ + ":").getOrElse("") + a.name.name) else None,
            if (onlyChildren) Some(indent) else None)
      }
      val selfAmount = if (!amountIsZero && a.childStates.nonEmpty) { " (" + a.amount + ")" } else { "" }
      lazy val selfRender =
        BalanceReportEntry(Some(a.name),
          ("%" + width + ".2f   %-" + maxNameLength + "s") format (
            a.total, myPrefix + (prefix.map(_ + ":").getOrElse("") + a.name.name) + selfAmount))

      if (renderableChildren.isEmpty) {
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
    BalanceReport(leftRender, rightRender, "%" + leftAmountWidth + ".2f" format leftTotal, "%" + rightAmountWidth + ".2f = %s" format (rightTotal, totalStr))
  }


  /**
    * Formats int date used for grouping to text, based on it's resolution
    *
    * @param groupingDate
    * @return Date as string with correct resolution
    */
  private def formatGroupingDate(groupingDate: Int) = {
    val date = Date.fromInt(groupingDate)
    if (date.hasDayResolution) {
      date.formatISO8601Ext
    }
    else if (date.hasMonthResolution) {
      s"${date.year} / ${Helper.monthLabels(date.month - 1)}"
    }
    else {
      s"${date.year}"
    }
  }

  def registerReport(state: AppState, reportSettings: RegisterReportSettings): Seq[RegisterReportGroup] = {
    val txns = state.accState.postGroups.filter(_.children.exists(c => reportSettings.isAccountMatching(c.name.fullPathStr)))

    /**
      * Grouping is done by truncated int numbers. The end result (groups) is
      * Seq[(String, Seq[PostGroup])] so we need mapping from truncated intDate -> String
      * For dates this fairly trivial, but for iso weeks and ISO week dates it gets complicated.
      * For this reason, for now, let's use two different groupBy operators: Int and String based
      *
      * With groupStrOp we can group by non-trivial dates, e.g. by ISO week dates,
      * and we don't have to convert the result, because it is already String
      */
    val txnGroups: Seq[(String, Seq[PostGroup])] = reportSettings.groupOp match {
      case Left(groupIntOp) => {
        txns.groupBy(groupIntOp).toSeq.sortBy(_._1).map({
          // groupIntOp result (Int, Seq) has to be converted to (String, Seq)
          case (intDate, groups) => (formatGroupingDate(intDate), groups)
        })
      }
      case Right(groupTxtOp) => {
        // this is (String, Seq) already
        txns.groupBy(groupTxtOp).toSeq.sortBy(_._1)
      }
    }

    var reportGroups = Seq[RegisterReportGroup]()
    var groupState = new AccountState()

    txnGroups.foreach {
      case (groupName, txnGroup) =>
        txnGroup foreach { g =>
          groupState.updateAmounts(g)
        }
        val matchingNames = txnGroup.flatMap(_.children.map(_.name)).toSet.filter(name => reportSettings.isAccountMatching(name.fullPathStr))
        val amounts = groupState.amounts
        val matchingAmounts = amounts.filter { case (accountName, amount) => matchingNames.contains(accountName) }

        val totalDeltasPerAccount = matchingAmounts.map {
          case (accountName, amount) =>
            val myPostings = txnGroup.flatMap(_.children).filter(_.name equals accountName)
            val render = "%-50s %20.2f %20.2f" format (accountName, sumDeltas(myPostings), amount)
            RegisterReportEntry(accountName.toString, myPostings, render)
        }

        val sortedTotalDeltasPerAccount = totalDeltasPerAccount.toSeq.sortBy(_.accountName)

        reportGroups :+= RegisterReportGroup(groupName, sortedTotalDeltasPerAccount)
    }
    reportGroups
  }

  def bookReport(state: AppState, reportSettings: BookReportSettings): Seq[RegisterReportGroup] = {
    val monthlyGroups = state.accState.postGroups.groupBy(d => d.date.toIntYYYYMM).toSeq.sortBy(_._1)
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
            RegisterReportEntry(accountName.toString, myTxns, render)
        }

        val sortedTotalDeltasPerAccount = totalDeltasPerAccount.toSeq.sortBy(_.accountName)

        reportGroups :+= RegisterReportGroup(formatGroupingDate(month), sortedTotalDeltasPerAccount)
    }
    reportGroups
  }

  /** Checks whether there are duplicate entries in closure sources.
    * Throws an exception when a duplicate is found
    */
  private def checkSourceNames(closures: Seq[ClosureExportSettings], accountNames: Seq[String]) = {
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
  def balanceExport(state: AppState, settings: Settings, reportSettings: BalanceExportSettings): Seq[LedgerExportData] = {
    if (reportSettings.accountMatch.isDefined) {
      throw new NotImplementedError("Filtering of accounts is not yet implemented in balance export. See https://github.com/hrj/abandon/issues/11")
    }
    val sortedGroup = state.accState.postGroups.sortBy(_.date.toInt)
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

      val closureAcc = reportSettings.closure.foldLeft(ClosureAccumulator(amounts, Nil)) { (acc, e) =>
        mkClosure(acc, latestDate, e)
      }
      balanceEntry +: closureAcc.closureEntries
    }
  }

  def mkClosure(accumulator: ClosureAccumulator, latestDate: Date, closure: ClosureExportSettings): ClosureAccumulator = {
    val amounts = accumulator.amounts
    val (srcEntries, otherEntries) = amounts.partition { name => closure.sources.exists(name._1.fullPathStr matches _) }
    val destEntry =
      amounts.find { case (name, amount) => name.fullPathStr == closure.destination } match {
        case Some(entry) => entry
        case None => (ASTHelper.parseAccountName(closure.destination), Zero)
      }
    if (srcEntries contains destEntry) {
      val message = s"Destination clashed with one of the sources: $destEntry"
      throw new SourceDestinationClashError(message)
    } else {
      val srcClosure = srcEntries.map {
        case (accountName, amount) => LedgerExportEntry(accountName, -amount)
      }
      val srcClosureSorted = srcClosure.sortBy(_.accountName.toString)

      val (destClosure, destResult) = destEntry match {
        case (accountName, amount) =>
          val srcTotalNeg = srcClosure.map(_.amount).sum
          val srcTotal = -srcTotalNeg
          val resultAmt = srcTotal + amount
          (LedgerExportEntry(accountName, srcTotal, Some(" result: " + resultAmt.toString)), resultAmt)
      }
      ClosureAccumulator(
        otherEntries :+ (destClosure.accountName, destResult),
        accumulator.closureEntries :+ LedgerExportData(latestDate, srcClosureSorted :+ destClosure))
    }

  }

  def xmlBalanceExport(state: AppState, exportSettings: BalanceExportSettings, filterXML: Option[xml.Node]): xml.Node = {
    val balance: Elem =
      <abandon>
        {
        filterXML match {
          case Some(xml) => {
            <info>
              { xml }
            </info>
          }
          case None => ;
        }
        }
        <balance>
          {state.accState.mkTree(exportSettings.isAccountMatching).toXML}
        </balance>
      </abandon>

    exportSettings.appVersion match {
      case Some(version) => addAttribute(balance, "version", version.id)
      case None => balance
    }
  }

  def xmlJournalExport(state: AppState, exportSettings: JournalExportSettings, filterXML: Option[xml.Node]): xml.Node = {
    val journal: Elem =
      <abandon>
        {
        filterXML match {
          case Some(xml) => {
            <info>
              { xml }
            </info>
          }
          case None => ;
        }
        }
        <journal>
          <transactions>{
            val sortedGroups = state.accState.postGroups.sortBy(_.date.toInt)
            sortedGroups.map { txnGroup =>
              <txn date={ txnGroup.date.formatISO8601Ext }>
                { txnGroup.payeeOpt.map(payee => <payee>{ payee }</payee>).getOrElse(xml.Null) }
                { txnGroup.annotationOpt.map(annotation => <annotation>{ annotation }</annotation>).getOrElse(xml.Null) }
                { txnGroup.groupComments.map { comment => <comment>{ comment }</comment> } }
                {
                txnGroup.children.map(txn =>
                  <post delta={ txn.delta.toString } name={ txn.name.fullPathStr }>{
                    txn.commentOpt.map { comment => <comment>{ comment }</comment> }.getOrElse(xml.Null)
                    }</post>)
                }
              </txn>
            }
            }</transactions>
        </journal>
      </abandon>

    exportSettings.appVersion match {
      case Some(version) => addAttribute(journal, "version", version.id)
      case None => journal
    }
  }

  def addAttribute(n: Elem, k: String, v: String) = n % new xml.UnprefixedAttribute(k, v, xml.Null)

  def xmlExport(state: AppState, exportSettings: ExportSettings, txnFilters: Option[TxnFilterStack]): xml.Node = {
    val filterXML: Option[xml.Node] = txnFilters match {
      case Some(filters) => Option(filters.xmlDescription)
      case None => None
    }

    exportSettings match {
      case jes: JournalExportSettings => xmlJournalExport(state, jes, filterXML)
      case bes: BalanceExportSettings => xmlBalanceExport(state, bes, filterXML)
    }
  }
}
