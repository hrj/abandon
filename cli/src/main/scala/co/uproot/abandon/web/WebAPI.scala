package co.uproot.abandon.web

import co.uproot.abandon.{AccountName, AccountTreeState, AppState, Date, Helper}

import java.text.DecimalFormat

private object Util {
  val Zero: BigDecimal = BigDecimal(0)

  def sumDeltas(s: Seq[WithDelta]): BigDecimal = s.foldLeft(Zero)(_ + _.delta)
  def except[T](a: Seq[T], i: Int): Seq[T] = a.slice(0, i) ++ a.slice(i + 1, a.length)
}

import  Util._

case class AccountDetails(name: AccountName, openingBalance: BigDecimal, closingBalance: BigDecimal, debitSubTotal: BigDecimal, creditSubTotal: BigDecimal, posts: Seq[CookedPost])

// case class AccountSemiDetails(name: String, debitSubTotal: Option[BigDecimal], creditSubTotal: Option[BigDecimal])

case class AccountBalance(name: AccountName, openingBalance: BigDecimal, closingBalance: BigDecimal, debitSubTotal: BigDecimal, creditSubTotal: BigDecimal, childBalances: Array[AccountBalance]) {
  def toMap: Map[String, Any] = {
    Map(
      "name" -> name.name,
      "opening" -> openingBalance,
      "closing" -> closingBalance,
      "debit" -> debitSubTotal,
      "credit" -> creditSubTotal,
      "children" -> childBalances.map(_.toMap),
    )
  }
}

object WebAPI {
  def makeReport(startDate: Date, appState: AppState, filterDescription: Option[String], showInactiveAccounts: Boolean = false): Array[Char] = {
    // TODO: show filterDescription in output
    val accState = appState.accState
    // TODO: Apply filter from settings, if not already applied
    val accTree = accState.mkTree(_ => true)

    val txns = accState.postGroups.map(pg => {
      val txnPosts = pg.children.map(cp => {
        Post(cp.name.fullPathStr, cp.delta, cp.commentOpt)
      })
      val txn = Transaction(pg.date, pg.annotationOpt, pg.payeeOpt, pg.groupComments, txnPosts)
      txn.updateTxnRefs()
      txn
    })
    val cookedPosts = txns.flatMap(_.cookPosts)

    makeReport(startDate, cookedPosts, accTree, showInactiveAccounts)
  }

  private def makeReport(startDate: Date, posts: Seq[CookedPost], accTree: AccountTreeState, showInactiveAccounts: Boolean): Array[Char] = {
    val (currPosts, prevPosts) = posts.partition(_.date.toInt >= startDate.toInt)
    val allAccountNames = posts.map(_.name).toSet
    val sortedAllAccountNames = allAccountNames.toSeq.sorted
    val groupedPosts = currPosts.groupBy(_.name)
    val openingBalances = prevPosts.groupBy(_.name).map { case (name, gPosts) => name -> sumDeltas(gPosts) }
    val closingBalances = sortedAllAccountNames.map(name => {
      val openingBalance = openingBalances.getOrElse(name, Zero)
      val gposts = groupedPosts.getOrElse(name, Nil)
      val closing = openingBalance + sumDeltas(gposts)
      name -> closing
    }).toMap

    def getDetails(name: String, filterPost: CookedPost => Boolean = _ => true) = {
      val gPosts = groupedPosts.getOrElse(name, Nil).filter(filterPost)
      val (debitPosts, creditPosts) = gPosts.partition(_.delta < Zero)
      val openingBalance = openingBalances.getOrElse(name, Zero)
      val closingBalance = closingBalances.getOrElse(name, Zero)
      val debitSubTotal = sumDeltas(debitPosts)
      val creditSubTotal = sumDeltas(creditPosts)
      val fullPath = name.split(':')
      AccountDetails(AccountName(fullPath), openingBalance, closingBalance, debitSubTotal, creditSubTotal, gPosts)
    }

    // Determine active accounts: An account is active if it has transactions in the period,
    // or has a non-zero opening balance.
    val directlyActiveAccounts = sortedAllAccountNames.filter { name =>
      val details = getDetails(name)
      details.openingBalance != Zero || details.posts.nonEmpty
    }.toSet

    // An account is also active if any of its descendants are directly active.
    def isActive(name: String): Boolean = {
      name.isEmpty || directlyActiveAccounts.exists(acc => acc == name || acc.startsWith(name + ":"))
    }

    def mkBalanceReport(node: AccountTreeState): Option[AccountBalance] = {
      val name = node.name
      val nameStr = name.fullPathStr
      val isNodeActive = isActive(nameStr)

      if (!showInactiveAccounts && nameStr.nonEmpty && !isNodeActive) {
        None
      } else {
        val details = getDetails(nameStr)
        val childBalances = node.childStates.flatMap(mkBalanceReport).toArray
        val totalOpening = details.openingBalance + childBalances.map(_.openingBalance).sum
        val totalClosing = details.closingBalance + childBalances.map(_.closingBalance).sum
        val totalDebits = details.debitSubTotal + childBalances.map(_.debitSubTotal).sum
        val totalCredits = details.creditSubTotal + childBalances.map(_.creditSubTotal).sum
        Some(AccountBalance(name, totalOpening, totalClosing, totalDebits, totalCredits, childBalances))
      }
    }

    var accTxnsReport = Map[String, Any]()

    def getDetailsByMonth(accName: AccountName): Array[Map[String, Any]] = {
      val name = accName.fullPathStr
      val gPosts = currPosts.filter(_.name == name)
      val openingBalance = openingBalances.getOrElse(name, Zero)
      val closingBalance = openingBalance + sumDeltas(gPosts)
      val months = gPosts.map(p => Date(p.date.year, p.date.month, 1)).toSet.toArray
      var prevMonthClosingBalance = openingBalance

      def txnsToMap(posts: Seq[CookedPost], openingBalance: BigDecimal) = {
        var currBalance = openingBalance
        posts.map(p =>
          val balanceAfterPost = currBalance + p.delta
          currBalance = balanceAfterPost
          Map(
            "date" -> p.date.formatYYYYMMDD,
            "indeterminate" -> p.isComplex,
            "comments" -> p.comments.toArray,
            "selfPost" -> Map(
              "debit" -> (p.delta < Zero),
              "delta" -> p.delta,
              "balance" -> balanceAfterPost
            ),
            "posts" -> (p.oppositeOthers ++ p.similarOthers).map(o =>
              Map(
                "debit" -> (o.delta < Zero),
                "delta" -> o.delta,
                "name" -> o.name,
                "comments" -> o.comments.toArray
              )
            ).toArray
          )
        ).toArray
      }

      def getDetailsForMonth(mDate: Date): Map[String, Any] = {
        val rawDetails = getDetails(name, p => p.date.year == mDate.year && p.date.month == mDate.month)
        val monthClosingBalance = prevMonthClosingBalance + rawDetails.debitSubTotal + rawDetails.creditSubTotal
        val monthOpeningBalance = prevMonthClosingBalance
        val debits = rawDetails.debitSubTotal
        val credits = rawDetails.creditSubTotal
        prevMonthClosingBalance = monthClosingBalance

        Map(
          "name" -> s"${Helper.getShortMonth(mDate.month)} ${mDate.year}",
          "opening" -> monthOpeningBalance,
          "closing" -> monthClosingBalance,
          "debit" -> debits,
          "credit" -> credits,
          "txns" -> txnsToMap(rawDetails.posts, monthOpeningBalance)
        )
      }

      val result = months.sortBy(_.toInt).map(getDetailsForMonth)

      assert(prevMonthClosingBalance == closingBalance, s"prev monthly closing bal = $prevMonthClosingBalance, acc closing: $closingBalance")

      result
    }

    def updateAccTxnReport(node: AccountTreeState): Unit = {
      val nameStr = node.name.fullPathStr
      if (nameStr.nonEmpty && (showInactiveAccounts || isActive(nameStr))) {
        accTxnsReport ++= Map(
          nameStr -> Map(
            "name" -> nameStr,
            "txnsByMonth" -> getDetailsByMonth(node.name)
          )
        )
      }
      node.childStates.foreach(updateAccTxnReport)
    }

    val balReport = if (accTree.name.fullPathStr.isEmpty) {
      accTree.childStates.flatMap(mkBalanceReport).map(_.toMap).toArray
    } else {
      mkBalanceReport(accTree).map(_.toMap).getOrElse(Map[String, Any]())
    }

    updateAccTxnReport(accTree)

    def makeMonthlyRegister(): Array[Map[String, Any]] = {
      val months = currPosts.map(p => Date(p.date.year, p.date.month, 1)).toSet.toArray

      months.sortBy(_.toInt).map(mDate =>
        val currMonthPosts = currPosts.filter(p => p.date.month == mDate.month && p.date.year == mDate.year)
        val closingBalance = sumDeltas(currMonthPosts)
        assert(closingBalance == Zero)

        val (debitPosts, creditPosts) = currMonthPosts.partition(_.delta < Zero)
        val debitSubTotal = sumDeltas(debitPosts)
        val creditSubTotal = sumDeltas(creditPosts)

        val accountSummaries = currMonthPosts.groupBy(_.name).toArray.sortBy(_._1).map((name, posts) =>
          val priorPosts = currPosts.filter(p => p.date.toIntYYYYMM < mDate.toIntYYYYMM && p.name == name)
          val accOpeningBalance = openingBalances.getOrElse(name, Zero) + sumDeltas(priorPosts)
          val (accDebitPosts, accCreditPosts) = posts.partition(_.delta < Zero)
          val accDebitSubTotal = sumDeltas(accDebitPosts)
          val accCreditSubTotal = sumDeltas(accCreditPosts)
          val accClosingBalance = accOpeningBalance + accDebitSubTotal + accCreditSubTotal
          Map(
            "name" -> name,
            "opening" -> accOpeningBalance,
            "debit" -> accDebitSubTotal,
            "credit" -> accCreditSubTotal,
            "closing" -> accClosingBalance,
          )
        )
        Map(
            "month" -> s"${Helper.getShortMonth(mDate.month)} ${mDate.year}",
            "opening" -> Zero,
            "closing" -> closingBalance,
            "debit" -> debitSubTotal,
            "credit" -> creditSubTotal,
            "accountSummaries" -> accountSummaries
          )
        )
    }

    val fullReport = Map(
      "accountBalances" -> balReport,
      "monthlySummaries" -> makeMonthlyRegister(),
      "accountTxns" -> accTxnsReport
    )

    JsonUtils.serializeJSON(fullReport)
  }
}
