package co.uproot.abandon.web

import co.uproot.abandon.{AccountName, AccountTreeState, AppState, Date, JournalExportSettings}

import java.io.FileWriter

object Util {
  val Zero = BigDecimal(0)

  def sumDeltas(s: Seq[WithDelta]) = s.foldLeft(Zero)(_ + _.delta)
  def except[T](a: Seq[T], i: Int) = a.slice(0, i) ++ a.slice(i + 1, a.length)
}

import  Util._

case class AccountDetails(name: AccountName, openingBalance: BigDecimal, closingBalance: BigDecimal, debitSubTotal: BigDecimal, creditSubTotal: BigDecimal, posts: Seq[CookedPost])

case class AccountSemiDetails(name: String, debitSubTotal: Option[BigDecimal], creditSubTotal: Option[BigDecimal])

object WebAPI {
  def execute(jes: JournalExportSettings, startDate: Date, appState: AppState, filterDescription: Option[String]): Unit = {
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

    execute(startDate, cookedPosts, accTree)
  }

  private def execute(startDate: Date, posts: Seq[CookedPost], accTree: AccountTreeState): Unit = {
    val (currPosts, prevPosts) = posts.partition(_.date.toInt >= startDate.toInt)
    val allAccountNames = posts.map(_.name).toSet
    val sortedAllAccountNames = allAccountNames.toSeq.sorted
    val groupedPosts = currPosts.groupBy(_.name)
    val openingBalances = prevPosts.groupBy(_.name).map { case (name, gPosts) => (name -> sumDeltas(gPosts)) }
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

    def mkBalanceReport(node: AccountTreeState): Map[String, Any] = {
      val name = node.name
      val details = getDetails(name.fullPathStr)
      Map(
        "name" -> name.name,
        "opening" -> details.openingBalance,
        "closing" -> details.closingBalance,
        "debit" -> details.debitSubTotal,
        "credit" -> details.creditSubTotal,
        "children" -> node.childStates.map(mkBalanceReport).toArray,
      )
    }

    var accTxnsReport = Map[String, Any]()

    def getDetailsByMonth(accName: AccountName): Map[String, Any] = {
      val name = accName.fullPathStr
      val gPosts = currPosts.filter(_.name == name)
      val openingBalance = openingBalances.getOrElse(name, Zero)
      val closingBalance = openingBalance + sumDeltas(gPosts)
      val months = gPosts.map(p => Date(p.date.year, p.date.month, 1)).toSet.toArray
      var allDetails = Map[String, Any]()
      var prevMonthClosingBalance = openingBalance

      def txnsToMap(posts: Seq[CookedPost], openingBalance: BigDecimal) = {
        var currBalance = openingBalance
        posts.map(p =>
          val balanceAfterPost = currBalance + p.delta
          currBalance = balanceAfterPost
          Map(
            "date" -> p.date.formatYYYYMMDD,
            "indeterminate" -> p.isComplex,
            "selfPost" -> Map(
              "debit" -> (p.delta < Zero),
              "delta" -> p.delta,
              "balance" -> balanceAfterPost
            ),
            "posts" -> (p.oppositeOthers ++ p.similarOthers).map(o =>
              Map(
                "debit" -> (o.delta < Zero),
                "delta" -> o.delta,
                "name" -> o.name
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
          "opening" -> monthOpeningBalance,
          "closing" -> monthClosingBalance,
          "debit" -> debits,
          "credit" -> credits,
          "txns" -> txnsToMap(rawDetails.posts, monthOpeningBalance)
        )
      }

      months.sortBy(_.toInt).foreach(mDate =>
        allDetails += s"${mDate.year} / ${mDate.month}" -> getDetailsForMonth(mDate)
      )
      assert(prevMonthClosingBalance == closingBalance, s"prev monthly closing bal = $prevMonthClosingBalance, acc closing: $closingBalance")
      allDetails
    }

    def updateAccTxnReport(node: AccountTreeState): Unit = {
      val name = node.name.fullPathStr
      if (name.nonEmpty) {
        val txnsByMonth = getDetailsByMonth(node.name)
        accTxnsReport ++= Map(
          name -> Map(
            "name" -> name,
            "txnsByMonth" -> txnsByMonth
          )
        )
      }
      node.childStates.foreach(updateAccTxnReport)
    }

    val balReport = if (accTree.name.fullPathStr.isEmpty) accTree.childStates.map(mkBalanceReport).toArray else mkBalanceReport(accTree)

    updateAccTxnReport(accTree)

    val fullReport = Map(
      "accountBalances" -> balReport,
      "accountTxns" -> accTxnsReport
    )

    val writer = FileWriter("web.json")
    writer.write(serializeJSON(fullReport))
    writer.close()

  }

  private def serializeJSON(any: Any): Array[Char] = {
    any match {
      case map: Map[String, Any] =>
        val sb = StringBuilder(1024)
        val keys = map.keys.toList
        val maxIndex = keys.size - 1
        sb.append('{')
        keys.zipWithIndex.foreach(ki => {
          val key = ki._1
          val index = ki._2
          val value = map(key)
          sb.append('"')
          sb.append(key)
          sb.append('"')
          sb.append(':')
          sb.appendAll(serializeJSON(value))
          if (index != maxIndex) {
            sb.append(',')
          }
        })
        sb.append('}')
        sb.toCharArray
      case array: Array[Any] =>
        val sb = StringBuilder(1024)
        val maxIndex = array.size - 1
        // test
        sb.append('[')
        array.zipWithIndex.foreach(vi => {
          val v = vi._1
          val index = vi._2
          sb.appendAll(serializeJSON(v))
          if (index != maxIndex) {
            sb.append(',')
          }
        })
        sb.append(']')
        sb.toCharArray
      case b: Boolean => ("" + b).toCharArray
      case s: String => ('"' + s + '"').toCharArray
      case any: Any => ('"' + any.toString + '"').toCharArray
    }
  }
}
