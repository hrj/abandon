package co.uproot.abandon.web

import co.uproot.abandon.{AccountName, AccountTreeState, AppState, Date, Helper}

private object Util {
  val Zero = BigDecimal(0)

  def sumDeltas(s: Seq[WithDelta]) = s.foldLeft(Zero)(_ + _.delta)
  def except[T](a: Seq[T], i: Int) = a.slice(0, i) ++ a.slice(i + 1, a.length)
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
      "children" -> childBalances.map(_.toMap).toArray,
    )
  }
}

object WebAPI {
  def makeReport(startDate: Date, appState: AppState, filterDescription: Option[String]): Array[Char] = {
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

    makeReport(startDate, cookedPosts, accTree)
  }

  private def makeReport(startDate: Date, posts: Seq[CookedPost], accTree: AccountTreeState): Array[Char] = {
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

    def mkBalanceReport(node: AccountTreeState): AccountBalance = {
      val name = node.name
      val details = getDetails(name.fullPathStr)
      val childBalances = node.childStates.map(mkBalanceReport).toArray
      val totalOpening = details.openingBalance + childBalances.map(_.openingBalance).sum
      val totalClosing = details.closingBalance + childBalances.map(_.closingBalance).sum
      val totalDebits = details.debitSubTotal + childBalances.map(_.debitSubTotal).sum
      val totalCredits = details.creditSubTotal + childBalances.map(_.creditSubTotal).sum
      AccountBalance(name, totalOpening, totalClosing, totalDebits, totalCredits, childBalances)
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
      val name = node.name.fullPathStr
      if (name.nonEmpty) {
        accTxnsReport ++= Map(
          name -> Map(
            "name" -> name,
            "txnsByMonth" -> getDetailsByMonth(node.name)
          )
        )
      }
      node.childStates.foreach(updateAccTxnReport)
    }

    val balReport = if (accTree.name.fullPathStr.isEmpty) accTree.childStates.map(mkBalanceReport(_).toMap).toArray else mkBalanceReport(accTree).toMap

    updateAccTxnReport(accTree)

    val fullReport = Map(
      "accountBalances" -> balReport,
      "accountTxns" -> accTxnsReport
    )

    serializeJSON(fullReport)
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
        val maxIndex = array.length - 1
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
      case s: String => escapeAsJSONString(s).toCharArray
      case any: Any => escapeAsJSONString(any.toString).toCharArray
    }
  }

  private def escapeAsJSONString(string: String): String = {
    if (string == null || string.length == 0) return "\"\""
    val len = string.length
    val sb = new StringBuilder(len + 4)
    var t: String = null
    sb.append('"')
    string.foreach(c => {
      c match {
        case '\\' =>
        case '"' =>
          sb.append('\\')
          sb.append(c)

        case '/' =>
          //                if (b == '<') {
          sb.append('\\')
          //                }
          sb.append(c)

        case '\b' =>
          sb.append("\\b")

        case '\t' =>
          sb.append("\\t")

        case '\n' =>
          sb.append("\\n")

        case '\f' =>
          sb.append("\\f")

        case '\r' =>
          sb.append("\\r")

        case _ =>
          if (c < ' ') {
            t = "000" + Integer.toHexString(c)
            sb.append("\\u" + t.substring(t.length - 4))
          }
          else sb.append(c)
      }
    })
    sb.append('"')
    sb.toString
  }
}
