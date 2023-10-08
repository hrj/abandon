package co.uproot.abandon.web

import co.uproot.abandon.Date
import co.uproot.abandon.web.Util.{except, sumDeltas, Zero}

trait WithDelta {
  val delta: BigDecimal
}

/** A transaction as parsed from input */
case class Transaction(
                        date: Date,
                        annotationOpt: Option[String],
                        payeeOpt: Option[String],
                        comments: Seq[String],
                        posts: Seq[Post]) {

  private def splitPostsByCount = {
    val (posPosts, negPosts) = posts.partition(_.delta > Zero)
    if (posPosts.length > negPosts.length) {
      (posPosts, negPosts)
    } else {
      (negPosts, posPosts)
    }
  }

  def demuxPosts: Transaction = {
    val (majorityPosts, minorityPosts) = splitPostsByCount
    val minorityAccountNames = minorityPosts.map(_.name).toSet
    // assert(minorityAccountNames.size == 1, "Currently, minority Post can only be from one account. MinorityAccountNames: " + minorityAccountNames.mkString(", "))
    if (majorityPosts.length == minorityPosts.length || minorityAccountNames.size != 1) {
      // Demux not required
      this
    } else {
      // Demux possible
      assert(minorityPosts.length == 1, "Currently, only one minority Post supported")
      val minorTxn = minorityPosts.head
      val demuxedPosts = majorityPosts.map(major => Post(minorTxn.name, -major.delta, minorTxn.commentOpt))
      copy(posts = majorityPosts ++ demuxedPosts)
    }
  }

  def cookPosts: Seq[CookedPost] = {
    val (majorityPosts, minorityPosts) = splitPostsByCount
    val (majorityCount, minorityCount) = (majorityPosts.length, minorityPosts.length)
    if (majorityCount == 1 && minorityCount == 1) {
      // Simple case
      posts.map { _.simpleCook }
    } else {
      val majorityAccountNames = majorityPosts.map(_.name).toSet
      if (minorityCount == 1 && majorityAccountNames.size == 1) {
        // Needs simple demux
        val minorityCookedTxn = minorityPosts.head.simpleCook
        val majorityAccountName = majorityAccountNames.head
        val oppOtherPosts = majorityPosts.map { mt =>
          CookedOtherPost(minorityCookedTxn.name, -mt.delta, mt.splitComments)
        }
        val majorityCookedTxn = CookedPost(date, majorityAccountName, sumDeltas(majorityPosts), comments, oppOtherPosts, Nil)
        Seq(majorityCookedTxn, minorityCookedTxn)
      } else {
        posts.map { _.simpleCook }
      }
    }
  }

  def updateTxnRefs(): Unit = {
    posts.zipWithIndex.foreach {
      case (post, idx) =>
        post.other = except(posts, idx)
        post.txn = this
    }
  }
}

/** A post as parsed from input */
case class Post(
                 name: String,
                 delta: BigDecimal,
                 commentOpt: Option[String]) extends WithDelta {

  var other: Seq[Post] = Nil
  var txn: Transaction = null

  private val isPositive = delta > Zero

  private def getBalancedOther = {
    other.partition(isPositive == _.isPositive)
  }

  val splitComments:Seq[String] = commentOpt.map(_.split(";").toList).getOrElse(Nil)

  // TODO: All annotations should not be prefixed by "Chq #". Should be a config setting either here or in abandon
  def comments: Iterable[String] = txn.annotationOpt.map("Chq #" + _) ++ txn.payeeOpt ++ splitComments ++ txn.comments

  def date: Date = txn.date

  def simpleCook: CookedPost = {
    val (similarOther, oppOther) = getBalancedOther
    val oppOtherCooked = oppOther.map(o => CookedOtherPost(o.name, o.delta, o.splitComments))
    val similarOtherCooked = similarOther.map(o => CookedOtherPost(o.name, o.delta, o.splitComments))
    CookedPost(date, name, delta, comments.toSeq, oppOtherCooked, similarOtherCooked)
  }
}

/** A transaction that is ready to be reported as the other entry of a transaction.
 * All the analysis has to be done previously */
case class CookedOtherPost(name: String, delta: BigDecimal, comments: Seq[String]) extends WithDelta {
  override def toString: String = {
    name + ": " + delta
  }
}

/** A post that is ready to be reported.
 * All the analysis has to be done previously */
case class CookedPost(date: Date, name: String, delta: BigDecimal, comments: Seq[String], oppositeOthers: Seq[CookedOtherPost], similarOthers: Seq[CookedOtherPost]) extends WithDelta {
  val isComplex: Boolean = math.min(oppositeOthers.length, similarOthers.length + 1) > 1
  override def toString: String = {
    "" + date + " " + name + ": " + delta + "\n  Opposites: " + oppositeOthers + "\n  similars: " + similarOthers
  }
}


