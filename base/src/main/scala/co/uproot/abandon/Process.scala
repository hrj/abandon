package co.uproot.abandon

import Helper._
import scala.util.parsing.input.PagedSeqReader
import scala.collection.immutable.PagedSeq

case class AppState(accState: AccountState, accDeclarations: Seq[AccountDeclaration])

class TxnGroup(
  _children: Seq[DetailedTransaction],
  val date: Date,
  val annotationOpt: Option[String],
  val payeeOpt: Option[String],
  val groupComments: List[String]) {
  val children = _children.map(_.copy(parentOpt = Some(this)))

  def dateLineStr = {
    val annotationStr = annotationOpt.map(" (" + _ + ")").getOrElse("")
    val payeeStr = payeeOpt.map(" " + _).getOrElse("")
    s"${date.formatYYYYMMMDD}$annotationStr$payeeStr"
  }
}
case class DetailedTransaction(name: AccountName, delta: BigDecimal, commentOpt: Option[String], parentOpt: Option[TxnGroup] = None) {
  def date = parentOpt.get.date
  var resultAmount = Zero
}

class AccountState {

  private var _amounts = Map[AccountName, BigDecimal]() // initAmounts
  private var _txns = Seq[DetailedTransaction]() // initTxns
  private var _txnGroups = Seq[TxnGroup]() // initTxns

  def amounts = _amounts
  def txns = _txns
  def txnGroups = _txnGroups

  def updateAmounts(txnGroup: TxnGroup) = {
    txnGroup.children foreach { txn =>
      val updatedAmount = updateAmountTxn(txn.name, txn.delta, txn.date)
      txn.resultAmount = updatedAmount
      _txns :+= txn
    }
    _txnGroups :+= txnGroup
  }

  private def updateAmountTxn(name: AccountName, delta: BigDecimal, date: Date) = {
    val origAmount = _amounts.get(name).getOrElse(Zero)
    val updatedAmount = (origAmount + delta)
    _amounts += (name -> updatedAmount)
    updatedAmount
  }

  def mkTree(nameMatcher: (String) => Boolean) = {
    val accountsByPathLengths = _amounts.filter(a => nameMatcher(a._1.fullPathStr)).groupBy(_._1.fullPath.length)
    val maxPathLength = maxElseZero(accountsByPathLengths.keys)
    val topLevelAccounts = accountsByPathLengths.get(1).getOrElse(Map())
    def mkTreeLevel(prefix: Seq[String], n: Int): Seq[AccountTreeState] = {
      if (n <= maxPathLength) {
        val children = (n to maxPathLength).flatMap(i => accountsByPathLengths.get(i).getOrElse(Map()).keys.map(_.fullPath).filter(_.startsWith(prefix)).map(_.drop(prefix.length))).toSet
        val (directChildren, inferredChildren) = children.partition(_.length == 1)
        val directChildrenNames = directChildren.map(_.head)
        val inferredChildrenNames = inferredChildren.map(_.head) diff directChildrenNames
        val inferredChildrenTrees = inferredChildrenNames.toSeq.map(x => AccountTreeState(AccountName(prefix :+ x), Zero, mkTreeLevel(prefix :+ x, n + 1)))
        val directChildrenTrees = directChildren.toSeq.map(x => AccountTreeState(AccountName(prefix ++ x), _amounts(AccountName(prefix ++ x)), mkTreeLevel(prefix ++ x, n + 1)))
        (inferredChildrenTrees ++ directChildrenTrees)
      } else {
        Nil
      }
    }
    AccountTreeState(AccountName(Nil), Zero, mkTreeLevel(Nil, 1))
  }

}

case class AccountTreeState(name: AccountName, amount: BigDecimal, childStates: Seq[AccountTreeState]) {
  assert(amount != null)
  assert(childStates != null)
  lazy val total: BigDecimal = amount + childStates.foldLeft(Zero)(_ + _.total)
  lazy val childrenNonZero: Int = childStates.count(c => (!(c.total equals Zero)) || (c.childrenNonZero != 0))
  def countRenderableChildren(isRenderable: (AccountTreeState) => Boolean): Int = {
    childStates.count(c => (!(c.total equals Zero)) || (c.countRenderableChildren(isRenderable) != 0))
  }
  override def toString = {
    val indent = name.depth * 2
    (" " * indent) + name.name + ": " + amount + "(" + total + ")" + (if (childStates.length > 0) "\n" else "") + childStates.mkString("\n")
  }
  def maxNameLength: Int = {
    math.max(name.name.length + name.depth * 2, if (childStates.nonEmpty) childStates.map(_.maxNameLength).max else 0)
  }
  def maxDepth(start: Int = 0): Int = {
    (childStates.map(_.maxDepth()) :+ start).max
  }
}

object Processor {
  def parseAll(inputFiles: Seq[String], settings: Seq[AliasSettings]) = {
    var astEntries = List[ASTEntry]()
    var inputQueue = inputFiles
    var processedFiles = List[String]()
    var parseError = false
    while (inputQueue.nonEmpty && !parseError) {
      val input = mkAbsolutePath(inputQueue.head)
      inputQueue = inputQueue.tail
      if (!processedFiles.contains(input)) {
        processedFiles :+= input
        println("Processing:" + input)
        val source = getSource(input)

        /*
          val scanner = new AbandonParser.lexical.Scanner(StreamReader(source.reader))
          println(Stream.iterate(scanner)(_.rest).takeWhile(!_.atEnd).map(_.first).toList)
          exit
        */
        var transactions = List[co.uproot.abandon.SingleTransaction]()
        val parseResult = AbandonParser.abandon(new AbandonParser.lexical.Scanner(readerForFile(input)))
        parseResult match {
          case AbandonParser.Success(result, _) =>
            val includes = filterByType[IncludeDirective](result)
            inputQueue ++= includes.map(id => mkRelativeFileName(id.fileName, input))
            if (!settings.isEmpty) {
              settings.map { setting =>
                val result1 = result match {
                  case List(Transaction(date, txns, b, c, d)) =>
                    txns.map { singleTransaction =>
                      singleTransaction match {
                        case SingleTransaction(acc1, expr1, expr2) =>
                          if (acc1.toString.equals(setting.alias)) {
                            transactions :+= SingleTransaction(AccountName(Seq(setting.name)), expr1, expr2)
                          } else {
                            transactions :+= SingleTransaction(acc1, expr1, expr2)
                          }
                      }
                    }
                    List(Transaction(date, transactions, b, c, d))
                }
                astEntries ++= result1
              }
            } else {
              astEntries ++= result
            }
          case n: AbandonParser.NoSuccess =>
            println("Error while parsing %s:\n%s" format (bold(input), n))
            parseError = true
        }
        source.close
      }
    }
    (parseError, astEntries, processedFiles.toSet)
  }

  def getSource(fileName: String): io.Source = {
    var retryCount = 0
    var source: io.Source = null
    while (retryCount < 10) {
      try {
        source = io.Source.fromFile(fileName)
        retryCount = 10
      } catch {
        case e: java.io.FileNotFoundException =>
          retryCount += 1
      }
    }
    source
  }

  def mkRelativeFileName(path: String, parentFile: String) = {
    if (new java.io.File(path).isAbsolute) {
      path
    } else {
      val parentFilePath = new java.io.File(parentFile).getParent()
      if (parentFilePath == null) {
        path
      } else {
        parentFilePath + java.io.File.separator + path
      }
    }
  }

  def mkAbsolutePath(path: String) = {
    (new java.io.File(path)).getCanonicalPath
  }

  private def readerForFile(fileName: String) = {
    new PagedSeqReader(PagedSeq.fromReader(io.Source.fromFile(fileName).reader))
  }

  def process(entries: Seq[ASTEntry]) = {
    val definitions = filterByType[Definition[BigDecimal]](entries)
    val evaluationContext = new EvaluationContext[BigDecimal](definitions, Nil, new NumericLiteralExpr(_))

    val transactions = filterByType[Transaction](entries)
    val sortedTxns = transactions.sortBy(_.date)(DateOrdering)
    val accState = new AccountState()
    sortedTxns foreach { tx =>
      val (txWithAmount, txNoAmount) = tx.transactions.partition(t => t.amount.isDefined)
      assert(txNoAmount.length <= 1, "More than one account with unspecified amount: " + txNoAmount)
      var txTotal = Zero
      var detailedTxns = Seq[DetailedTransaction]()
      txWithAmount foreach { t =>
        val delta = t.amount.get.evaluate(evaluationContext)
        txTotal += delta
        detailedTxns :+= DetailedTransaction(t.accName, delta, t.commentOpt)
      }
      txNoAmount foreach { t =>
        val delta = -txTotal
        txTotal += delta
        detailedTxns :+= DetailedTransaction(t.accName, delta, t.commentOpt)
      }
      accState.updateAmounts(new TxnGroup(detailedTxns, tx.date, tx.annotationOpt, tx.payeeOpt, tx.comments))
      assert(txTotal equals Zero, s"Transactions do not balance. Unbalance amount: $txTotal")
    }
    val accountDeclarations = filterByType[AccountDeclaration](entries)
    AppState(accState, accountDeclarations)
  }

  def checkConstaints(appState: AppState, constraints: Seq[Constraint]) = {
    constraints.foreach { c => c.check(appState) }
  }

}
