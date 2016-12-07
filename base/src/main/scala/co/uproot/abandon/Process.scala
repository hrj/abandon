package co.uproot.abandon

import Helper._
import scala.util.parsing.input.PagedSeqReader
import scala.collection.immutable.PagedSeq
import java.io.FileNotFoundException

case class AppState(accState: AccountState)

class PostGroup(
  _children: Seq[DetailedPost],
  val txn: Transaction,
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

case class DetailedPost(name: AccountName, delta: BigDecimal, commentOpt: Option[String], parentOpt: Option[PostGroup] = None) {
  def date = parentOpt.get.date
  var resultAmount = Zero
}

class AccountState {

  private var _amounts = Map[AccountName, BigDecimal]() // initAmounts
  private var _posts = Seq[DetailedPost]() 
  private var _postGroups = Seq[PostGroup]() 

  def amounts = _amounts
  def posts = _posts
  def postGroups = _postGroups

  def updateAmounts(postGroup: PostGroup) = {
    postGroup.children foreach { post =>
      val updatedAmount = updateAmountTxn(post.name, post.delta, post.date)
      post.resultAmount = updatedAmount
      _posts :+= post
    }
    _postGroups :+= postGroup
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

case class AccountTreeState(name: AccountName, amount: BigDecimal, childStates: Seq[AccountTreeState]) extends Ordered[AccountTreeState] {
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

  def compare(that: AccountTreeState) = this.name.fullPathStr.compare(that.name.fullPathStr)

  def toXML: xml.Node = {
    if (name.fullPath.isEmpty) {
      // This is root-node
      <accounttree>
        { childStates.sorted.map(_.toXML) }
      </accounttree>
    }
    else {
      <account sum={amount.toString()} cumulative={total.toString()} name={name.fullPathStr} >
        { childStates.sorted.map(_.toXML) }
      </account>
    }
  }
  def maxNameLength: Int = {
    math.max(name.name.length + name.depth * 2, if (childStates.nonEmpty) childStates.map(_.maxNameLength).max else 0)
  }
  def maxDepth(start: Int = 0): Int = {
    (childStates.map(_.maxDepth()) :+ start).max
  }
}

object Processor {
  case class Input(path: String, parentScope: Scope)
  
  def parseAll(inputFiles: Seq[String], quiet: Boolean) = {
    // var astEntries = List[ASTEntry]()
    val rootScope = Scope(Nil, None)
    var inputQueue = inputFiles.map(Input(_, rootScope))
    var processedFiles = List[String]()
    var parseError = false
    while (inputQueue.nonEmpty && !parseError) {
      val input = inputQueue.head
      val inputPath = mkAbsolutePath(input.path)
      inputQueue = inputQueue.tail
      if (!processedFiles.contains(inputPath)) {
        processedFiles :+= inputPath
        if (!quiet) println("Processing:" + inputPath)
        val sourceOpt = getSource(inputPath)
        sourceOpt match {
          case Some(source) =>
            val parser = new AbandonParser(Some(inputPath))

            val parseResult = parser.abandon(Option(input.parentScope))(parser.scannerFromFile(inputPath))
            parseResult match {
              case parser.Success(scope, _) =>
                val includes = filterByType[IncludeDirective](scope.entries)
                inputQueue ++= includes.map(id => Input(mkRelativeFileName(id.fileName, inputPath), scope))
                input.parentScope.addIncludedScope(scope)
              case n: parser.NoSuccess =>
                println("Error while parsing %s:\n%s" format (bold(inputPath), n))
                parseError = true
            }
            source.close
          case None =>
            throw new InputFileNotFoundError("File not found: " + input)
        }
      }
    }
    (parseError, rootScope, processedFiles.toSet)
  }

  def getSource(fileName: String): Option[io.Source] = {
    var retryCount = 0
    var source: Option[io.Source] = None
    while (retryCount < 10) {
      try {
        source = Some(io.Source.fromFile(fileName))
        retryCount = 10
      } catch {
        case e: java.io.FileNotFoundException =>
          retryCount += 1
      }
    }
    source
  }

  /**
   * dirPath must be path to directory
   * Returns '/' or '\' terminated canonical path
   */
  def mkCanonicalDirPath(dirPath: String): String = {
    val canonicalPath = (new java.io.File(dirPath)).getCanonicalPath
    if (canonicalPath.endsWith(java.io.File.separator))
      // this is root e.g. '/'
      canonicalPath
    else
      canonicalPath + java.io.File.separator
  }
  /**
   * This is '/' or '\' terminated string, or empty
   */
  def mkParentDirPath(parentFile: String): String = {
    Option(new java.io.File(parentFile).getParentFile) match {
      case Some(parentDir) => mkCanonicalDirPath(parentDir.getCanonicalPath)
      case None => ""
    }
  }

  def mkRelativeFileName(path: String, parentFile: String) = {
    if (new java.io.File(path).isAbsolute) {
      path
    } else {
      mkParentDirPath(parentFile) + path
    }
  }

  def mkAbsolutePath(path: String) = {
    (new java.io.File(path)).getCanonicalPath
  }

  def process(scope: Scope, accountSettings: Seq[AccountSettings], txnFilters: Option[TxnFilterStack]) = {
    scope.checkDupes()
    val transactions = (filterByType[ScopedTxn](scope.allTransactions)).filter { scopeTxn =>
      txnFilters match {
        case Some(filterStack) => filterStack.filter(scopeTxn.txn)
        case None => { true }
      }
    }
    val sortedTxns = transactions.sortBy(_.txn.date)(DateOrdering)
    val accState = new AccountState()
    val aliasMap = accountSettings.collect{ case AccountSettings(name, Some(alias)) => alias -> name }.toMap

    def transformAlias(accName: AccountName): AccountName = {
      aliasMap.get(accName.fullPathStr).getOrElse(accName)
    }

    sortedTxns foreach { case ScopedTxn(tx, txScope) =>
      val entries = txScope.entries
      val evaluationContext = new EvaluationContext(txScope, Nil)

      val (postsWithAmount, postsNoAmount) = tx.posts.partition(p => p.amount.isDefined)
      assert(postsNoAmount.length <= 1, "More than one account with unspecified amount: " + postsNoAmount)
      var txTotal = Zero
      var detailedPosts = Seq[DetailedPost]()
      postsWithAmount foreach { p =>
        val delta = evaluationContext.evaluateBD(p.amount.get)
        txTotal += delta
        detailedPosts :+= DetailedPost(transformAlias(p.accName), delta, p.commentOpt)
      }
      postsNoAmount foreach { p =>
        val delta = -txTotal
        txTotal += delta
        detailedPosts :+= DetailedPost(transformAlias(p.accName), delta, p.commentOpt)
      }
      if (!(txTotal equals Zero)) {
        txScope.definitions.find { d => d.name equals "defaultAccount" } match {
          case Some(defaultAccountDef) => {
            val defaultAccount = evaluationContext.evaluateString(FunctionExpr("defaultAccount", Nil, Some(tx.pos)))
            val fullDefaultAccount = transformAlias(AccountName(defaultAccount.split(":")))
            val delta = -txTotal
            txTotal += delta
            detailedPosts :+= DetailedPost(fullDefaultAccount, delta, None)
          }
          case None =>
        }
      }
      if (!(txTotal equals Zero)) {
        throw new ConstraintPosError(s"Transaction does not balance. Unbalanced amount: $txTotal", tx.pos)
      }
      accState.updateAmounts(new PostGroup(detailedPosts, tx, tx.date, tx.annotationOpt, tx.payeeOpt, tx.comments))
    }
    // val accountDeclarations = filterByType[AccountDeclaration](entries)
    AppState(accState)
  }

  def checkConstaints(appState: AppState, constraints: Seq[Constraint]) = {
    constraints.foreach { c => c.check(appState) }
  }

}
