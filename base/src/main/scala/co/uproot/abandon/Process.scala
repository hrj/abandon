package co.uproot.abandon

import Helper._
import scala.util.parsing.input.PagedSeqReader
import scala.collection.immutable.PagedSeq
import java.io.FileNotFoundException

case class AppState(accState: AccountState, accDeclarations: Seq[AccountDeclaration])

class PostGroup(
  _children: Seq[DetailedPost],
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
  def parseAll(inputFiles: Seq[String]) = {
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
        val sourceOpt = getSource(input)
        sourceOpt match {
          case Some(source) =>

            /*
            val scanner = new AbandonParser.lexical.Scanner(StreamReader(source.reader))
            println(Stream.iterate(scanner)(_.rest).takeWhile(!_.atEnd).map(_.first).toList)
            exit
          */

            val parseResult = AbandonParser.abandon(new AbandonParser.lexical.Scanner(readerForFile(input)))
            parseResult match {
              case AbandonParser.Success(result, _) =>
                val includes = filterByType[IncludeDirective](result)
                inputQueue ++= includes.map(id => mkRelativeFileName(id.fileName, input))
                astEntries ++= result
              case n: AbandonParser.NoSuccess =>
                println("Error while parsing %s:\n%s" format (bold(input), n))
                parseError = true
            }
            source.close
          case None =>
            throw new InputFileNotFoundError("File not found: " + input)
        }
      }
    }
    (parseError, astEntries, processedFiles.toSet)
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

  private def readerForFile(fileName: String) = {
    new PagedSeqReader(PagedSeq.fromReader(io.Source.fromFile(fileName).reader))
  }

  def process(entries: Seq[ASTEntry], accountSettings: Seq[AccountSettings]) = {
    val definitions = filterByType[Definition](entries)
    val evaluationContext = new EvaluationContext(definitions, Nil)


    val transactions = filterByType[Transaction](entries)
    val sortedTxns = transactions.sortBy(_.date)(DateOrdering)
    val accState = new AccountState()
    val aliasMap = accountSettings.collect{ case AccountSettings(name, Some(alias)) => alias -> name }.toMap

    def transformAlias(accName: AccountName): AccountName = {
      aliasMap.get(accName.fullPathStr).getOrElse(accName)
    }

    sortedTxns foreach { tx =>
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
      accState.updateAmounts(new PostGroup(detailedPosts, tx.date, tx.annotationOpt, tx.payeeOpt, tx.comments))
      assert(txTotal equals Zero, s"Transactions do not balance. Unbalance amount: $txTotal")
    }
    val accountDeclarations = filterByType[AccountDeclaration](entries)
    AppState(accState, accountDeclarations)
  }

  def checkConstaints(appState: AppState, constraints: Seq[Constraint]) = {
    constraints.foreach { c => c.check(appState) }
  }

}
