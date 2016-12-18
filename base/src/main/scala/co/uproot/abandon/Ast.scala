package co.uproot.abandon

import scala.util.parsing.input.Position

object ASTHelper {

  def parseAccountName(name:String):AccountName = {
    ParserHelper.parser.accountName(ParserHelper.scanner(name)) match {
      case ParserHelper.parser.Success(result, _) => result
      case ParserHelper.parser.Failure(msg, next) => throw new InputError("Couldn't parse accountName: " + name)
      case ParserHelper.parser.Error(msg, next) => throw new InputError("Couldn't parse accountName: " + name)
    }
  }
}

case class InputPosition(pathOpt: Option[String], pos: Position) {
  override def toString = pathOpt.getOrElse("") + " line: " + pos.line + " col: " + pos.column
}

class InputError(msg: String) extends RuntimeException(msg)
class MissingDestinationError(msg: String) extends InputError(msg)
class SourceDestinationClashError(msg: String) extends InputError(msg)
class InputFileNotFoundError(fileName:String) extends InputError("File not found: " + fileName)

class InputPosError(msg: String, pos: InputPosition) extends InputError(msg + " in " + pos)

class DupSymbolInScopeError(symbol: String, pos: InputPosition) extends InputPosError("A symbol was defined multiple times within the same scope: " + symbol, pos)

class ConstraintError(msg: String) extends RuntimeException(msg)
class ConstraintPosError(msg: String, pos: InputPosition) extends ConstraintError(msg + " in " + pos)

object Date {
  val yearMultiplier = 10000
  val monthMultiplier = 100

  def fromInt(i: Int) = {
    val year = i / yearMultiplier
    val yearComponent = year * yearMultiplier
    val month = (i - yearComponent) / monthMultiplier
    val monthComponent = month * monthMultiplier
    val day = i - yearComponent - monthComponent
    Date(year, month, day)
  }
}


case class Date(year: Int, month: Int, day: Int) {
  def formatYYYYMMDD = {
    f"$year%4d / $month%d / $day%d"
  }

  /**
    * @return ISO 8601 extended day (YYYY-MM-DD)
    */
  def formatISO8601Ext = {
    f"$year%4d-$month%02d-$day%02d"
  }

  /**
    * @return ISO-8601 week without day (2010-01-01 => 2009-W53)
    */
  def formatISO8601Week = {
    val frmtISOWeek = java.time.format.DateTimeFormatter.ISO_WEEK_DATE
    val jDate = java.time.LocalDate.of(year, month, day)

    jDate.format(frmtISOWeek).substring(0,8)
  }

  /**
    * @return ISO-8601 week with week day (2010-01-01 => 2009-W53-5)
    */
  def formatISO8601WeekDay = {
    val frmtISOWeek = java.time.format.DateTimeFormatter.ISO_WEEK_DATE
    val jDate = java.time.LocalDate.of(year, month, day)

    jDate.format(frmtISOWeek)
  }

  def formatCompactYYYYMMDD = {
    f"$year%4d/$month%d/$day%d"
  }

  def formatYYYYMMMDD = {
    f"$year%4d ${Helper.getShortMonth(month)} $day%d"
  }

  /**
    * @return date as int with day resolution
    */
  def toInt = {
    year * Date.yearMultiplier + month * Date.monthMultiplier + day
  }

  /**
    * @return date as int with month resolution
    */
  def toIntYYYYMM = {
    year * Date.yearMultiplier + month * Date.monthMultiplier
  }

  /**
    * @return date as int with year resolution
    */
  def toIntYYYY = {
    year * Date.yearMultiplier
  }

  def formatCompact = {
    s"$year,$month,$day"
  }

  def hasDayResolution = {
    day != 0
  }

  def hasMonthResolution = {
    month != 0
  }
}

/**
 * Trait for stackable Transaction filters
 * These filters operate on "raw" AST level,
 * so all filtering decisions are based on unprocessed
 * information.
 *
 * TxnFilterStack is used to glue these filters together.
 */
sealed trait TransactionFilter {
  def filter(txn: Transaction): Boolean
  def description(): String
  def xmlDescription(): xml.Node
}

/*
 * Txn:Time filters
 * To create filter for time span, stack "onOrAfter" and "before" filters.
 */
case class BeforeDateTxnFilter(before: Date) extends TransactionFilter {
  override def filter(txn: Transaction) = { txn.date.toInt < before.toInt }
  override def description() = { "before: Transaction date is before: " + before.formatISO8601Ext }
  override def xmlDescription() = { <filter type="before" date={ before.formatISO8601Ext } />}
}
case class OnOrAfterDateTxnFilter(onOrAfter: Date) extends TransactionFilter {
  override def filter(txn: Transaction) = { onOrAfter.toInt <= txn.date.toInt }
  override def description() = { "onOrAfter: Transaction date is on or after: " + onOrAfter.formatISO8601Ext }
  override def xmlDescription() = { <filter type="onOrAfter" date={ onOrAfter.formatISO8601Ext } />}
}

case class PayeeTxnFilter(regex: String) extends TransactionFilter {
  val pattern = java.util.regex.Pattern.compile(regex)

  override def filter(txn: Transaction) = {
    pattern.matcher(txn.payeeOpt match {
      case Some(payee) => payee
      case None => ""
    }).matches
  }
  override def description() = { "payee: Payee must match \"" + pattern.toString + "\""}
  override def xmlDescription() = { <filter type="payee" pattern={ pattern.toString } /> }
}

/**
 * Annotation Txn filter
 *  - returns all transactions with matching annotation
 */
case class AnnotationTxnFilter(regex: String) extends TransactionFilter {
  val pattern = java.util.regex.Pattern.compile(regex)

  override def filter(txn: Transaction) = {
    pattern.matcher(txn.annotationOpt match {
      case Some(ann) => ann
      case None => ""
    }).matches
  }
  override def description() = { "annotation: Annotation must match \"" + pattern.toString + "\"" }
  override def xmlDescription() = { <filter type="annotation" pattern={ pattern.toString }/> }
}

/**
 * Account Name Txn filter
 * Returns all transactions which have at least one matching account name
 */
case class AccountNameTxnFilter(regex: String) extends TransactionFilter {
  val pattern = java.util.regex.Pattern.compile(regex)

  override def filter(txn: Transaction) = {
    txn.posts.exists { post =>
      pattern.matcher(post.accName.toString).matches
    }
  }
  override def description() = { "account: At least one of transaction's accounts must match \"" + pattern.toString + "\"" }
  override def xmlDescription() = { <filter type="account" pattern={ pattern.toString }/> }
}

// TODO Txn comment filter
// TODO Txn:Post comment filter

object FilterStackHelper {
  def getFilterWarnings(txnFilters: Option[TxnFilterStack], indent: String): List[String] = {
    txnFilters match {
      case Some(txnFilters) => {
        indent + txnFilters.description() ::
        txnFilters.filterDescriptions().map { desc =>
          indent * 2 + desc
        }.toList
      }
      case None => Nil
    }
  }
}

/**
 * Trait for Transaction filter stacks
 * Filter stack defines relationship between filters
 * (e.g. f1 && f2,  f1 || f2 or some other, specialized logic)
 */
sealed trait TxnFilterStack {
  def filter(txn: Transaction): Boolean
  def description(): String
  def filterDescriptions(): Seq[String]
  def xmlDescription(): xml.Node
}

/**
 * AND- TxnFilterStack (e.g. f1 && f2 && ..)
 */
case class ANDTxnFilterStack(filterStack: Seq[TransactionFilter]) extends TxnFilterStack {
  override def filter(txn: Transaction): Boolean = {
    filterStack.forall { f => f.filter(txn) }
  }
  override def description() = {
    assert(!filterStack.isEmpty)
    "All following conditions must be true:"
  }
  override def filterDescriptions() = {
    assert(!filterStack.isEmpty)
    filterStack.map({case f => f.description})
  }
  override def xmlDescription(): xml.Node = {
    assert(!filterStack.isEmpty)
    <filters type="every">
    {
      filterStack.map({ filt => filt.xmlDescription })
    }
    </filters>
  }
}

object DateOrdering extends Ordering[Date] {
  def compare(x: Date, y: Date) = {
    x.toInt - y.toInt
  }
}


case class AccountName(fullPath: Seq[String]) {
  val name = fullPath.lastOption.getOrElse("")
  val fullPathStr = fullPath.mkString(":")
  override def toString = fullPathStr
  val depth = fullPath.length
}

case class Post(accName: AccountName, amount: Option[Expr], commentOpt: Option[String])

sealed class ASTEntry

case class PayeeDef(name: String) extends ASTEntry
case class TagDef(name: String) extends ASTEntry

sealed class ASTTangibleEntry extends ASTEntry

case class Transaction(pos: InputPosition, date: Date, posts: Seq[Post], annotationOpt: Option[String], payeeOpt: Option[String], comments: List[String]) extends ASTTangibleEntry

case class Definition(pos: InputPosition, name: String, params: List[String], rhs: Expr) extends ASTTangibleEntry {
  def prettyPrint = "def %s(%s) = %s" format (name, params.mkString(", "), rhs.prettyPrint)
}

case class AccountDeclaration(name: AccountName, details: Map[String, Expr]) extends ASTTangibleEntry

case class IncludeDirective(fileName: String) extends ASTEntry

sealed abstract class Expr {
  val pos: Option[InputPosition]
  def prettyPrint = toString
  def getRefs: Seq[Ref]
}

trait LiteralValue[T] {
  val value: T
  def getRefs = Nil
}

case class BooleanLiteralExpr(val value: Boolean)(val pos: Option[InputPosition]) extends Expr with LiteralValue[Boolean]
case class StringLiteralExpr(val value: String)(val pos: Option[InputPosition]) extends Expr with LiteralValue[String]

case class NumericLiteralExpr(val value: BigDecimal)(val pos: Option[InputPosition]) extends Expr with LiteralValue[BigDecimal] {
  override def prettyPrint = value.toString
}

case class FunctionExpr(val name: String, val arguments: Seq[Expr], val pos: Option[InputPosition]) extends Expr {
  override def prettyPrint = "%s(%s)" format (name, arguments.map(_.prettyPrint).mkString(", "))
  def getRefs = Ref(name, arguments.length, pos) +: arguments.flatMap(_.getRefs)
}

case class IdentifierExpr(val name: String)(val pos: Option[InputPosition]) extends Expr {
  override def prettyPrint = name
  def getRefs = Seq(Ref(name, 0, pos))
}

sealed abstract class BinaryExpr(op1: Expr, op2: Expr, opChar: String, operation: (BigDecimal, BigDecimal) => BigDecimal) extends Expr {
  override def prettyPrint = op1.prettyPrint + " " + opChar + " " + op2.prettyPrint
  def getRefs = op1.getRefs ++ op2.getRefs
}

case class AddExpr(val op1: Expr, val op2: Expr)(val pos: Option[InputPosition]) extends BinaryExpr(op1, op2, "+", _ + _)

case class SubExpr(val op1: Expr, val op2: Expr)(val pos: Option[InputPosition]) extends BinaryExpr(op1, op2, "-", _ - _)

case class MulExpr(val op1: Expr, val op2: Expr)(val pos: Option[InputPosition]) extends BinaryExpr(op1, op2, "*", _ * _)

case class DivExpr(val op1: Expr, val op2: Expr)(val pos: Option[InputPosition]) extends BinaryExpr(op1, op2, "/", _ / _)

case class UnaryNegExpr(val op: Expr)(val pos: Option[InputPosition]) extends Expr {
  override def prettyPrint = " -(" + op.prettyPrint + ")"
  def getRefs = op.getRefs
}

case class ConditionExpr(val e1: Expr, val op: String, val e2: Expr)(val pos: Option[InputPosition]) extends Expr {
  def getRefs = e1.getRefs ++ e2.getRefs
}

case class IfExpr(val cond: Expr, val op1: Expr, val op2: Expr)(val pos: Option[InputPosition]) extends Expr {
  def getRefs = cond.getRefs ++ op1.getRefs ++ op2.getRefs
}

case class ScopedTxn(txn: Transaction, scope: Scope)

/* Note: This is class has mutable state */
case class Scope(entries: Seq[ASTEntry], parentOpt: Option[Scope]) extends ASTEntry {
  private var includedScopes: List[Scope] = Nil

  def addIncludedScope(i: Scope) {
    includedScopes :+= i
  }

  private val localDefinitions = Helper.filterByType[Definition](entries)
  private def allLocalDefinitions: Seq[Definition] = localDefinitions ++ includedScopes.flatMap(_.allLocalDefinitions)

  def definitions:Seq[Definition] = {
    val parentDefinitions = parentOpt.map(_.definitions).getOrElse(Nil)
    val allLocalDefs = allLocalDefinitions
    val allLocalNames = allLocalDefs.map(_.name)
    parentDefinitions.filter(d => !allLocalNames.contains(d.name)) ++ allLocalDefs
  }

  private val localTransactions = Helper.filterByType[Transaction](entries)
  private def allLocalTransactions:Seq[ScopedTxn] = localTransactions.map(ScopedTxn(_, this)) ++ includedScopes.flatMap(_.allTransactions)

  def childScopes = Helper.filterByType[Scope](entries)

  def allTransactions:Seq[ScopedTxn] = allLocalTransactions ++ childScopes.flatMap(_.allTransactions)

  def checkDupes() {
    Helper.allUnique(localDefinitions.map(_.name)) match {
      case Some(nonUnique) =>
        throw new DupSymbolInScopeError(nonUnique, localDefinitions.find { _.name == nonUnique }.get.pos)
      case None =>
        includedScopes.foreach(_.checkDupes())
    }
  }
}
