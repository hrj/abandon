package co.uproot.abandon

object ASTHelper {
  type NumericExpr = Expr[BigDecimal]

  def parseAccountName(name:String):AccountName = {
    AbandonParser.accountName(ParserHelper.scanner(name)) match {
      case AbandonParser.Success(result, _) => result
      case _ => throw new InputError("Couldn't parse accountName: " + name)
    }
  }
}

class InputError(msg: String) extends RuntimeException(msg)
class MissingDestinationError(msg: String) extends InputError(msg)
class SourceDestinationClashError(msg: String) extends InputError(msg)
class InputFileNotFoundError(fileName:String) extends InputError("File not found: " + fileName)

class ConstraintError(msg: String) extends RuntimeException(msg)

import ASTHelper._

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

  def formatISO8601Ext = {
    f"$year%4d-$month%02d-$day%02d"
  }

  def formatCompactYYYYMMDD = {
    f"$year%4d/$month%d/$day%d"
  }

  def formatYYYYMMMDD = {
    f"$year%4d ${Helper.getShortMonth(month)} $day%d"
  }

  def toInt = {
    year * Date.yearMultiplier + month * Date.monthMultiplier + day
  }

  def formatCompact = {
    s"$year,$month,$day"
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
 * To create filter for time span, stack "Begin" and "End" filters.
 */
case class EndDateTxnFilter(end: Date) extends TransactionFilter {
  override def filter(txn: Transaction) = { txn.date.toInt <= end.toInt }
  override def description() = { "end: Transaction date is on or before: " + end.formatISO8601Ext }
  override def xmlDescription() = { <filter type="end" date={ end.formatISO8601Ext } />}
}
case class BeginDateTxnFilter(begin: Date) extends TransactionFilter {
  override def filter(txn: Transaction) = { begin.toInt <= txn.date.toInt }
  override def description() = { "begin: Transaction date is on or after: " + begin.formatISO8601Ext }
  override def xmlDescription() = { <filter type="begin" date={ begin.formatISO8601Ext } />}
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
 * Account Txn filter
 * Returns all transactions which have at least one matching account name
 */
case class AccountTxnFilter(regex: String) extends TransactionFilter {
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

case class Post(accName: AccountName, amount: Option[NumericExpr], commentOpt: Option[String])

sealed class ASTEntry

case class PayeeDef(name: String) extends ASTEntry
case class TagDef(name: String) extends ASTEntry

sealed class ASTTangibleEntry extends ASTEntry

case class Transaction(date: Date, posts: Seq[Post], annotationOpt: Option[String], payeeOpt: Option[String], comments: List[String]) extends ASTTangibleEntry

case class Definition[T](name: String, params: List[String], rhs: Expr[T]) extends ASTTangibleEntry {
  def prettyPrint = "def %s(%s) = %s" format (name, params.mkString(", "), rhs.prettyPrint)
}

case class AccountDeclaration(name: AccountName, details: Map[String, Expr[_]]) extends ASTTangibleEntry

case class IncludeDirective(fileName: String) extends ASTEntry

sealed abstract class Expr[T] {
  def evaluate(context: EvaluationContext[T]): T
  def prettyPrint = toString
  def getRefs: Seq[Ref]
}

trait LiteralValue[T] {
  val value: T
  def getRefs = Nil
  def evaluate(context: EvaluationContext[T]): T = value
}

abstract class BooleanExpr extends Expr[Boolean] {
  def evaluate(context: EvaluationContext[Boolean]): Boolean
}

case class BooleanLiteralExpr(val value: Boolean) extends BooleanExpr with LiteralValue[Boolean] {
}

case class NumericLiteralExpr(val value: BigDecimal) extends NumericExpr with LiteralValue[BigDecimal] {
  override def prettyPrint = value.toString
}

case class FunctionExpr[T](val name: String, val arguments: Seq[Expr[T]]) extends Expr[T] {
  def evaluate(context: EvaluationContext[T]): T = context.getValue(name, arguments.map(_.evaluate(context)))
  override def prettyPrint = "%s(%s)" format (name, arguments.map(_.prettyPrint).mkString(", "))
  def getRefs = Ref(name, arguments.length) +: arguments.flatMap(_.getRefs)
}

case class IdentifierExpr[T](val name: String) extends Expr[T] {
  def evaluate(context: EvaluationContext[T]): T = context.getValue(name, Nil)
  override def prettyPrint = name
  def getRefs = Seq(Ref(name, 0))
}

abstract class BinaryNumericExpr(op1: NumericExpr, op2: NumericExpr, opChar: String, operation: (BigDecimal, BigDecimal) => BigDecimal) extends NumericExpr {
  def evaluate(context: EvaluationContext[BigDecimal]): BigDecimal = operation(op1.evaluate(context), op2.evaluate(context))
  override def prettyPrint = op1.prettyPrint + " " + opChar + " " + op2.prettyPrint
  def getRefs = op1.getRefs ++ op2.getRefs
}

case class AddExpr(val op1: NumericExpr, val op2: NumericExpr) extends BinaryNumericExpr(op1, op2, "+", _ + _)

case class SubExpr(val op1: NumericExpr, val op2: NumericExpr) extends BinaryNumericExpr(op1, op2, "-", _ - _)

case class MulExpr(val op1: NumericExpr, val op2: NumericExpr) extends BinaryNumericExpr(op1, op2, "*", _ * _)

case class DivExpr(val op1: NumericExpr, val op2: NumericExpr) extends BinaryNumericExpr(op1, op2, "/", _ / _)

case class UnaryNegExpr(val op: NumericExpr) extends NumericExpr {
  def evaluate(context: EvaluationContext[BigDecimal]): BigDecimal = -op.evaluate(context)
  override def prettyPrint = " -(" + op.prettyPrint + ")"
  def getRefs = op.getRefs
}
