package co.uproot.abandon

object ASTHelper {
  type NumericExpr = Expr[BigDecimal]
}

class InputError(msg: String) extends RuntimeException(msg)
class ConstraintError(msg: String) extends RuntimeException(msg)
import ASTHelper._
import Helper._
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

case class SingleTransaction(accName: AccountName, amount: Option[NumericExpr], commentOpt: Option[String])

sealed class ASTEntry

case class PayeeDef(name: String) extends ASTEntry
case class TagDef(name: String) extends ASTEntry

sealed class ASTTangibleEntry extends ASTEntry

case class Transaction(date: Date, transactions: Seq[SingleTransaction], annotationOpt: Option[String], payeeOpt: Option[String], comments: List[String]) extends ASTTangibleEntry

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