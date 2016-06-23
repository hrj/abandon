package co.uproot.abandon

object ASTHelper {

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

case class Transaction(date: Date, posts: Seq[Post], annotationOpt: Option[String], payeeOpt: Option[String], comments: List[String]) extends ASTTangibleEntry

case class Definition(name: String, params: List[String], rhs: Expr) extends ASTTangibleEntry {
  def prettyPrint = "def %s(%s) = %s" format (name, params.mkString(", "), rhs.prettyPrint)
}

case class AccountDeclaration(name: AccountName, details: Map[String, Expr]) extends ASTTangibleEntry

case class IncludeDirective(fileName: String) extends ASTEntry

sealed abstract class Expr {
  def prettyPrint = toString
  def getRefs: Seq[Ref]
}

trait LiteralValue[T] {
  val value: T
  def getRefs = Nil
}

case class BooleanLiteralExpr(val value: Boolean) extends Expr with LiteralValue[Boolean] {
}

case class NumericLiteralExpr(val value: BigDecimal) extends Expr with LiteralValue[BigDecimal] {
  override def prettyPrint = value.toString
}

case class FunctionExpr(val name: String, val arguments: Seq[Expr]) extends Expr {
  override def prettyPrint = "%s(%s)" format (name, arguments.map(_.prettyPrint).mkString(", "))
  def getRefs = Ref(name, arguments.length) +: arguments.flatMap(_.getRefs)
}

case class IdentifierExpr(val name: String) extends Expr {
  override def prettyPrint = name
  def getRefs = Seq(Ref(name, 0))
}

sealed abstract class BinaryExpr(op1: Expr, op2: Expr, opChar: String, operation: (BigDecimal, BigDecimal) => BigDecimal) extends Expr {
  override def prettyPrint = op1.prettyPrint + " " + opChar + " " + op2.prettyPrint
  def getRefs = op1.getRefs ++ op2.getRefs
}

case class AddExpr(val op1: Expr, val op2: Expr) extends BinaryExpr(op1, op2, "+", _ + _)

case class SubExpr(val op1: Expr, val op2: Expr) extends BinaryExpr(op1, op2, "-", _ - _)

case class MulExpr(val op1: Expr, val op2: Expr) extends BinaryExpr(op1, op2, "*", _ * _)

case class DivExpr(val op1: Expr, val op2: Expr) extends BinaryExpr(op1, op2, "/", _ / _)

case class UnaryNegExpr(val op: Expr) extends Expr {
  override def prettyPrint = " -(" + op.prettyPrint + ")"
  def getRefs = op.getRefs
}

case class ConditionExpr(val e1: Expr, val op: String, val e2: Expr) extends Expr {
  def getRefs = e1.getRefs ++ e2.getRefs
}

case class IfExpr(val cond: Expr, val op1: Expr, val op2: Expr) extends Expr {
  def getRefs = cond.getRefs ++ op1.getRefs ++ op2.getRefs
}
