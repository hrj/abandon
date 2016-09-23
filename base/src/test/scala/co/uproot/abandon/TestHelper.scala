package co.uproot.abandon

import scala.util.parsing.input.Position
import scala.util.parsing.input.OffsetPosition

object TestHelper {
  def mkPos(count: Int)(implicit input:String) = {
    Some(InputPosition(None, new OffsetPosition(input, count)))
  }

  def mkPos(input: String, count: Int):Some[InputPosition] = {
    Some(InputPosition(None, new OffsetPosition(input, count)))
  }

  def nlit(n: BigDecimal) = NumericLiteralExpr(n)(None)
  def nlit(n: BigDecimal, count: Int)(implicit testInput: String) = NumericLiteralExpr(n)(mkPos(testInput, count))

  val expenseAccount = AccountName(Seq("Expense"))
  val cashAccount = AccountName(Seq("Cash"))
  val bankAccount = AccountName(Seq("Bank", "Current"))
  val incomeAccount = AccountName(Seq("Income"))
  val equityAccount = AccountName(Seq("Equity"))
  val assetsAccount = AccountName(Seq("Assets"))
}