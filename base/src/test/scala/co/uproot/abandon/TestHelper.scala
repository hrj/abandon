package co.uproot.abandon

object TestHelper {
  def nlit(n: BigDecimal) = NumericLiteralExpr(n)

  val expenseAccount = AccountName(Seq("Expense"))
  val cashAccount = AccountName(Seq("Cash"))
  val bankAccount = AccountName(Seq("Bank", "Current"))
  val incomeAccount = AccountName(Seq("Income"))
  val equityAccount = AccountName(Seq("Equity"))
  val assetsAccount = AccountName(Seq("Assets"))
}