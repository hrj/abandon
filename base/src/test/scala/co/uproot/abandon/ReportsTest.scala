package co.uproot.abandon

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ReportsTest extends AnyFlatSpec with Matchers {
  private def state(posts: (Seq[String], BigDecimal)*) = {
    val accountState = new AccountState
    val detailedPosts = posts.map { case (path, amount) =>
      DetailedPost(AccountName(path), amount, None)
    }
    val transaction = Transaction(null, Date(2026, 1, 2), Nil, None, None, Nil)
    accountState.updateAmounts(
      new PostGroup(detailedPosts, transaction, transaction.date, None, None, Nil)
    )
    AppState(accountState)
  }

  private def settings(rightAccounts: Seq[String] = Seq("Income")) =
    Settings(Nil, Nil, Nil, Nil, ReportOptions(rightAccounts), Nil, None, true, None, None)

  "balanceReport" should "render nested left and right accounts with a zero total" in {
    val appState = state(
      Seq("Assets", "Cash") -> BigDecimal(100),
      Seq("Expenses", "Food") -> BigDecimal(25),
      Seq("Income", "Salary") -> BigDecimal(-125)
    )
    val report = Reports.balanceReport(
      appState,
      settings(),
      BalanceReportSettings("Balance", None, Nil, showZeroAmountAccounts = false)
    )

    report.leftEntries.map(_.accName.get.fullPathStr) should contain allOf (
      "Assets:Cash", "Expenses:Food"
    )
    report.rightEntries.map(_.accName.get.fullPathStr) should contain("Income:Salary")
    report.leftEntries.map(_.render).mkString("\n") should include("Assets:Cash")
    report.rightEntries.map(_.render).mkString("\n") should include("Salary")
    report.totalLeft.trim should startWith("125.00")
    report.totalRight should include("-125.00 = Zero")
  }

  it should "hide zero accounts and collapse a single visible child" in {
    val appState = state(
      Seq("Assets") -> BigDecimal(0),
      Seq("Assets", "Cash") -> BigDecimal(10),
      Seq("Income") -> BigDecimal(-10),
      Seq("Unused") -> BigDecimal(0)
    )
    val report = Reports.balanceReport(
      appState,
      settings(),
      BalanceReportSettings("Balance", None, Nil, showZeroAmountAccounts = false)
    )

    report.leftEntries.map(_.accName.get.fullPathStr) should contain("Assets:Cash")
    report.leftEntries.map(_.accName.get.fullPathStr) should not contain "Assets"
    report.leftEntries.map(_.accName.get.fullPathStr) should not contain "Unused"
    report.leftEntries.map(_.render).mkString should include("Assets:Cash")
  }

  it should "show zero accounts when requested and support account filtering" in {
    val appState = state(
      Seq("Assets", "Cash") -> BigDecimal(10),
      Seq("Income", "Salary") -> BigDecimal(-10),
      Seq("Unused") -> BigDecimal(0)
    )
    val report = Reports.balanceReport(
      appState,
      settings(),
      BalanceReportSettings(
        "Balance",
        Some(Seq("Assets.*", "Unused")),
        Nil,
        showZeroAmountAccounts = true
      )
    )

    report.leftEntries.map(_.accName.get.fullPathStr) should contain allOf (
      "Assets", "Assets:Cash", "Unused"
    )
    report.rightEntries shouldBe empty
    report.totalRight should include("= 10")
  }

  it should "render parent balances and tree branches precisely" in {
    val appState = state(
      Seq("Assets") -> BigDecimal(5),
      Seq("Assets", "Cash") -> BigDecimal(10),
      Seq("Assets", "Investments", "Brokerage") -> BigDecimal(20),
      Seq("Assets", "Investments", "Retirement") -> BigDecimal(30),
      Seq("Income", "Salary") -> BigDecimal(-65)
    )
    val report = Reports.balanceReport(
      appState,
      settings(),
      BalanceReportSettings("Balance", None, Nil, showZeroAmountAccounts = false)
    )
    val entries = report.leftEntries.map(entry => entry.accName.get.fullPathStr -> entry.render).toMap

    entries("Assets") should include("Assets (5)")
    entries("Assets:Cash") should include(" ├╴Cash")
    entries("Assets:Investments") should include(" └╴Investments")
    entries("Assets:Investments:Brokerage") should include("    ├╴Brokerage")
    entries("Assets:Investments:Retirement") should include("    └╴Retirement")
  }
}