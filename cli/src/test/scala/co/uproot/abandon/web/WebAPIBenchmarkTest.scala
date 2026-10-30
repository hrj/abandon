package co.uproot.abandon.web

import co.uproot.abandon._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class WebAPIBenchmarkTest extends AnyFlatSpec with Matchers {

  def buildBenchmarkAppState(numMonths: Int, numAccounts: Int, postsPerMonth: Int): AppState = {
    val accState = new AccountState()
    val appState = AppState(accState)

    for {
      m <- 1 to numMonths
      year = 2020 + (m - 1) / 12
      month = ((m - 1) % 12) + 1
      p <- 1 to postsPerMonth
    } {
      val day = (p % 28) + 1
      val date = Date(year, month, day)
      val accIdx = p % numAccounts
      val acc1 = AccountName(Seq("Expenses", s"Category$accIdx"))
      val acc2 = AccountName(Seq("Assets", "Bank"))

      val amount = BigDecimal(p * 10)
      val dp1 = DetailedPost(acc1, amount, Some(s"Expense $p"))
      val dp2 = DetailedPost(acc2, -amount, Some(s"Payment $p"))

      val txn = Transaction(null, date, Nil, None, Some("Payee"), Nil)
      val pg = new PostGroup(Seq(dp1, dp2), txn, date, None, Some("Payee"), Nil)
      accState.updateAmounts(pg)
    }

    appState
  }

  "WebAPI.makeReport" should "execute efficiently on standard datasets" in {
    val startDate = Date(2020, 1, 1)
    val appState = buildBenchmarkAppState(numMonths = 24, numAccounts = 50, postsPerMonth = 200)

    // Warmup
    for (_ <- 1 to 5) {
      WebAPI.makeReport(startDate, appState, None, showInactiveAccounts = true)
    }

    // Benchmark run
    val runs = 10
    val start = System.currentTimeMillis()
    for (_ <- 1 to runs) {
      val report = WebAPI.makeReport(startDate, appState, None, showInactiveAccounts = true)
      report.length should be > 0
    }
    val elapsed = System.currentTimeMillis() - start
    val avgMs = elapsed.toDouble / runs
    println(s"[BENCHMARK] Average WebAPI.makeReport execution time: ${avgMs} ms over $runs runs")
  }
}
