package co.uproot.abandon

import scala.util.parsing.input.PagedSeqReader
import scala.collection.immutable.PagedSeq

object TestHelper {
  def reader(s: String) = new PagedSeqReader(PagedSeq.fromStrings(collection.immutable.Seq(s)))
  def mkScanner(r: PagedSeqReader) = new AbandonParser.lexical.Scanner(r)
  def scanner(s: String) = mkScanner(reader(s))
  def nlit(n: BigDecimal) = NumericLiteralExpr(n)

  val expenseAccount = AccountName(Seq("Expense"))
  val cashAccount = AccountName(Seq("Cash"))
  val bankAccount = AccountName(Seq("Bank", "Current"))
}