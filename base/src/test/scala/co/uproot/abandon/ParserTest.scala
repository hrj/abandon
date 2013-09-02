package co.uproot.abandon

import org.scalatest.FlatSpec
import org.scalatest.exceptions.TestFailedException
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.ShouldMatchers
import scala.util.parsing.input.PagedSeqReader
import scala.collection.immutable.PagedSeq
import org.scalatest.Inside

class ParserTest extends FlatSpec with ShouldMatchers with Inside {

  def reader(s: String) = new PagedSeqReader(PagedSeq.fromStrings(collection.immutable.Seq(s)))

  "parser" should "parse empty file" in {
    val testInput = ""
    val parseResult = AbandonParser.abandon(new AbandonParser.lexical.Scanner(reader(testInput)))
    inside(parseResult) {
      case AbandonParser.Success(result, _) =>
        result should be('empty)
    }
  }

  "parser" should "parse a simple transaction" in {
    val testInput = """
    2013/1/1
      Expense       200
      Cash          -200
    """
    val parseResult = AbandonParser.abandon(new AbandonParser.lexical.Scanner(reader(testInput)))

    inside(parseResult) {
      case AbandonParser.Success(result, _) =>
        inside(result) {
          case List(txnGroup) =>
            inside(txnGroup) {
              case Transaction(date, txns, None, None, Nil) =>
                date should be(Date(2013, 1, 1))
                inside(txns) {
                  case List(SingleTransaction(acc1, expr1, _), SingleTransaction(acc2, expr2, _)) =>
                    acc1 should be (AccountName(Seq("Expense")))
                    acc2 should be (AccountName(Seq("Cash")))
                    expr1 should be (Some(NumericLiteralExpr(200)))
                    expr2 should be (Some(NumericLiteralExpr(-200)))
                }
            }
        }
    }
  }

  "parser" should "parse human readable dates and more than two transactions in a group" in {
    val testInput = """
    2013/March/1
      Expense       200
      Cash          -100
      Bank          -100
    """
    val parseResult = AbandonParser.abandon(new AbandonParser.lexical.Scanner(reader(testInput)))

    inside(parseResult) {
      case AbandonParser.Success(result, _) =>
        inside(result) {
          case List(txnGroup) =>
            inside(txnGroup) {
              case Transaction(date, txns, None, None, Nil) =>
                date should be(Date(2013, 3, 1))
                inside(txns) {
                  case List(SingleTransaction(acc1, expr1, _), SingleTransaction(acc2, expr2, _), SingleTransaction(acc3, expr3, _)) =>
                    acc1 should be (AccountName(Seq("Expense")))
                    acc2 should be (AccountName(Seq("Cash")))
                    acc3 should be (AccountName(Seq("Bank")))
                    expr1 should be (Some(NumericLiteralExpr(200)))
                    expr2 should be (Some(NumericLiteralExpr(-100)))
                    expr3 should be (Some(NumericLiteralExpr(-100)))
                }
            }
        }
    }
  }

  "parser" should "parse short human readable dates and transactions with empty value field" in {
    val testInput = """
    2013/Mar/1
      Expense       200
      Cash          -100
      Bank
    """
    val parseResult = AbandonParser.abandon(new AbandonParser.lexical.Scanner(reader(testInput)))

    inside(parseResult) {
      case AbandonParser.Success(result, _) =>
        inside(result) {
          case List(txnGroup) =>
            inside(txnGroup) {
              case Transaction(date, txns, None, None, Nil) =>
                date should be(Date(2013, 3, 1))
                inside(txns) {
                  case List(SingleTransaction(acc1, expr1, _), SingleTransaction(acc2, expr2, _), SingleTransaction(acc3, expr3, _)) =>
                    acc1 should be (AccountName(Seq("Expense")))
                    acc2 should be (AccountName(Seq("Cash")))
                    acc3 should be (AccountName(Seq("Bank")))
                    expr1 should be (Some(NumericLiteralExpr(200)))
                    expr2 should be (Some(NumericLiteralExpr(-100)))
                    expr3 should be (None)
                }
            }
        }
    }
  }

}