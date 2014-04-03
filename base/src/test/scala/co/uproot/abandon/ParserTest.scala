package co.uproot.abandon

import org.scalatest.FlatSpec
import org.scalatest.matchers.Matcher
import org.scalatest.Matchers
import scala.util.parsing.input.PagedSeqReader
import scala.collection.immutable.PagedSeq
import org.scalatest.Inside

class ParserTest extends FlatSpec with Matchers with Inside {

  private def reader(s: String) = new PagedSeqReader(PagedSeq.fromStrings(collection.immutable.Seq(s)))
  private def mkScanner(r: PagedSeqReader) = new AbandonParser.lexical.Scanner(r)
  private def scanner(s: String) = mkScanner(reader(s))

  private val expenseAccount = AccountName(Seq("Expense"))
  private val cashAccount = AccountName(Seq("Cash"))
  private val bankAccount = AccountName(Seq("Bank", "Current"))

  "parser" should "parse empty file" in {
    val testInput = ""
    val parseResult = AbandonParser.abandon(scanner(testInput))
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
    val parseResult = AbandonParser.abandon(scanner(testInput))

    inside(parseResult) {
      case AbandonParser.Success(result, _) =>
        inside(result) {
          case List(txnGroup) =>
            inside(txnGroup) {
              case Transaction(date, txns, None, None, Nil) =>
                date should be(Date(2013, 1, 1))
                inside(txns) {
                  case List(SingleTransaction(acc1, expr1, _), SingleTransaction(acc2, expr2, _)) =>
                    acc1 should be (expenseAccount)
                    acc2 should be (cashAccount)
                    expr1 should be (Some(NumericLiteralExpr(200)))
                    expr2 should be (Some(UnaryNegExpr(NumericLiteralExpr(200))))
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
      Bank:Current  -100
    """
    val parseResult = AbandonParser.abandon(scanner(testInput))

    inside(parseResult) {
      case AbandonParser.Success(result, _) =>
        inside(result) {
          case List(txnGroup) =>
            inside(txnGroup) {
              case Transaction(date, txns, None, None, Nil) =>
                date should be(Date(2013, 3, 1))
                inside(txns) {
                  case List(SingleTransaction(acc1, expr1, None), SingleTransaction(acc2, expr2, None), SingleTransaction(acc3, expr3, None)) =>
                    acc1 should be (expenseAccount)
                    acc2 should be (cashAccount)
                    acc3 should be (bankAccount)
                    expr1 should be (Some(NumericLiteralExpr(200)))
                    expr2 should be (Some(UnaryNegExpr(NumericLiteralExpr(100))))
                    expr3 should be (Some(UnaryNegExpr(NumericLiteralExpr(100))))
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
      Bank:Current

    2013/Jun/1
      Expense       200
      Cash              ; Comment
      Bank:Current
    """
    val parseResult = AbandonParser.abandon(new AbandonParser.lexical.Scanner(reader(testInput)))

    inside(parseResult) {
      case AbandonParser.Success(result, _) =>
        inside(result) {
          case List(txnGroup1, txnGroup2) =>
            inside(txnGroup1) {
              case Transaction(date, txns, None, None, Nil) =>
                date should be(Date(2013, 3, 1))
                inside(txns) {
                  case List(SingleTransaction(acc1, expr1, _), SingleTransaction(acc2, expr2, _), SingleTransaction(acc3, expr3, _)) =>
                    acc1 should be (expenseAccount)
                    acc2 should be (cashAccount)
                    acc3 should be (bankAccount)
                    expr1 should be (Some(NumericLiteralExpr(200)))
                    expr2 should be (Some(UnaryNegExpr(NumericLiteralExpr(100))))
                    expr3 should be (None)
                }
            }
            inside(txnGroup2) {
              case Transaction(date, txns, None, None, Nil) =>
                date should be(Date(2013, 6, 1))
                inside(txns) {
                  case List(SingleTransaction(acc1, expr1, None), SingleTransaction(acc2, expr2, Some(comment)), SingleTransaction(acc3, expr3, None)) =>
                    acc1 should be (expenseAccount)
                    acc2 should be (cashAccount)
                    acc3 should be (bankAccount)
                    expr1 should be (Some(NumericLiteralExpr(200)))
                    expr2 should be (None)
                    expr3 should be (None)
                    comment should be (" Comment")
                }
            }
        }
    }
  }

  "parser" should "parse a simple expression" in {
    val testInput = """
    2013/1/1
      Expense       -(200 + 40)
      Cash
    """
     
    val parseResult = AbandonParser.abandon(scanner(testInput))

    inside(parseResult) {
      case AbandonParser.Success(result, _) =>
        inside(result) {
          case List(txnGroup) =>
            inside(txnGroup) {
              case Transaction(date, txns, None, None, Nil) =>
                date should be(Date(2013, 1, 1))
                inside(txns) {
                  case List(SingleTransaction(acc1, expr1, _), SingleTransaction(acc2, expr2, _)) =>
                    acc1 should be (expenseAccount)
                    acc2 should be (cashAccount)
                    expr1 should be (Some(UnaryNegExpr(AddExpr(NumericLiteralExpr(200),NumericLiteralExpr(40)))))
                    expr2 should be (None)
                }
            }
        }
    }
  }
  
  "parser" should "parse a transaction with zero value" in {
    val testInput = """
    2013/1/1
      Expense       200
      Cash          000
      Cash          0
      Cash          0.00
      Cash          00.00
      Cash 			0+200
      Cash 			(200+10)*0
    """

    val parseResult = AbandonParser.abandon(scanner(testInput))

    inside(parseResult) {
      case AbandonParser.Success(result, _) =>
        inside(result) {
          case List(txnGroup) =>
            inside(txnGroup) {
              case Transaction(date, txns, None, None, Nil) =>
                date should be(Date(2013, 1, 1))
                inside(txns) {
                  case List(SingleTransaction(acc1, expr1, _), SingleTransaction(acc2, expr2, _),SingleTransaction(acc3, expr3, _),SingleTransaction(acc4, expr4, _),SingleTransaction(acc5, expr5, _),SingleTransaction(acc6, expr6, _),SingleTransaction(acc7, expr7, _)) =>
                    acc1 should be (expenseAccount)
                    acc2 should be (cashAccount)
                    acc3 should be (cashAccount)
                    acc4 should be (cashAccount)
                    acc5 should be (cashAccount)
                    acc6 should be (cashAccount)
                    expr1 should be (Some(NumericLiteralExpr(200)))
                    expr2 should be (Some(NumericLiteralExpr(0)))
                    expr3 should be (Some(NumericLiteralExpr(0)))
                    expr4 should be (Some(NumericLiteralExpr(0)))
                    expr5 should be (Some(NumericLiteralExpr(0)))
                    expr6 should be (Some(AddExpr(NumericLiteralExpr(0),NumericLiteralExpr(200))))
                    expr7 should be (Some(MulExpr(AddExpr(NumericLiteralExpr(200),NumericLiteralExpr(10)),NumericLiteralExpr(0))))
                }
            }
         }
    }
  }

  "parser" should "parse a transaction with Unary plus(+) zero value" in {
    val testInput = """
     2013/9/1
      Expense       200
      Cash          +00
      Cash          +(0 + 10)
    """

    pending

    val parseResult = AbandonParser.abandon(scanner(testInput))

    inside(parseResult) {
      case AbandonParser.Success(result, _) =>
        inside(result) {
          case List(txnGroup) =>
              inside(txnGroup) {
              case Transaction(date, txns, None, None, Nil) =>
                date should be(Date(2013, 9, 1))
                inside(txns) {
                  case List(SingleTransaction(acc1, expr1, _), SingleTransaction(acc2, expr2, _),SingleTransaction(acc3, expr3, _)) =>
                    acc1 should be (expenseAccount)
                    acc2 should be (cashAccount)
                    acc3 should be (cashAccount)
                    expr1 should be (Some(NumericLiteralExpr(200)))
                    expr2 should be (Some(NumericLiteralExpr(0)))
                    expr3 should be (Some(AddExpr(NumericLiteralExpr(0),NumericLiteralExpr(10))))
                	}
                }
            }
        }
    }

  "parser" should "parse a transaction with Unary minus(-) zero value" in {
    val testInput = """
      2013/9/1
       Expense       200
       Cash          -00
       Cash         -0.00
       Cash         -0 + (10*3)
     """

    pending

    val parseResult = AbandonParser.abandon(scanner(testInput))

    inside(parseResult) {
      case AbandonParser.Success(result, _) =>
        inside(result) {
          case List(txnGroup) =>
            inside(txnGroup) {
              case Transaction(date, txns, None, None, Nil) =>
                date should be(Date(2013, 9, 1))
                inside(txns) {
                  case List(SingleTransaction(acc1, expr1, _), SingleTransaction(acc2, expr2, _), SingleTransaction(acc3, expr3, _), SingleTransaction(acc4, expr4, _)) =>
                    acc1 should be(expenseAccount)
                    acc2 should be(cashAccount)
                    acc3 should be(cashAccount)
                    acc4 should be(cashAccount)
                    expr1 should be(Some(NumericLiteralExpr(200)))
                    expr2 should be(Some(UnaryNegExpr((NumericLiteralExpr(0)))))
                    expr3 should be(Some(UnaryNegExpr((NumericLiteralExpr(0)))))
                    expr4 should be(Some(AddExpr(UnaryNegExpr(NumericLiteralExpr(0)), MulExpr(NumericLiteralExpr(10), NumericLiteralExpr(3)))))
                }
            }
        }
    }
  }

  "parser" should "parse a simple subtraction expression" in {
    val testInput = """
    2013/1/1
      Expense       200-4
      Cash        10-(5-6)
      Cash        -20+(3*(-4))
      Cash        0-(3*4)
    """

    val parseResult = AbandonParser.abandon(scanner(testInput))

    inside(parseResult) {
      case AbandonParser.Success(result, _) =>
        inside(result) {
          case List(txnGroup1) =>
            inside(txnGroup1) {
              case Transaction(date, txns, None, None, Nil) =>
                date should be(Date(2013, 1, 1))
                inside(txns) {
                  case List(SingleTransaction(acc1, expr1, _), SingleTransaction(acc2, expr2, _),SingleTransaction(acc3, expr3, _),SingleTransaction(acc4, expr4, _)) =>
                    acc1 should be (expenseAccount)
                    acc2 should be (cashAccount)
                    acc3 should be (cashAccount)
                    acc4 should be (cashAccount)
                    expr1 should be (Some(SubExpr(NumericLiteralExpr(200),NumericLiteralExpr(4))))
                    expr2 should be (Some(SubExpr(NumericLiteralExpr(10),(SubExpr(NumericLiteralExpr(5),NumericLiteralExpr(6))))))
                    expr3 should be (Some(UnaryNegExpr(AddExpr(NumericLiteralExpr(20),MulExpr(NumericLiteralExpr(3),UnaryNegExpr(NumericLiteralExpr(4)))))))
                    expr4 should be (Some(SubExpr(NumericLiteralExpr(0),MulExpr(NumericLiteralExpr(3),NumericLiteralExpr(4)))))
                }
            }
        }
    }
  }

  import ASTHelper.NumericExpr

  "parser" should "parse simple numeric expression" in {
    val tests = Map(
      "0001" -> NumericLiteralExpr(1),
      "000" -> NumericLiteralExpr(0),
      "0" -> NumericLiteralExpr(0),
      "-20" -> UnaryNegExpr(NumericLiteralExpr(20))
    )

    tests foreach {
      case (testInput, expectedOutput) =>
        val parseResult = AbandonParser.numericParser(scanner(testInput))

        inside(parseResult) {
          case AbandonParser.Success(result, _) =>
            inside(result) {
              case ne:NumericExpr =>
                ne should be(expectedOutput)
            }
        }
    }
  }

}