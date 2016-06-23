package co.uproot.abandon

import org.scalatest.FlatSpec
import org.scalatest.matchers.Matcher
import org.scalatest.Matchers
import org.scalatest.Inside
import ParserHelper._
import TestHelper._

class ParserTest extends FlatSpec with Matchers with Inside {

  "Parser" should "parse empty file" in {
    val testInput = ""
    val parseResult = AbandonParser.abandon(scanner(testInput))
    inside(parseResult) {
      case AbandonParser.Success(result, _) =>
        result should be('empty)
    }
  }

  it should "parse a simple transaction" in {
    val testInput = """
    2013/1/2
      Expense       200
      Cash          -200
    """
    val parseResult = AbandonParser.abandon(scanner(testInput))

    inside(parseResult) {
      case AbandonParser.Success(result, _) =>
        inside(result) {
          case List(txnGroup) =>
            inside(txnGroup) {
              case Transaction(date, posts, None, None, Nil) =>
                date should be(Date(2013, 1, 2))
                inside(posts) {
                  case List(Post(acc1, expr1, _), Post(acc2, expr2, _)) =>
                    acc1 should be (expenseAccount)
                    acc2 should be (cashAccount)
                    expr1 should be (Some(nlit(200)))
                    expr2 should be (Some(UnaryNegExpr(nlit(200))))
                }
            }
        }
    }
  }

  it should "parse a simple transaction with ISO 8601 date" in {
    val testInput = """
    2013-01-02
      Expense       200
      Cash          -200
    """
    val parseResult = AbandonParser.abandon(scanner(testInput))

    inside(parseResult) {
      case AbandonParser.Success(result, _) =>
        inside(result) {
          case List(txnGroup) =>
            inside(txnGroup) {
              case Transaction(date, posts, None, None, Nil) =>
                date should be(Date(2013, 1, 2))
                inside(posts) {
                  case List(Post(acc1, expr1, _), Post(acc2, expr2, _)) =>
                    acc1 should be (expenseAccount)
                    acc2 should be (cashAccount)
                    expr1 should be (Some(nlit(200)))
                    expr2 should be (Some(UnaryNegExpr(nlit(200))))
                }
            }
        }
    }
  }

  it should "parse human readable dates and more than two posts in a transaction" in {
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
              case Transaction(date, posts, None, None, Nil) =>
                date should be(Date(2013, 3, 1))
                inside(posts) {
                  case List(Post(acc1, expr1, None), Post(acc2, expr2, None), Post(acc3, expr3, None)) =>
                    acc1 should be (expenseAccount)
                    acc2 should be (cashAccount)
                    acc3 should be (bankAccount)
                    expr1 should be (Some(nlit(200)))
                    expr2 should be (Some(UnaryNegExpr(nlit(100))))
                    expr3 should be (Some(UnaryNegExpr(nlit(100))))
                }
            }
        }
    }
  }

  it should "parse short human readable dates and posts with empty value field" in {
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
              case Transaction(date, posts, None, None, Nil) =>
                date should be(Date(2013, 3, 1))
                inside(posts) {
                  case List(Post(acc1, expr1, _), Post(acc2, expr2, _), Post(acc3, expr3, _)) =>
                    acc1 should be (expenseAccount)
                    acc2 should be (cashAccount)
                    acc3 should be (bankAccount)
                    expr1 should be (Some(nlit(200)))
                    expr2 should be (Some(UnaryNegExpr(nlit(100))))
                    expr3 should be (None)
                }
            }
            inside(txnGroup2) {
              case Transaction(date, posts, None, None, Nil) =>
                date should be(Date(2013, 6, 1))
                inside(posts) {
                  case List(Post(acc1, expr1, None), Post(acc2, expr2, Some(comment)), Post(acc3, expr3, None)) =>
                    acc1 should be (expenseAccount)
                    acc2 should be (cashAccount)
                    acc3 should be (bankAccount)
                    expr1 should be (Some(nlit(200)))
                    expr2 should be (None)
                    expr3 should be (None)
                    comment should be (" Comment")
                }
            }
        }
    }
  }

  it should "parse a simple expression" in {
    val testInput = """
    2013/1/2
      Expense       -(200 + 40)
      Cash
    """

    val parseResult = AbandonParser.abandon(scanner(testInput))

    inside(parseResult) {
      case AbandonParser.Success(result, _) =>
        inside(result) {
          case List(txnGroup) =>
            inside(txnGroup) {
              case Transaction(date, posts, None, None, Nil) =>
                date should be(Date(2013, 1, 2))
                inside(posts) {
                  case List(Post(acc1, expr1, _), Post(acc2, expr2, _)) =>
                    acc1 should be (expenseAccount)
                    acc2 should be (cashAccount)
                    expr1 should be (Some(UnaryNegExpr(AddExpr(nlit(200), nlit(40)))))
                    expr2 should be (None)
                }
            }
        }
    }
  }

  it should "parse posts with zero value" in {
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
              case Transaction(date, posts, None, None, Nil) =>
                date should be(Date(2013, 1, 1))
                inside(posts) {
                  case List(Post(acc1, expr1, _), Post(acc2, expr2, _), Post(acc3, expr3, _), Post(acc4, expr4, _), Post(acc5, expr5, _), Post(acc6, expr6, _), Post(acc7, expr7, _)) =>
                    acc1 should be (expenseAccount)
                    acc2 should be (cashAccount)
                    acc3 should be (cashAccount)
                    acc4 should be (cashAccount)
                    acc5 should be (cashAccount)
                    acc6 should be (cashAccount)
                    expr1 should be (Some(nlit(200)))
                    expr2 should be (Some(nlit(0)))
                    expr3 should be (Some(nlit(0)))
                    expr4 should be (Some(nlit(0)))
                    expr5 should be (Some(nlit(0)))
                    expr6 should be (Some(AddExpr(nlit(0), nlit(200))))
                    expr7 should be (Some(MulExpr(AddExpr(nlit(200), nlit(10)), nlit(0))))
                }
            }
        }
    }
  }

  it should "parse a post with Unary plus(+) zero value" in {
    val testInput = """
     2013/9/1
      Expense       200
      Cash          +00
      Cash          +(0 + 10)
    """

    val parseResult = AbandonParser.abandon(scanner(testInput))

    inside(parseResult) {
      case AbandonParser.Success(result, _) =>
        inside(result) {
          case List(txnGroup) =>
            inside(txnGroup) {
              case Transaction(date, posts, None, None, Nil) =>
                date should be(Date(2013, 9, 1))
                inside(posts) {
                  case List(Post(acc1, expr1, _), Post(acc2, expr2, _), Post(acc3, expr3, _)) =>
                    acc1 should be (expenseAccount)
                    acc2 should be (cashAccount)
                    acc3 should be (cashAccount)
                    expr1 should be (Some(nlit(200)))
                    expr2 should be (Some(nlit(0)))
                    expr3 should be (Some(AddExpr(nlit(0), nlit(10))))
                }
            }
        }
    }
  }

  it should "parse a post with Unary minus(-) zero value" in {
    val testInput = """
      2013/9/1
       Expense       200
       Cash          -00
       Cash         -0.00
       Cash         -0 + (10*3)
     """

    val parseResult = AbandonParser.abandon(scanner(testInput))

    inside(parseResult) {
      case AbandonParser.Success(result, _) =>
        inside(result) {
          case List(txnGroup) =>
            inside(txnGroup) {
              case Transaction(date, posts, None, None, Nil) =>
                date should be(Date(2013, 9, 1))
                inside(posts) {
                  case List(Post(acc1, expr1, _), Post(acc2, expr2, _), Post(acc3, expr3, _), Post(acc4, expr4, _)) =>
                    acc1 should be(expenseAccount)
                    acc2 should be(cashAccount)
                    acc3 should be(cashAccount)
                    acc4 should be(cashAccount)
                    expr1 should be(Some(nlit(200)))
                    expr2 should be(Some(UnaryNegExpr((nlit(0)))))
                    expr3 should be(Some(UnaryNegExpr((nlit(0)))))
                    expr4 should be(Some(AddExpr(UnaryNegExpr(nlit(0)), MulExpr(nlit(10), nlit(3)))))
                }
            }
        }
    }
  }

  it should "parse a simple subtraction expression" in {
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
              case Transaction(date, posts, None, None, Nil) =>
                date should be(Date(2013, 1, 1))
                inside(posts) {
                  case List(Post(acc1, expr1, _), Post(acc2, expr2, _), Post(acc3, expr3, _), Post(acc4, expr4, _)) =>
                    acc1 should be (expenseAccount)
                    acc2 should be (cashAccount)
                    acc3 should be (cashAccount)
                    acc4 should be (cashAccount)
                    expr1 should be (Some(SubExpr(nlit(200), nlit(4))))
                    expr2 should be (Some(SubExpr(nlit(10), (SubExpr(nlit(5), nlit(6))))))
                    expr3 should be (Some(AddExpr(UnaryNegExpr(nlit(20)), MulExpr(nlit(3), UnaryNegExpr(nlit(4))))))
                    expr4 should be (Some(SubExpr(nlit(0), MulExpr(nlit(3), nlit(4)))))
                }
            }
        }
    }
  }

  def bd(s: String) = BigDecimal(s)

  it should "parse simple numeric expression" in {
    val tests = Map(
      "0001" -> (bd("1") -> nlit(1)),
      "000" -> (bd("0") -> nlit(0)),
      "0" -> (bd("0") -> nlit(0)),
      "-20" -> (bd("-20") -> UnaryNegExpr(nlit(20)))
    )
    val context = new co.uproot.abandon.EvaluationContext(Nil, Nil)

    tests foreach {
      case (testInput, expectedOutput) =>
        val parseResult = AbandonParser.numericParser(scanner(testInput))

        inside(parseResult) {
          case AbandonParser.Success(result, _) =>
            inside(result) {
              case ne: Expr =>
                context.evaluateBD(ne) should be (expectedOutput._1)
                ne should be(expectedOutput._2)
            }
        }
    }
  }

  it should "parse complex numeric expression" in {
    val tests = Map(
      "0001 + 10.00" -> (bd("11"), AddExpr(nlit(1), nlit(10.00))),
      "10.5 - (2.5 * 2)" -> (bd("5.5"), SubExpr(nlit(10.5), MulExpr(nlit(2.5), nlit(2)))),
      "10.5 - (2.5 * -2)" -> (bd("15.5"), SubExpr(nlit(10.5), MulExpr(nlit(2.5), UnaryNegExpr(nlit(2))))),
      "10.5 + -(2.5 * 2)" -> (bd("5.5"), AddExpr(nlit(10.5), UnaryNegExpr(MulExpr(nlit(2.5), nlit(2))))),
      "10.5 - (10.0 / 2)" -> (bd("5.5"), SubExpr(nlit(10.5), DivExpr(nlit(10.0), nlit(2)))),
      "10.5 + (10.0 / -2)" -> (bd("5.5"), AddExpr(nlit(10.5), DivExpr(nlit(10.0), UnaryNegExpr(nlit(2))))),
      "-20 + 30" -> (bd("10"), AddExpr(UnaryNegExpr(nlit(20)), nlit(30))),
      "-20 + 30*-5.0" -> (bd("-170"), AddExpr(UnaryNegExpr(nlit(20)), MulExpr(nlit(30), UnaryNegExpr(nlit(5)))))
    )

    val context = new co.uproot.abandon.EvaluationContext(Nil, Nil)

    tests foreach {
      case (testInput, expectedOutput) =>
        val parseResult = AbandonParser.numericParser(scanner(testInput))

        inside(parseResult) {
          case AbandonParser.Success(result, _) =>
            inside(result) {
              case ne: Expr =>
                context.evaluateBD(ne) should be (expectedOutput._1)
                ne should be(expectedOutput._2)
            }
        }
    }
  }

  it should "do something about divide by zero" in {
    val tests = Map(
      "1 / 0" -> (bd("0"), DivExpr(nlit(1), nlit(0)))
    )

    val context = new co.uproot.abandon.EvaluationContext(Nil, Nil)

    tests foreach {
      case (testInput, expectedOutput) =>
        val parseResult = AbandonParser.numericParser(scanner(testInput))

        inside(parseResult) {
          case AbandonParser.Success(result, _) =>
            inside(result) {
              case ne: Expr =>
                ne should be(expectedOutput._2)
                intercept[java.lang.ArithmeticException] {
                    context.evaluateBD(ne)
                }
            }
        }
    }
  }

  it should "ensure precedence of operators" in {
    val tests = Map(
      "1 + 2 * 3" -> (bd("7"), AddExpr(nlit(1), MulExpr(nlit(2), nlit(3)))),
      "1 * 2 + 3" -> (bd("5"), AddExpr(MulExpr(nlit(1), nlit(2)), nlit(3))),
      "2 + 1 / 4" -> (bd("2.25"), AddExpr(nlit(2), DivExpr(nlit(1), nlit(4)))),
      "1 / 2 + 3" -> (bd("3.5"), AddExpr(DivExpr(nlit(1), nlit(2)), nlit(3))),
      "1 * 2 + 3 * 4 - 2" -> (bd("12"), SubExpr(AddExpr(MulExpr(nlit(1), nlit(2)), MulExpr(nlit(3), nlit(4))), nlit(2))),
      "1 + 2 * 3 * 4 - 2" -> (bd("23"), SubExpr(AddExpr(nlit(1), MulExpr(MulExpr(nlit(2), nlit(3)), nlit(4))), nlit(2))),
      "1 / 2 + 3 / 4 - 2" -> (bd("-0.75"), SubExpr(AddExpr(DivExpr(nlit(1), nlit(2)), DivExpr(nlit(3), nlit(4))), nlit(2))),
      "1 + 2 / 5 / 4 - 2" -> (bd("-0.9"), SubExpr(AddExpr(nlit(1), DivExpr(DivExpr(nlit(2), nlit(5)), nlit(4))), nlit(2))),
      "1 + 2 * -3 * 4 - 2" -> (bd("-25"), SubExpr(AddExpr(nlit(1), MulExpr(MulExpr(nlit(2), UnaryNegExpr(nlit(3))), nlit(4))), nlit(2))),
      "1 / 2 * 3" -> (bd("1.5"), MulExpr(DivExpr(nlit(1), nlit(2)), nlit(3)))
    )

    val context = new co.uproot.abandon.EvaluationContext(Nil, Nil)

    tests foreach {
      case (testInput, expectedOutput) =>
        val parseResult = AbandonParser.numericParser(scanner(testInput))

        inside(parseResult) {
          case AbandonParser.Success(result, _) =>
            inside(result) {
              case ne: Expr =>
                context.evaluateBD(ne) should be (expectedOutput._1)
                ne should be(expectedOutput._2)
            }
        }
    }
  }

  it should "handle parenthesis correctly" in {
    val tests = Map(
      "1 + (2 * 3)" -> (bd("7"), AddExpr(nlit(1), MulExpr(nlit(2), nlit(3)))),
      "(1 + 2) * 3" -> (bd("9"), MulExpr(AddExpr(nlit(1), nlit(2)), nlit(3))),
      "1 * (2 + 3)" -> (bd("5"), MulExpr(nlit(1), AddExpr(nlit(2), nlit(3)))),
      "1 * (2 + 3) * 4 - 2" -> (bd("18"), SubExpr(MulExpr(MulExpr(nlit(1), AddExpr(nlit(2), nlit(3))), nlit(4)), nlit(2)))
    )

    val context = new co.uproot.abandon.EvaluationContext(Nil, Nil)

    tests foreach {
      case (testInput, expectedOutput) =>
        val parseResult = AbandonParser.numericParser(scanner(testInput))

        inside(parseResult) {
          case AbandonParser.Success(result, _) =>
            inside(result) {
              case ne: Expr =>
                context.evaluateBD(ne) should be (expectedOutput._1)
                ne should be(expectedOutput._2)
            }
        }
    }
  }

  it should "parse month names in date" in {
    val testInput = List(
      "2013/January/1",
      "2013/january/1",
      "2013/jaNuAry/1",
      "2013/Jan/1",
      "2013/jan/1"
    )

    testInput foreach {input =>
      val parseResult = AbandonParser.dateFrag(scanner(input))

      inside(parseResult) {
        case AbandonParser.Success(date, _) =>
          date should be(Date(2013, 1, 1))
      }
    }

  }

  it should "parse dates in ISO 8601" in {
    val testInput = List(
      "2013-01-02"
    )

    testInput foreach {input =>
      val parseResult = AbandonParser.isoDateFrag(scanner(input))

      inside(parseResult) {
        case AbandonParser.Success(date, _) =>
          date should be(Date(2013, 1, 2))
      }
    }

  }
}
