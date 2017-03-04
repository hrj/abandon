package co.uproot.abandon

import org.scalatest.FlatSpec
import org.scalatest.matchers.Matcher
import org.scalatest.Matchers
import org.scalatest.Inside
import ParserHelper._
import TestHelper._
import org.scalactic.Equality

// TODO: use "shouldEqual" when comparing expressions. It will ensure that the position is also checked as defined in ExprEquality

class ParserTest extends FlatSpec with Matchers with Inside {
  val parser = new AbandonParser(None)

  "Parser" should "parse empty file" in {
    val testInput = ""
    val parseResult = parser.abandon(scanner(testInput))
    inside(parseResult) {
      case parser.Success(result, _) =>
        result.entries should be('empty)
    }
  }

  implicit object ExprEquality extends Equality[Option[Expr]] {
    def areEqual(a: Option[Expr], b: Any) = {
      a match {
        case Some(ae) =>
          b match {
            case Some(be: Expr) =>
              val posMatch = (ae.pos, be.pos) match {
                case (Some(ap), Some(bp)) =>
                  ap.pos.column == bp.pos.column && ap.pos.line == bp.pos.line
                case (None, None) =>
                  true
                case _ => false
              }
              if (!posMatch) {
                println("Failed to match positions: " + ae.pos + " and " + be.pos)
              }
              (ae equals be) && posMatch
            case _ =>
              false
          }
        case None =>
          b match {
            case None => true
            case _ => false
          }
      }
    }
  }

  def identifierExpr(s: String, pos: Option[InputPosition] = None) = {
    IdentifierExpr(s)(pos)
  }

  def addExpr(e1: Expr, e2: Expr, pos: Option[InputPosition] = None) = {
    AddExpr(e1, e2)(pos)
  }

  def subExpr(e1: Expr, e2: Expr, pos: Option[InputPosition] = None) = {
    SubExpr(e1, e2)(pos)
  }

  def mulExpr(e1: Expr, e2: Expr, pos: Option[InputPosition] = None) = {
    MulExpr(e1, e2)(pos)
  }

  def divExpr(e1: Expr, e2: Expr, pos: Option[InputPosition] = None) = {
    DivExpr(e1, e2)(pos)
  }

  def unaryNegExpr(e1: Expr, pos: Option[InputPosition] = None) = {
    UnaryNegExpr(e1)(pos)
  }

  it should "parse a simple transaction" in {
    implicit val testInput = """
    2013/1/2
      Expense       200
      Cash          -200
    """
    val parseResult = parser.abandon(scanner(testInput))

    inside(parseResult) {
      case parser.Success(result, _) =>
        inside(result.entries) {
          case List(txnGroup) =>
            inside(txnGroup) {
              case Transaction(_, date, posts, None, None, Nil) =>
                date should be(Date(2013, 1, 2))
                inside(posts) {
                  case List(Post(acc1, expr1, _), Post(acc2, expr2, _)) =>
                    acc1 should be (expenseAccount)
                    acc2 should be (cashAccount)
                    expr1 shouldEqual Some(nlit(200, 34))
                    expr2 shouldEqual Some(UnaryNegExpr(nlit(200, 44))(mkPos(58)))
                }
            }
        }
    }
  }

  it should "parse a simple transaction with ISO 8601 date" in {
    implicit val testInput = """
    2013-01-02
      Expense       200
      Cash          -200
    """
    val parseResult = parser.abandon(scanner(testInput))

    inside(parseResult) {
      case parser.Success(result, _) =>
        inside(result.entries) {
          case List(txnGroup) =>
            inside(txnGroup) {
              case Transaction(_, date, posts, None, None, Nil) =>
                date should be(Date(2013, 1, 2))
                inside(posts) {
                  case List(Post(acc1, expr1, _), Post(acc2, expr2, _)) =>
                    acc1 should be (expenseAccount)
                    acc2 should be (cashAccount)
                    expr1 shouldEqual Some(nlit(200, 36))
                    expr2 shouldEqual Some(UnaryNegExpr(nlit(200))(mkPos(60)))
                }
            }
        }
    }
  }

  it should "parse human readable dates and more than two posts in a transaction" in {
    implicit val testInput = """
    2013/March/1
      Expense       200
      Cash          -100
      Bank:Current  -100
    """
    val parseResult = parser.abandon(scanner(testInput))

    inside(parseResult) {
      case parser.Success(result, _) =>
        inside(result.entries) {
          case List(txnGroup) =>
            inside(txnGroup) {
              case Transaction(_, date, posts, None, None, Nil) =>
                date should be(Date(2013, 3, 1))
                inside(posts) {
                  case List(Post(acc1, expr1, None), Post(acc2, expr2, None), Post(acc3, expr3, None)) =>
                    acc1 should be (expenseAccount)
                    acc2 should be (cashAccount)
                    acc3 should be (bankAccount)
                    expr1 should be (Some(nlit(200)))
                    expr2 should be (Some(UnaryNegExpr(nlit(100))(mkPos(1))))
                    expr3 should be (Some(UnaryNegExpr(nlit(100))(mkPos(1))))
                }
            }
        }
    }
  }

  it should "parse short human readable dates and posts with empty value field" in {
    implicit val testInput = """
    2013/Mar/1
      Expense       200
      Cash          -100
      Bank:Current

    2013/Jun/1
      Expense       200
      Cash              ; Comment
      Bank:Current
    """
    val parseResult = parser.abandon(parser.scanner(testInput))

    inside(parseResult) {
      case parser.Success(result, _) =>
        inside(result.entries) {
          case List(txnGroup1, txnGroup2) =>
            inside(txnGroup1) {
              case Transaction(_, date, posts, None, None, Nil) =>
                date should be(Date(2013, 3, 1))
                inside(posts) {
                  case List(Post(acc1, expr1, _), Post(acc2, expr2, _), Post(acc3, expr3, _)) =>
                    acc1 should be (expenseAccount)
                    acc2 should be (cashAccount)
                    acc3 should be (bankAccount)
                    expr1 should be (Some(nlit(200)))
                    expr2 should be (Some(UnaryNegExpr(nlit(100))(mkPos(1))))
                    expr3 should be (None)
                }
            }
            inside(txnGroup2) {
              case Transaction(_, date, posts, None, None, Nil) =>
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
    implicit val testInput = """
    2013/1/2
      Expense       -(200 + 40)
      Cash
    """

    val parseResult = parser.abandon(scanner(testInput))

    inside(parseResult) {
      case parser.Success(result, _) =>
        inside(result.entries) {
          case List(txnGroup) =>
            inside(txnGroup) {
              case Transaction(_, date, posts, None, None, Nil) =>
                date should be(Date(2013, 1, 2))
                inside(posts) {
                  case List(Post(acc1, expr1, _), Post(acc2, expr2, _)) =>
                    acc1 should be (expenseAccount)
                    acc2 should be (cashAccount)
                    expr1 should be (Some(UnaryNegExpr(AddExpr(nlit(200), nlit(40))(mkPos(1)))(mkPos(1))))
                    expr2 should be (None)
                }
            }
        }
    }
  }

  it should "parse posts with zero value" in {
    implicit val testInput = """
    2013/1/1
      Expense       200
      Cash          000
      Cash          0
      Cash          0.00
      Cash          00.00
      Cash 			0+200
      Cash 			(200+10)*0
    """

    val parseResult = parser.abandon(scanner(testInput))

    inside(parseResult) {
      case parser.Success(result, _) =>
        inside(result.entries) {
          case List(txnGroup) =>
            inside(txnGroup) {
              case Transaction(_, date, posts, None, None, Nil) =>
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
                    expr6 should be (Some(AddExpr(nlit(0), nlit(200))(mkPos(0))))
                    expr7 should be (Some(MulExpr(AddExpr(nlit(200), nlit(10))(mkPos(0)), nlit(0))(mkPos(0))))
                }
            }
        }
    }
  }

  it should "parse a post with Unary plus(+) zero value" in {
    implicit val testInput = """
     2013/9/1
      Expense       200
      Cash          +00
      Cash          +(0 + 10)
    """

    val parseResult = parser.abandon(scanner(testInput))

    inside(parseResult) {
      case parser.Success(result, _) =>
        inside(result.entries) {
          case List(txnGroup) =>
            inside(txnGroup) {
              case Transaction(_, date, posts, None, None, Nil) =>
                date should be(Date(2013, 9, 1))
                inside(posts) {
                  case List(Post(acc1, expr1, _), Post(acc2, expr2, _), Post(acc3, expr3, _)) =>
                    acc1 should be (expenseAccount)
                    acc2 should be (cashAccount)
                    acc3 should be (cashAccount)
                    expr1 should be (Some(nlit(200)))
                    expr2 should be (Some(nlit(0)))
                    expr3 should be (Some(AddExpr(nlit(0), nlit(10))(mkPos(0))))
                }
            }
        }
    }
  }

  it should "parse a post with Unary minus(-) zero value" in {
    implicit val testInput = """
      2013/9/1
       Expense       200
       Cash          -00
       Cash         -0.00
       Cash         -0 + (10*3)
     """

    val parseResult = parser.abandon(scanner(testInput))

    inside(parseResult) {
      case parser.Success(result, _) =>
        inside(result.entries) {
          case List(txnGroup) =>
            inside(txnGroup) {
              case Transaction(_, date, posts, None, None, Nil) =>
                date should be(Date(2013, 9, 1))
                inside(posts) {
                  case List(Post(acc1, expr1, _), Post(acc2, expr2, _), Post(acc3, expr3, _), Post(acc4, expr4, _)) =>
                    acc1 should be(expenseAccount)
                    acc2 should be(cashAccount)
                    acc3 should be(cashAccount)
                    acc4 should be(cashAccount)
                    expr1 should be(Some(nlit(200)))
                    expr2 should be(Some(UnaryNegExpr(nlit(0))(mkPos(0))))
                    expr3 should be(Some(UnaryNegExpr(nlit(0))(mkPos(0))))
                    expr4 should be(Some(AddExpr(UnaryNegExpr(nlit(0))(mkPos(0)), MulExpr(nlit(10), nlit(3))(mkPos(0)))(mkPos(0))))
                }
            }
        }
    }
  }

  it should "parse a simple subtraction expression" in {
    implicit val testInput = """
    2013/1/1
      Expense       200-4
      Cash        10-(5-6)
      Cash        -20+(3*(-4))
      Cash        0-(3*4)
    """

    val parseResult = parser.abandon(scanner(testInput))

    inside(parseResult) {
      case parser.Success(result, _) =>
        inside(result.entries) {
          case List(txnGroup1) =>
            inside(txnGroup1) {
              case Transaction(_, date, posts, None, None, Nil) =>
                date should be(Date(2013, 1, 1))
                inside(posts) {
                  case List(Post(acc1, expr1, _), Post(acc2, expr2, _), Post(acc3, expr3, _), Post(acc4, expr4, _)) =>
                    acc1 should be (expenseAccount)
                    acc2 should be (cashAccount)
                    acc3 should be (cashAccount)
                    acc4 should be (cashAccount)
                    expr1 should be (Some(SubExpr(nlit(200), nlit(4))(mkPos(0))))
                    expr2 should be (Some(SubExpr(nlit(10), (SubExpr(nlit(5), nlit(6))(mkPos(0))))(mkPos(0))))
                    expr3 should be (Some(AddExpr(UnaryNegExpr(nlit(20))(mkPos(0)), MulExpr(nlit(3), UnaryNegExpr(nlit(4))(mkPos(0)))(mkPos(0)))(mkPos(0))))
                    expr4 should be (Some(SubExpr(nlit(0), MulExpr(nlit(3), nlit(4))(mkPos(0)))(mkPos(0))))
                }
            }
        }
    }
  }

  private def bd(s: String) = BigDecimal(s)
  private val emptyScope = Scope(Nil, None)
  private val emptyContext = new co.uproot.abandon.EvaluationContext(emptyScope, Nil)

  it should "parse simple numeric expression" in {
    val tests = Map(
      "0001" -> (bd("1") -> nlit(1, 0)("0001")),
      "000" -> (bd("0") -> nlit(0, 0)("000")),
      "0" -> (bd("0") -> nlit(0, 0)("0")),
      "-20" -> (bd("-20") -> unaryNegExpr(nlit(20)))
    )

    tests foreach {
      case (testInput, expectedOutput) =>
        val parseResult = parser.numericParser(scanner(testInput))

        inside(parseResult) {
          case parser.Success(result, _) =>
            inside(result) {
              case ne: Expr =>
                emptyContext.evaluateBD(ne) should be (expectedOutput._1)
                ne should be(expectedOutput._2)
            }
        }
    }
  }

  it should "parse complex numeric expression" in {
    val tests = Map(
      "0001 + 10.00" -> (bd("11"), addExpr(nlit(1), nlit(10.00))),
      "10.5 - (2.5 * 2)" -> (bd("5.5"), subExpr(nlit(10.5), mulExpr(nlit(2.5), nlit(2)))),
      "10.5 - (2.5 * -2)" -> (bd("15.5"), subExpr(nlit(10.5), mulExpr(nlit(2.5), unaryNegExpr(nlit(2))))),
      "10.5 + -(2.5 * 2)" -> (bd("5.5"), addExpr(nlit(10.5), unaryNegExpr(mulExpr(nlit(2.5), nlit(2))))),
      "10.5 - (10.0 / 2)" -> (bd("5.5"), subExpr(nlit(10.5), divExpr(nlit(10.0), nlit(2)))),
      "10.5 + (10.0 / -2)" -> (bd("5.5"), addExpr(nlit(10.5), divExpr(nlit(10.0), unaryNegExpr(nlit(2))))),
      "-20 + 30" -> (bd("10"), addExpr(unaryNegExpr(nlit(20)), nlit(30))),
      "-20 + 30*-5.0" -> (bd("-170"), addExpr(unaryNegExpr(nlit(20)), mulExpr(nlit(30), unaryNegExpr(nlit(5)))))
    )

    tests foreach {
      case (testInput, expectedOutput) =>
        val parseResult = parser.numericParser(scanner(testInput))

        inside(parseResult) {
          case parser.Success(result, _) =>
            inside(result) {
              case ne: Expr =>
                emptyContext.evaluateBD(ne) should be (expectedOutput._1)
                ne should be(expectedOutput._2)
            }
        }
    }
  }

  it should "do something about divide by zero" in {
    val tests = Map(
      "1 / 0" -> (bd("0"), divExpr(nlit(1), nlit(0)))
    )

    tests foreach {
      case (testInput, expectedOutput) =>
        val parseResult = parser.numericParser(scanner(testInput))

        inside(parseResult) {
          case parser.Success(result, _) =>
            inside(result) {
              case ne: Expr =>
                ne should be(expectedOutput._2)
                intercept[java.lang.ArithmeticException] {
                    emptyContext.evaluateBD(ne)
                }
            }
        }
    }
  }

  it should "ensure precedence of operators" in {
    val tests = Map(
      "1 + 2 * 3" -> (bd("7"), addExpr(nlit(1), mulExpr(nlit(2), nlit(3)))),
      "1 * 2 + 3" -> (bd("5"), addExpr(mulExpr(nlit(1), nlit(2)), nlit(3))),
      "2 + 1 / 4" -> (bd("2.25"), addExpr(nlit(2), divExpr(nlit(1), nlit(4)))),
      "1 / 2 + 3" -> (bd("3.5"), addExpr(divExpr(nlit(1), nlit(2)), nlit(3))),
      "1 * 2 + 3 * 4 - 2" -> (bd("12"), subExpr(addExpr(mulExpr(nlit(1), nlit(2)), mulExpr(nlit(3), nlit(4))), nlit(2))),
      "1 + 2 * 3 * 4 - 2" -> (bd("23"), subExpr(addExpr(nlit(1), mulExpr(mulExpr(nlit(2), nlit(3)), nlit(4))), nlit(2))),
      "1 / 2 + 3 / 4 - 2" -> (bd("-0.75"), subExpr(addExpr(divExpr(nlit(1), nlit(2)), divExpr(nlit(3), nlit(4))), nlit(2))),
      "1 + 2 / 5 / 4 - 2" -> (bd("-0.9"), subExpr(addExpr(nlit(1), divExpr(divExpr(nlit(2), nlit(5)), nlit(4))), nlit(2))),
      "1 + 2 * -3 * 4 - 2" -> (bd("-25"), subExpr(addExpr(nlit(1), mulExpr(mulExpr(nlit(2), unaryNegExpr(nlit(3))), nlit(4))), nlit(2))),
      "1 / 2 * 3" -> (bd("1.5"), mulExpr(divExpr(nlit(1), nlit(2)), nlit(3)))
    )

    tests foreach {
      case (testInput, expectedOutput) =>
        val parseResult = parser.numericParser(scanner(testInput))

        inside(parseResult) {
          case parser.Success(result, _) =>
            inside(result) {
              case ne: Expr =>
                emptyContext.evaluateBD(ne) should be (expectedOutput._1)
                ne should be(expectedOutput._2)
            }
        }
    }
  }

  it should "handle parenthesis correctly" in {
    val tests = Map(
      "1 + (2 * 3)" -> (bd("7"), addExpr(nlit(1), mulExpr(nlit(2), nlit(3)))),
      "(1 + 2) * 3" -> (bd("9"), mulExpr(addExpr(nlit(1), nlit(2)), nlit(3))),
      "1 * (2 + 3)" -> (bd("5"), mulExpr(nlit(1), addExpr(nlit(2), nlit(3)))),
      "1 * (2 + 3) * 4 - 2" -> (bd("18"), subExpr(mulExpr(mulExpr(nlit(1), addExpr(nlit(2), nlit(3))), nlit(4)), nlit(2)))
    )

    tests foreach {
      case (testInput, expectedOutput) =>
        val parseResult = parser.numericParser(scanner(testInput))

        inside(parseResult) {
          case parser.Success(result, _) =>
            inside(result) {
              case ne: Expr =>
                emptyContext.evaluateBD(ne) should be (expectedOutput._1)
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
      val parseResult = parser.dateFrag(scanner(input))

      inside(parseResult) {
        case parser.Success(date, _) =>
          date should be(Date(2013, 1, 1))
      }
    }

  }

  it should "parse dates in ISO 8601" in {
    val testInput = List(
      "2013-01-02"
    )

    testInput foreach {input =>
      val parseResult = parser.isoDateFrag(scanner(input))

      inside(parseResult) {
        case parser.Success(date, _) =>
          date should be(Date(2013, 1, 2))
      }
    }

  }

  it should "parse compact transactions" in {
    val testInput = """
    |def defaultAccount = bank
    |def tax = 0.1
    |. 2016/May/1 Expense        100 * tax            ; simple
    |  .   2016/May/2 Cash           800 + tax        ; with a space
    |2016/May/2
    |  Cash                      900 + tax
    """.stripMargin

    val parseResult = parser.abandon(scanner(testInput))

    inside(parseResult) {
      case parser.Success(result, _) =>
        inside(result.entries) {
          case List(_, _, txnGroup1, txnGroup2, txnGroup3) =>
            inside(txnGroup1) {
              case Transaction(_, date, posts, None, None, Nil) =>
                date should be(Date(2016, 5, 1))
                inside(posts) {
                  case List(Post(acc1, expr1, _)) =>
                    acc1 should be (expenseAccount)
                    expr1 should be (Some(mulExpr(nlit(100), identifierExpr("tax"))))
                }
            }
            inside(txnGroup2) {
              case Transaction(_, date, posts, None, None, Nil) =>
                date should be(Date(2016, 5, 2))
                inside(posts) {
                  case List(Post(acc1, expr1, _)) =>
                    acc1 should be (cashAccount)
                    expr1 should be (Some(addExpr(nlit(800), identifierExpr("tax"))))
                }
            }
            inside(txnGroup3) {
              case Transaction(_, date, posts, None, None, Nil) =>
                date should be(Date(2016, 5, 2))
                inside(posts) {
                  case List(Post(acc1, expr1, _)) =>
                    acc1 should be (cashAccount)
                    expr1 should be (Some(addExpr(nlit(900), identifierExpr("tax"))))
                }
            }
        }
    }
  }

  it should "parse compact transactions with multiple comments in a line" in {
    val testInput = """
                      |def defaultAccount = bank
                      |def tax = 0.1
                      |. 2016/May/1 Expense        100 * tax            ; multiple ; comments ; in a line
                    """.stripMargin

    val parseResult = parser.abandon(scanner(testInput))

    inside(parseResult) {
      case parser.Success(result, _) =>
        inside(result.entries) {
          case List(_, _, txnGroup1) =>
            inside(txnGroup1) {
              case Transaction(_, _, posts, None, None, comments) =>
                comments should be(List(" comments ", " in a line"))
                inside(posts) {
                  case List(Post(_, _, commentOpt)) =>
                    commentOpt should be (Some(" multiple "))
                }
            }
        }
    }
  }

  it should "parse multiple comments in multiline transactions" in {
    implicit val testInput =
      """
    2013/1/2
      ; Part 1 ; Part 2
      Expense       -200   ; Part 3 ; Part 4
      Cash                 ; Part 5 ; Part 6
    """

    val parseResult = parser.abandon(scanner(testInput))

    inside(parseResult) {
      case parser.Success(result, _) =>
        inside(result.entries) {
          case List(txnGroup) =>
            inside(txnGroup) {
              case Transaction(_, _, posts, None, None, comment1 :: Nil) =>
                comment1 should be (" Part 1 ; Part 2")
                inside(posts) {
                  case List(Post(_, _, comment1), Post(_, _, comment2)) =>
                    comment1 should be (Some(" Part 3 ; Part 4"))
                    comment2 should be (Some(" Part 5 ; Part 6"))
                }
            }
        }
    }
  }

  it should "parse a payee with an apostrophe in the name" in {
    implicit val testInput =
      """
      2017-02-23 Wendy's
         Expenses:Restaurants    2.76
         amexbce
      """
    val parseResult = parser.abandon(scanner(testInput))

    inside(parseResult) {
      case parser.Success(result, _) =>
        inside(result.entries) {
          case List(txnGroup) =>
            inside(txnGroup) {
              case Transaction(_, _, _, None, payee, Nil) =>
                payee should be(Some("Wendy's"))
            }
        }
    }
  }
}
