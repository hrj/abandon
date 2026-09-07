package co.uproot.abandon

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.parsing.input.OffsetPosition

class EvaluationContextTest extends AnyFlatSpec with Matchers {
  private val position = InputPosition(None, new OffsetPosition("test", 0))
  private def number(value: BigDecimal) = NumericLiteralExpr(value)(None)
  private def definition(name: String, params: List[String], rhs: Expr) =
    Definition(position, name, params, rhs)
  private def context(definitions: Definition*) =
    new EvaluationContext(Scope(definitions, None), Nil)

  "EvaluationContext" should "evaluate arithmetic, conditions, and branches" in {
    val ctx = context()

    ctx.evaluateBD(AddExpr(number(7), number(5))(None)) shouldBe 12
    ctx.evaluateBD(SubExpr(number(7), number(5))(None)) shouldBe 2
    ctx.evaluateBD(MulExpr(number(7), number(5))(None)) shouldBe 35
    ctx.evaluateBD(DivExpr(number(10), number(5))(None)) shouldBe 2
    ctx.evaluateBD(UnaryNegExpr(number(7))(None)) shouldBe -7

    Seq(
      ">" -> true,
      ">=" -> true,
      "<" -> false,
      "<=" -> false,
      "==" -> false
    ).foreach { case (operator, expected) =>
      ctx.evaluateBoolean(ConditionExpr(number(7), operator, number(5))(None)) shouldBe expected
    }

    ctx.evaluateBD(IfExpr(BooleanLiteralExpr(true)(None), number(1), number(2))(None)) shouldBe 1
    ctx.evaluateBD(IfExpr(BooleanLiteralExpr(false)(None), number(1), number(2))(None)) shouldBe 2
  }

  it should "respect comparison boundaries" in {
    val ctx = context()

    ctx.evaluateBoolean(ConditionExpr(number(5), ">", number(5))(None)) shouldBe false
    ctx.evaluateBoolean(ConditionExpr(number(5), ">=", number(5))(None)) shouldBe true
    ctx.evaluateBoolean(ConditionExpr(number(5), "<", number(5))(None)) shouldBe false
    ctx.evaluateBoolean(ConditionExpr(number(5), "<=", number(5))(None)) shouldBe true
    ctx.evaluateBoolean(ConditionExpr(number(5), "<=", number(7))(None)) shouldBe true
  }

  it should "resolve constants and parameterized definitions" in {
    val constant = definition("answer", Nil, number(42))
    val increment = definition(
      "increment",
      List("value"),
      AddExpr(IdentifierExpr("value")(None), number(1))(None)
    )
    val ctx = context(constant, increment)

    ctx.evaluateBD(IdentifierExpr("answer")(None)) shouldBe 42
    ctx.evaluateBD(FunctionExpr("increment", Seq(number(9)), None)) shouldBe 10
    constant.isUsed shouldBe true
    increment.isUsed shouldBe true
  }

  it should "reject unresolved and invalid references" in {
    an[InputError] should be thrownBy context(
      definition("broken", Nil, IdentifierExpr("missing")(None))
    )

    an[InputPosError] should be thrownBy context(
      definition("oneArg", List("x"), IdentifierExpr("x")(None)),
      definition("caller", Nil, FunctionExpr("oneArg", Nil, None))
    )

    val ctx = context(definition("oneArg", List("x"), IdentifierExpr("x")(None)))
    an[InputPosError] should be thrownBy
      ctx.evaluateBD(FunctionExpr("oneArg", Nil, None))
    an[InputError] should be thrownBy
      ctx.evaluateBD(IdentifierExpr("missing")(None))
  }

  it should "reject an unexpected result type" in {
    val ctx = context()
    val error = the[InputError] thrownBy ctx.evaluateBD(StringLiteralExpr("not a number")(None))
    error.getMessage should include("Expected type")
  }
}