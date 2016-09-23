package co.uproot.abandon

import com.sun.org.apache.xalan.internal.xsltc.compiler.LiteralExpr

case class Ref(name: String, argCount: Int)

object EvaluationContext {
  private def ensureUnique(defs: Seq[Definition]):Unit = {
    if (defs.toSet.size != defs.size) {
      val duplicate = defs.map(_.name).combinations(2).find(e => e.head equals e.tail.head)
      throw new InputError("Attempt to redefine symbol: " + duplicate.get.head)
    }
  }
}

class EvaluationContext(scope: Scope, localDefs: Seq[Definition]) {

  // println("Context created\n" + definitions.map(_.prettyPrint).mkString("\n"))

  private val localNames = localDefs.map(_.name)
  private val definitions = scope.definitions.filter(d => !localNames.contains(d.name)) ++ localDefs
  private val defined = definitions.map(d => d.name -> d).toMap
  definitions.foreach { d =>
    d.rhs.getRefs foreach { ref =>
      if (!defined.isDefinedAt(ref.name)) {
        if (!d.params.contains(ref.name)) {
          throw new InputError("Definition not found: " + ref.name)
        }
      } else {
        if (defined(ref.name).params.length != ref.argCount) {
          throw new InputError("Reference and Definition parameters don't match: " + ref.name)
        }
      }
    }
  }

  private def mkContext(newLocalDefs: Seq[Definition]) = {
    // println("Making new context with", newLocalDefs.map(_.prettyPrint))
    new EvaluationContext(scope, newLocalDefs)
  }

  def isImmediatelyEvaluable(name: String) = true

  def getValue(name: String, params: Seq[Expr]) = {
    val d = defined(name)
    if (d.params.length != params.length) {
      throw new InputError("Parameter lengths don't match for " + name)
    }
    val newLocalDefs = d.params.zip(params).map(pairs => Definition(pairs._1, Nil, pairs._2))
    val result = mkContext(newLocalDefs).evaluateInternal(d.rhs)
    // println("evaluated", name, params, result)
    result
  }

  def evaluate[T](e: Expr)(implicit m: Manifest[T]):T = {
    evaluateInternal(e) match {
      case t:T => t
      case x => throw new InputError("Expected type: " + m + " but expression evaluated to: " + x.getClass)
    }
  }

  def evaluateBD(e:Expr):BigDecimal = { evaluate[NumericLiteralExpr](e).value }
  def evaluateBoolean(e:Expr):Boolean = { evaluate[BooleanLiteralExpr](e).value }
  def evaluateString(e:Expr):String = { evaluate[StringLiteralExpr](e).value }

  private def evaluateInternal(e:Expr):Expr = {
    e match {
      case AddExpr(e1, e2) => NumericLiteralExpr(evaluateBD(e1) + evaluateBD(e2))
      case SubExpr(e1, e2) => NumericLiteralExpr(evaluateBD(e1) - evaluateBD(e2))
      case MulExpr(e1, e2) => NumericLiteralExpr(evaluateBD(e1) * evaluateBD(e2))
      case DivExpr(e1, e2) => NumericLiteralExpr(evaluateBD(e1) / evaluateBD(e2))
      case UnaryNegExpr(e1) => NumericLiteralExpr(-evaluateBD(e1))
      case le:LiteralValue[_] => le
      case IdentifierExpr(name) => getValue(name, Nil)
      case FunctionExpr(name, arguments) => getValue(name, arguments.map(evaluateInternal(_)))
      case IfExpr(cond, e1, e2) => evaluateInternal(if(evaluateBoolean(cond)) e1 else e2)
      case ConditionExpr(e1, op, e2) => {
        val r1 = evaluateBD(e1)
        val r2 = evaluateBD(e2)
        op match {
          case ">" => BooleanLiteralExpr(r1 > r2)
          case ">=" => BooleanLiteralExpr(r1 >= r2)
          case "<" => BooleanLiteralExpr(r1 < r2)
          case "<=" => BooleanLiteralExpr(r1 <= r2)
          case "==" => BooleanLiteralExpr(r1 == r2)
        }
      }
    }
  }
}