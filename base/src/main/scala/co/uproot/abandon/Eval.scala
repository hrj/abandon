package co.uproot.abandon

import com.sun.org.apache.xalan.internal.xsltc.compiler.LiteralExpr

case class Ref(name: String, argCount: Int, pos: Option[InputPosition])

object EvaluationContext {
  private def ensureUnique(defs: Seq[Definition]):Unit = {
    if (defs.toSet.size != defs.size) {
      val duplicate = defs.map(_.name).combinations(2).find(e => e.head equals e.tail.head)
      val dupName = duplicate.get.head
      throw new InputPosError("Attempt to redefine symbol: " + dupName, defs.find(_.name == dupName).get.pos)
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
          throw mkInputError("Couldn't resolve reference: " + ref.name, ref.pos)
        }
      } else {
        if (defined(ref.name).params.length != ref.argCount) {
          throw new InputPosError("Reference and Definition parameters don't match: " + ref.name, d.pos)
        }
      }
    }
  }

  private def mkContext(newLocalDefs: Seq[Definition]) = {
    // println("Making new context with", newLocalDefs.map(_.prettyPrint))
    new EvaluationContext(scope, newLocalDefs)
  }

  def isImmediatelyEvaluable(name: String) = true

  private def mkInputError(msg: String, posOpt: Option[InputPosition]):RuntimeException = {
    posOpt match {
      case Some(pos) =>
        new InputPosError(msg, pos)
      case None =>
        new InputError(msg)
    }
  }

  def getValue(name: String, params: Seq[Expr], e: Expr) = {
    defined.get(name) match {
      case Some(d) =>
        val d = defined(name)
        if (d.params.length != params.length) {
          throw new InputPosError("Parameter lengths don't match for " + name, d.pos)
        }
        val newLocalDefs = d.params.zip(params).map(pairs => Definition(d.pos, pairs._1, Nil, pairs._2))
        val result = mkContext(newLocalDefs).evaluateInternal(d.rhs)
        // println("evaluated", name, params, result)
        result
      case None =>
        throw mkInputError("Couldn't resolve reference: " + name, e.pos)
    }
  }

  def evaluate[T](e: Expr)(implicit m: Manifest[T]):T = {
    // TODO: Expression can also have a position, and we can throw InputPosError
    evaluateInternal(e) match {
      case t:T => t
      case x =>
        throw mkInputError("Expected type: " + m + " but expression evaluated to: " + x.getClass, e.pos)
    }
  }

  def evaluateBD(e:Expr):BigDecimal = { evaluate[NumericLiteralExpr](e).value }
  def evaluateBoolean(e:Expr):Boolean = { evaluate[BooleanLiteralExpr](e).value }
  def evaluateString(e:Expr):String = { evaluate[StringLiteralExpr](e).value }

  private def evaluateInternal(e:Expr):Expr = {
    e match {
      case AddExpr(e1, e2) => NumericLiteralExpr(evaluateBD(e1) + evaluateBD(e2))(None)
      case SubExpr(e1, e2) => NumericLiteralExpr(evaluateBD(e1) - evaluateBD(e2))(None)
      case MulExpr(e1, e2) => NumericLiteralExpr(evaluateBD(e1) * evaluateBD(e2))(None)
      case DivExpr(e1, e2) => NumericLiteralExpr(evaluateBD(e1) / evaluateBD(e2))(None)
      case UnaryNegExpr(e1) => NumericLiteralExpr(-evaluateBD(e1))(None)
      case le:LiteralValue[_] => le
      case IdentifierExpr(name) => getValue(name, Nil, e)
      case FunctionExpr(name, arguments, _) => getValue(name, arguments.map(evaluateInternal(_)), e)
      case IfExpr(cond, e1, e2) => evaluateInternal(if(evaluateBoolean(cond)) e1 else e2)
      case ConditionExpr(e1, op, e2) => {
        val r1 = evaluateBD(e1)
        val r2 = evaluateBD(e2)
        op match {
          case ">" => BooleanLiteralExpr(r1 > r2)(None)
          case ">=" => BooleanLiteralExpr(r1 >= r2)(None)
          case "<" => BooleanLiteralExpr(r1 < r2)(None)
          case "<=" => BooleanLiteralExpr(r1 <= r2)(None)
          case "==" => BooleanLiteralExpr(r1 == r2)(None)
        }
      }
    }
  }
}