package co.uproot.abandon

import com.sun.org.apache.xalan.internal.xsltc.compiler.LiteralExpr

case class Ref(name: String, argCount: Int)

object EvaluationContext {
  private def ensureUnique[T](defs: Seq[Definition[T]]):Unit = {
    if (defs.toSet.size != defs.size) {
      val duplicate = defs.map(_.name).combinations(2).find(e => e.head equals e.tail.head)
      throw new InputError("Attempt to redefine value having name: " + duplicate.get.head)
    }
  }
}

class EvaluationContext[T](globalDefinitions: Seq[Definition[T]], localDefinitions: Seq[Definition[T]], literalFactory: (T) => Expr[T]) {
  EvaluationContext.ensureUnique(globalDefinitions)
  EvaluationContext.ensureUnique(localDefinitions)

  private val localNames = localDefinitions.map(_.name)
  private val definitions = globalDefinitions.filter(d => !localNames.contains(d.name)) ++ localDefinitions
  // println("Context created\n" + definitions.map(_.prettyPrint).mkString("\n"))

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

  private def mkContext(newLocalDefs: Seq[Definition[T]]) = {
    // println("Making new context with", newLocalDefs.map(_.prettyPrint))
    new EvaluationContext[T](globalDefinitions, newLocalDefs, literalFactory)
  }

  def isImmediatelyEvaluable(name: String) = true

  def getValue(name: String, params: Seq[T]) = {
    val d = defined(name)
    if (d.params.length != params.length) {
      throw new InputError("Parameter lengths don't match for " + name)
    }
    val newLocalDefs = d.params.zip(params).map(pairs => Definition(pairs._1, Nil, literalFactory(pairs._2)))
    val result = d.rhs.evaluate(mkContext(newLocalDefs))
    // println("evaluated", name, params, result)
    result
  }
}