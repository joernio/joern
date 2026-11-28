package io.joern.erlang2cpg.ast

import io.joern.erlang2cpg.parser.Term
import io.joern.x2cpg.{Ast, Defines}
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, EvaluationStrategies}
import io.shiftleft.codepropertygraph.generated.nodes.*

import scala.collection.mutable

private[ast] trait ErlangFuns { this: ModuleAst =>
  def fun(term: Term, outer: Scope): Ast = {
    val named = term.tag == "named_fun"
    val body  = if (named) term(3) else term(2)
    if (!named && body.tag == "function") {
      val target = body.items match {
        case Vector(_, Term.Atom(name), Term.Integer(arity)) =>
          val atom = Term.Tuple(Vector(Term.Atom("atom"), term(1), Term.Atom(name)))
          index.target(module, atom, arity.toInt)
        case Vector(_, mod, name, arity) if mod.tag == "atom" && name.tag == "atom" && arity.tag == "integer" =>
          Some(s"${mod(2).text}:${name(2).text}/${arity(2).text}")
        case _ => None
      }
      target
        .map { fullName =>
          Ast(methodRefNode(term, s"fun $fullName", fullName, "fun"))
        }
        .getOrElse(operator(term, "<operator>.funReference", body.items.drop(1).map(expr(_, outer)), "fun"))
    } else {
      val rows     = if (named) body.items else body(1).items
      val arity    = rows.head(2).items.size
      val name     = fresh("fun")
      val fullName = s"$module:$name/$arity"
      val closure  = methodRefNode(term, s"fun $name/$arity", fullName, "fun")
      val captured = mutable.LinkedHashMap.empty[String, NewNode]
      val locals   = mutable.ArrayBuffer.empty[NewLocal]
      val scope    = new Scope(
        locals = locals,
        inherited = variable => {
          outer.lookup(variable).map { original =>
            captured.getOrElseUpdate(
              variable, {
                val id      = s"$fullName:$variable"
                val binding = NewClosureBinding()
                  .closureBindingId(id)
                  .evaluationStrategy(EvaluationStrategies.BY_VALUE)
                diffGraph.addNode(binding)
                diffGraph.addEdge(closure, binding, EdgeTypes.CAPTURE)
                diffGraph.addEdge(binding, original, EdgeTypes.REF)
                val localCopy = localNode(term, variable, variable, Defines.Any, Some(id))
                locals += localCopy
                localCopy
              }
            )
          }
        }
      )
      if (named) local(term, term(2).text, scope)
      val method = function(term, name, rows, scope, shadowHead = true)
      closures += method
      Ast(closure)
    }
  }
}
