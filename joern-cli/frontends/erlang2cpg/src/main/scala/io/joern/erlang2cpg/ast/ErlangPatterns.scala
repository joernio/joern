package io.joern.erlang2cpg.ast

import io.joern.erlang2cpg.parser.Term
import io.joern.x2cpg.{Ast, Defines}
import io.shiftleft.codepropertygraph.generated.Operators

/** Each accessor builds a fresh use of an already evaluated subject. This keeps the AST a tree even for nested
  * patterns. Map keys and binary sizes are reads in the scope at their position in the pattern.
  */
private[ast] trait ErlangPatterns { this: ModuleAst =>
  def bind(pattern: Term, value: () => Ast, scope: Scope): Seq[Ast] = {
    def projected(child: Term, selector: () => Ast): Seq[Ast] =
      bind(child, () => operator(child, Operators.indexAccess, Seq(value(), selector()), code(child)), scope)

    def indexed(children: Seq[Term]): Seq[Ast] = children.zipWithIndex.flatMap { case (child, i) =>
      projected(child, () => Ast(literalNode(child, (i + 1).toString, "integer")))
    }

    def shape(kind: String): Ast = operator(pattern, s"<operator>.$kind", Seq(value()), code(pattern))

    pattern.tag match {
      case "var" =>
        val name = pattern(2).text
        if (name == "_") Seq(reference(pattern, name, None))
        else {
          val input = value()
          scope.lookup(name) match {
            case Some(decl)
                if input.root.exists(root => input.refEdges.exists(edge => edge.src == root && edge.dst == decl)) =>
              Seq(reference(pattern, name, Some(decl)))
            case Some(decl) =>
              Seq(operator(pattern, Operators.equals, Seq(reference(pattern, name, Some(decl)), input), name))
            case None =>
              val variable = reference(pattern, name, Some(local(pattern, name, scope)))
              Seq(operator(pattern, Operators.assignment, Seq(variable, input), name))
          }
        }
      case "match" => bind(pattern(2), value, scope) ++ bind(pattern(3), value, scope)
      case "tuple" => shape("matchTuple") +: indexed(pattern(2).items)
      case "cons"  => shape("matchCons") +: indexed(pattern.items.drop(2))
      case "map"   =>
        shape("matchMap") +: pattern(2).items.flatMap { field =>
          projected(field(3), () => expr(field(2), scope))
        }
      case "record" =>
        shape("matchRecord") +: pattern(3).items.flatMap { field =>
          projected(field(3), () => expr(field(2), scope))
        }
      case "bin" =>
        shape("matchBinary") +: pattern(2).items.zipWithIndex.flatMap { case (segment, i) =>
          def segmentValue(): Ast = {
            val size    = Option.when(segment(3) != Term.Atom("default"))(expr(segment(3), scope))
            val ordinal = Ast(literalNode(segment, i.toString, "integer"))
            operator(segment, "<operator>.binarySegment", Seq(value(), ordinal) ++ size, code(segment))
          }
          bind(segment(2), segmentValue, scope)
        }
      case _ => Seq(operator(pattern, Operators.equals, Seq(value(), expr(pattern, scope)), code(pattern)))
    }
  }

  def matched(term: Term, scope: Scope): Ast = {
    val pattern = term(2)
    val value   = expr(term(3), scope)
    if (pattern.tag == "var" && pattern(2).text != "_" && scope.lookup(pattern(2).text).isEmpty) {
      val name     = pattern(2).text
      val variable = reference(pattern, name, Some(local(pattern, name, scope)))
      operator(term, Operators.assignment, Seq(variable, value), code(term))
    } else {
      val name           = fresh("match")
      val temporary      = local(term, name, scope)
      def subject(): Ast = reference(term, name, Some(temporary))
      val save           = operator(term, Operators.assignment, Seq(subject(), value), code(term))
      block(term, Seq(save) ++ bind(pattern, subject, scope) :+ subject())
    }
  }
}
