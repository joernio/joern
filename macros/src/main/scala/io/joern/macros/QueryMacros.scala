package io.joern.macros

import io.joern.console.TraversalWithStrRep
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.StoredNode

import scala.quoted.{Expr, Quotes}

object QueryMacros {

  inline def withStrRep(inline traversal: Cpg => Iterator[? <: StoredNode]): TraversalWithStrRep =
    ${ withStrRepImpl('{ traversal }) }

  private def withStrRepImpl(
    travExpr: Expr[Cpg => Iterator[? <: StoredNode]]
  )(using quotes: Quotes): Expr[TraversalWithStrRep] = {
    import quotes.reflect._
    val pos  = travExpr.asTerm.pos
    val code = Position(pos.sourceFile, pos.start, pos.end).sourceCode.getOrElse("N/A")
    '{ TraversalWithStrRep(${ travExpr }, ${ Expr(code) }) }
  }
}
