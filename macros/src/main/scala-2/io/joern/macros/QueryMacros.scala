package io.joern.macros

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.StoredNode
import io.joern.console.TraversalWithStrRep
import overflowdb.traversal.Traversal

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

object QueryMacros {

  def withStrRep(traversal: Cpg => Traversal[_ <: StoredNode]): TraversalWithStrRep = macro withStrRepImpl

  def withStrRepImpl(c: whitebox.Context)(traversal: c.Tree): c.Expr[TraversalWithStrRep] = {
    import c.universe._
    val fileContent               = new String(traversal.pos.source.content)
    val start                     = traversal.pos.start
    val end                       = traversal.pos.end
    val traversalAsString: String = fileContent.slice(start, end)

    c.Expr(q"""
        TraversalWithStrRep($traversal, $traversalAsString)
       """)
  }
}
