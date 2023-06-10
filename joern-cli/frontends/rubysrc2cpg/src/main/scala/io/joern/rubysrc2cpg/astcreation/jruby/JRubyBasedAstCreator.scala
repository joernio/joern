package io.joern.rubysrc2cpg.astcreation.jruby

import io.joern.x2cpg.{AstCreatorBase, AstNodeBuilder}
import org.jruby.ast.Node
import overflowdb.BatchedUpdate

class JRubyBasedAstCreator(filename: String)
    extends AstCreatorBase(filename)
    with AstNodeBuilder[Node, JRubyBasedAstCreator] {

  override def createAst(): BatchedUpdate.DiffGraphBuilder = {
    diffGraph
  }

  override protected def line(node: Node): Option[Integer]         = Some(node.getLine)
  override protected def column(node: Node): Option[Integer]       = None
  override protected def lineEnd(node: Node): Option[Integer]      = None
  override protected def columnEnd(element: Node): Option[Integer] = None
}
