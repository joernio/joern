package io.joern.x2cpg.astgen

import io.joern.x2cpg.AstNodeBuilder

/** An extension of AstNodeBuilder that is able to provide useful defaults from the more specialized node type that
  * AstGen-based frontends use.
  */
trait AstGenNodeBuilder[NodeProcessor] extends AstNodeBuilder[BaseNodeInfo[_], NodeProcessor] { this: NodeProcessor =>

  override def code(node: BaseNodeInfo[_]): String = node.code

  override def line(node: BaseNodeInfo[_]): Option[Integer] = node.lineNumber

  override def lineEnd(node: BaseNodeInfo[_]): Option[Integer] = node.lineNumberEnd

  override def column(node: BaseNodeInfo[_]): Option[Integer] = node.columnNumber

  override def columnEnd(node: BaseNodeInfo[_]): Option[Integer] = node.columnNumberEnd

}
