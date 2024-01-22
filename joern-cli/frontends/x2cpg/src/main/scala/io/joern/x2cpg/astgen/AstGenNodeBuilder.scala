package io.joern.x2cpg.astgen

import io.joern.x2cpg.AstNodeBuilder
import io.shiftleft.codepropertygraph.generated.nodes.AstNode.PropertyDefaults

/** An extension of AstNodeBuilder that is able to provide useful defaults from the more specialized node type that
  * AstGen-based frontends use.
  */
trait AstGenNodeBuilder[NodeProcessor] extends AstNodeBuilder[BaseNodeInfo[_], NodeProcessor] { this: NodeProcessor =>

  override def code(node: BaseNodeInfo[_]): String = Option(node).map(_.code).getOrElse(PropertyDefaults.Code)

  override def line(node: BaseNodeInfo[_]): Option[Integer] = Option(node).flatMap(_.lineNumber)

  override def lineEnd(node: BaseNodeInfo[_]): Option[Integer] = Option(node).flatMap(_.lineNumberEnd)

  override def column(node: BaseNodeInfo[_]): Option[Integer] = Option(node).flatMap(_.columnNumber)

  override def columnEnd(node: BaseNodeInfo[_]): Option[Integer] = Option(node).flatMap(_.columnNumberEnd)

}
