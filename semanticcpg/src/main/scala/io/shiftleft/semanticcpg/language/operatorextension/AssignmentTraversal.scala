package io.shiftleft.semanticcpg.language.operatorextension

import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.semanticcpg.language.*
import overflowdb.traversal.help
import io.shiftleft.codepropertygraph.generated.help.Doc

@help.Traversal(elementType = classOf[nodes.Call])
class AssignmentTraversal(val traversal: Iterator[OpNodes.Assignment]) extends AnyVal {

  @Doc(info = "Left-hand sides of assignments")
  def target: Iterator[nodes.Expression] = traversal.map(_.target)

  @Doc(info = "Right-hand sides of assignments")
  def source: Iterator[nodes.Expression] = traversal.map(_.source)
}
