package io.shiftleft.semanticcpg.language.operatorextension

import io.shiftleft.codepropertygraph.generated.nodes.Expression
import io.shiftleft.semanticcpg.language.*
import overflowdb.traversal.help
import overflowdb.traversal.help.Doc

@help.Traversal(elementType = classOf[OpNodes.Assignment])
class AssignmentTraversal(val traversal: Iterator[OpNodes.Assignment]) extends AnyVal {

  @Doc(info = "Left-hand sides of assignments")
  def target: Iterator[Expression] = traversal.map(_.target)

  @Doc(info = "Right-hand sides of assignments")
  def source: Iterator[Expression] = traversal.map(_.source)
}
