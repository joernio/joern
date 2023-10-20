package io.shiftleft.semanticcpg.language.operatorextension

import io.shiftleft.codepropertygraph.generated.nodes.Expression
import io.shiftleft.semanticcpg.language.*
// TODO bring back: import overflowdb.traversal.help
// TODO bring back: import overflowdb.traversal.help.Doc

// TODO bring back: @help.Traversal(elementType = classOf[OpNodes.Assignment])
class AssignmentTraversal(val traversal: Iterator[OpNodes.Assignment]) extends AnyVal {

  // TODO bring back: // TODO bring back: @Doc(info = "Left-hand sides of assignments")
  def target: Iterator[Expression] = traversal.map(_.target)

  // TODO bring back: // TODO bring back: @Doc(info = "Right-hand sides of assignments")
  def source: Iterator[Expression] = traversal.map(_.source)
}
