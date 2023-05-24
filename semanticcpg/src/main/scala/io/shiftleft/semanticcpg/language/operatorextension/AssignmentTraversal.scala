package io.shiftleft.semanticcpg.language.operatorextension

import io.shiftleft.codepropertygraph.generated.nodes.Expression
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.help.Doc
import overflowdb.traversal.{help, _}

@help.Traversal(elementType = classOf[OpNodes.Assignment])
class AssignmentTraversal(val traversal: Traversal[OpNodes.Assignment]) extends AnyVal {

  @Doc(info = "Left-hand sides of assignments")
  def target: Traversal[Expression] = traversal.map(_.target)

  @Doc(info = "Right-hand sides of assignments")
  def source: Traversal[Expression] = traversal.map(_.source)
}
