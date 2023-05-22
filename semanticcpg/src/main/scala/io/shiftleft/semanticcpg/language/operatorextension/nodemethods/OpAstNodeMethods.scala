package io.shiftleft.semanticcpg.language.operatorextension.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, Call}
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.operatorextension.{
  OpNodes,
  allArithmeticTypes,
  allArrayAccessTypes,
  allAssignmentTypes,
  allFieldAccessTypes
}

class OpAstNodeMethods[A <: AstNode](val node: A) extends AnyVal {

  def assignment: Traversal[OpNodes.Assignment] =
    astDown(allAssignmentTypes).cast[OpNodes.Assignment]

  def arithmetic: Traversal[OpNodes.Arithmetic] =
    astDown(allArithmeticTypes).cast[OpNodes.Arithmetic]

  def arrayAccess: Traversal[OpNodes.ArrayAccess] =
    astDown(allArrayAccessTypes).cast[OpNodes.ArrayAccess]

  def fieldAccess: Traversal[OpNodes.FieldAccess] =
    astDown(allFieldAccessTypes).cast[OpNodes.FieldAccess]

  private def astDown(callNames: Set[String]): Traversal[Call] =
    node.ast.isCall.filter(x => callNames.contains(x.name))

  def inAssignment: Traversal[OpNodes.Assignment] =
    astUp(allAssignmentTypes)
      .cast[OpNodes.Assignment]

  def inArithmetic: Traversal[OpNodes.Arithmetic] =
    astUp(allArithmeticTypes)
      .cast[OpNodes.Arithmetic]

  def inArrayAccess: Traversal[OpNodes.ArrayAccess] =
    astUp(allArrayAccessTypes)
      .cast[OpNodes.ArrayAccess]

  def inFieldAccess: Traversal[OpNodes.FieldAccess] =
    astUp(allFieldAccessTypes)
      .cast[OpNodes.FieldAccess]

  private def astUp(strings: Set[String]): Traversal[Call] =
    node.inAstMinusLeaf.isCall
      .filter(x => strings.contains(x.name))

}
