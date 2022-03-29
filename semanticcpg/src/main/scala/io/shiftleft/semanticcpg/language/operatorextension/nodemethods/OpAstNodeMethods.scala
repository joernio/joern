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
import overflowdb.traversal.Traversal

class OpAstNodeMethods[A <: AstNode](val node: A) extends AnyVal {

  def assignment: Traversal[OpNodes.Assignment] =
    astDown(allAssignmentTypes).map(new OpNodes.Assignment(_))

  def arithmetic: Traversal[OpNodes.Arithmetic] =
    astDown(allArithmeticTypes).map(new OpNodes.Arithmetic(_))

  def arrayAccess: Traversal[OpNodes.ArrayAccess] =
    astDown(allArrayAccessTypes).map(new OpNodes.ArrayAccess(_))

  def fieldAccess: Traversal[OpNodes.FieldAccess] =
    astDown(allFieldAccessTypes).map(new OpNodes.FieldAccess(_))

  private def astDown(callNames: Set[String]): Traversal[Call] =
    node.ast.isCall.filter(x => callNames.contains(x.name))

  def inAssignment: Traversal[OpNodes.Assignment] =
    astUp(allAssignmentTypes)
      .map(new OpNodes.Assignment(_))

  def inArithmetic: Traversal[OpNodes.Arithmetic] =
    astUp(allArithmeticTypes)
      .map(new OpNodes.Arithmetic(_))

  def inArrayAccess: Traversal[OpNodes.ArrayAccess] =
    astUp(allArrayAccessTypes)
      .map(new OpNodes.ArrayAccess(_))

  def inFieldAccess: Traversal[OpNodes.FieldAccess] =
    astUp(allFieldAccessTypes)
      .map(new OpNodes.FieldAccess(_))

  private def astUp(strings: Set[String]): Traversal[Call] =
    node.inAstMinusLeaf.isCall
      .filter(x => strings.contains(x.name))

}
