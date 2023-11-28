package io.shiftleft.semanticcpg.language.operatorextension.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, Call}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.operatorextension.*

class OpAstNodeMethods[A <: AstNode](val node: A) extends AnyVal {

  def assignment: Iterator[OpNodes.Assignment] =
    astDown(allAssignmentTypes).cast[OpNodes.Assignment]

  def arithmetic: Iterator[OpNodes.Arithmetic] =
    astDown(allArithmeticTypes).cast[OpNodes.Arithmetic]

  def arrayAccess: Iterator[OpNodes.ArrayAccess] =
    astDown(allArrayAccessTypes).cast[OpNodes.ArrayAccess]

  def fieldAccess: Iterator[OpNodes.FieldAccess] =
    astDown(allFieldAccessTypes).cast[OpNodes.FieldAccess]

  private def astDown(callNames: Set[String]): Iterator[Call] =
    node.ast.isCall.filter(x => callNames.contains(x.name))

  def inAssignment: Iterator[OpNodes.Assignment] =
    astUp(allAssignmentTypes)
      .cast[OpNodes.Assignment]

  def inArithmetic: Iterator[OpNodes.Arithmetic] =
    astUp(allArithmeticTypes)
      .cast[OpNodes.Arithmetic]

  def inArrayAccess: Iterator[OpNodes.ArrayAccess] =
    astUp(allArrayAccessTypes)
      .cast[OpNodes.ArrayAccess]

  def inFieldAccess: Iterator[OpNodes.FieldAccess] =
    astUp(allFieldAccessTypes)
      .cast[OpNodes.FieldAccess]

  private def astUp(strings: Set[String]): Iterator[Call] =
    node.inAstMinusLeaf.isCall
      .filter(x => strings.contains(x.name))

}
