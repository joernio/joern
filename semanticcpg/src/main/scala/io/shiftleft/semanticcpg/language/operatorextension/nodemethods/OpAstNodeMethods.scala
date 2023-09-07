package io.shiftleft.semanticcpg.language.operatorextension.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, Call}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.operatorextension.*

class OpAstNodeMethods[A <: AstNode](val node: A) extends AnyVal {

  def assignment: Iterator[OpNodes.Assignment] =
    astDown(allAssignmentTypes).map(new OpNodes.Assignment(_))

  def arithmetic: Iterator[OpNodes.Arithmetic] =
    astDown(allArithmeticTypes).map(new OpNodes.Arithmetic(_))

  def arrayAccess: Iterator[OpNodes.ArrayAccess] =
    astDown(allArrayAccessTypes).map(new OpNodes.ArrayAccess(_))

  def fieldAccess: Iterator[OpNodes.FieldAccess] =
    astDown(allFieldAccessTypes).map(new OpNodes.FieldAccess(_))

  private def astDown(callNames: Set[String]): Iterator[Call] =
    node.ast.isCall.filter(x => callNames.contains(x.name))

  def inAssignment: Iterator[OpNodes.Assignment] =
    astUp(allAssignmentTypes)
      .map(new OpNodes.Assignment(_))

  def inArithmetic: Iterator[OpNodes.Arithmetic] =
    astUp(allArithmeticTypes)
      .map(new OpNodes.Arithmetic(_))

  def inArrayAccess: Iterator[OpNodes.ArrayAccess] =
    astUp(allArrayAccessTypes)
      .map(new OpNodes.ArrayAccess(_))

  def inFieldAccess: Iterator[OpNodes.FieldAccess] =
    astUp(allFieldAccessTypes)
      .map(new OpNodes.FieldAccess(_))

  private def astUp(strings: Set[String]): Iterator[Call] =
    node.inAstMinusLeaf.isCall
      .filter(x => strings.contains(x.name))

}
