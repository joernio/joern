package io.shiftleft.semanticcpg.language.operatorextension.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, Call}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.operatorextension.*

class OpAstNodeMethods[A <: AstNode](val node: A) extends AnyVal {

  @deprecated(
    "this step is very imprecise, recursively checking for matching AST children. It may return more than you intended. Consider call.isAssignment instead."
  )
  def assignment: Iterator[OpNodes.Assignment] = astDown(allAssignmentTypes).cast[OpNodes.Assignment]

  @deprecated(
    "this step is very imprecise, recursively checking for matching AST nodes. It may return more than you intended. Consider call.isArithmetic instead."
  )
  def arithmetic: Iterator[OpNodes.Arithmetic] = astDown(allArithmeticTypes).cast[OpNodes.Arithmetic]

  @deprecated(
    "this step is very imprecise, recursively checking for matching AST nodes. It may return more than you intended. Consider call.isArrayAccess instead."
  )
  def arrayAccess: Iterator[OpNodes.ArrayAccess] = astDown(allArrayAccessTypes).cast[OpNodes.ArrayAccess]

  @deprecated(
    "this step is very imprecise, recursively checking for matching AST nodes. It may return more than you intended. Consider call.isFieldAccess instead."
  )
  def fieldAccess: Iterator[OpNodes.FieldAccess] = astDown(allFieldAccessTypes).cast[OpNodes.FieldAccess]

  private def astDown(callNames: Seq[String]): Iterator[Call] = node.ast.isCall.nameExact(callNames*)

  @deprecated(
    "this step is very imprecise, recursively checking for matching AST ancestors. It may return more than you intended. Consider call.isAssignment instead."
  )
  def inAssignment: Iterator[OpNodes.Assignment] = astUp(allAssignmentTypes).cast[OpNodes.Assignment]

  @deprecated(
    "this step is very imprecise, recursively checking for matching AST ancestors. It may return more than you intended. Consider call.isArithmetic instead."
  )
  def inArithmetic: Iterator[OpNodes.Arithmetic] = astUp(allArithmeticTypes).cast[OpNodes.Arithmetic]

  @deprecated(
    "this step is very imprecise, recursively checking for matching AST ancestors. It may return more than you intended. Consider call.isArrayAccess instead."
  )
  def inArrayAccess: Iterator[OpNodes.ArrayAccess] = astUp(allArrayAccessTypes).cast[OpNodes.ArrayAccess]

  @deprecated(
    "this step is very imprecise, recursively checking for matching AST ancestors. It may return more than you intended. Consider call.isFieldAccess instead."
  )
  def inFieldAccess: Iterator[OpNodes.FieldAccess] = astUp(allFieldAccessTypes).cast[OpNodes.FieldAccess]

  private def astUp(strings: Seq[String]): Iterator[Call] = node.inAstMinusLeaf.isCall.nameExact(strings*)

}
