package io.shiftleft.semanticcpg.language.operatorextension.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, Call}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.operatorextension.*

class OpAstNodeMethods[A <: AstNode](val node: A) extends AnyVal {

  @deprecated(
    "this step is very imprecise, recursively checking for matching AST children. It may return more than you intended. Consider call.isAssignment instead."
  )
  def assignment: Iterator[OpNodes.Assignment] = node.ast.isCall.isAssignment

  @deprecated(
    "this step is very imprecise, recursively checking for matching AST nodes. It may return more than you intended. Consider call.isArithmetic instead."
  )
  def arithmetic: Iterator[OpNodes.Arithmetic] = node.ast.isCall.isArithmetic

  @deprecated(
    "this step is very imprecise, recursively checking for matching AST nodes. It may return more than you intended. Consider call.isArrayAccess instead."
  )
  def arrayAccess: Iterator[OpNodes.ArrayAccess] = node.ast.isCall.isArrayAccess

  @deprecated(
    "this step is very imprecise, recursively checking for matching AST nodes. It may return more than you intended. Consider call.isFieldAccess instead."
  )
  def fieldAccess: Iterator[OpNodes.FieldAccess] = node.ast.isCall.isFieldAccess

  @deprecated(
    "this step is very imprecise, recursively checking for matching AST ancestors. It may return more than you intended. Consider call.isAssignment instead."
  )
  def inAssignment: Iterator[OpNodes.Assignment] = node.inAstMinusLeaf.isCall.isAssignment

  @deprecated(
    "this step is very imprecise, recursively checking for matching AST ancestors. It may return more than you intended. Consider call.isArithmetic instead."
  )
  def inArithmetic: Iterator[OpNodes.Arithmetic] = node.inAstMinusLeaf.isCall.isArithmetic

  @deprecated(
    "this step is very imprecise, recursively checking for matching AST ancestors. It may return more than you intended. Consider call.isArrayAccess instead."
  )
  def inArrayAccess: Iterator[OpNodes.ArrayAccess] = node.inAstMinusLeaf.isCall.isArrayAccess

  @deprecated(
    "this step is very imprecise, recursively checking for matching AST ancestors. It may return more than you intended. Consider call.isFieldAccess instead."
  )
  def inFieldAccess: Iterator[OpNodes.FieldAccess] = node.inAstMinusLeaf.isCall.isFieldAccess

}
