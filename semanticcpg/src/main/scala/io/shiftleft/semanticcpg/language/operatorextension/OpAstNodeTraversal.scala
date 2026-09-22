package io.shiftleft.semanticcpg.language.operatorextension

import io.shiftleft.codepropertygraph.generated.help.{Doc, Traversal}
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, Call}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.codepropertygraph.generated.help.Doc

@scala.annotation.nowarn("cat=deprecation")
@Traversal(elementType = classOf[AstNode])
class OpAstNodeTraversal[A <: AstNode](val traversal: Iterator[A]) extends AnyVal {

  @deprecated(
    "this step is very imprecise, recursively checking for matching AST children. It may return more than you intended."
  )
  @Doc(info = "Any assignments that this node is a part of (traverse up)")
  def assignment: Iterator[OpNodes.Assignment] = traversal.flatMap(_.assignment)

  @deprecated(
    "this step is very imprecise, recursively checking for matching AST children. It may return more than you intended."
  )
  @Doc(info = "Arithmetic expressions nested in this tree")
  def arithmetic: Iterator[OpNodes.Arithmetic] = traversal.flatMap(_.arithmetic)

  @deprecated(
    "this step is very imprecise, recursively checking for matching AST children. It may return more than you intended."
  )
  @Doc(info = "All array accesses")
  def arrayAccess: Iterator[OpNodes.ArrayAccess] = traversal.flatMap(_.arrayAccess)

  @deprecated(
    "this step is very imprecise, recursively checking for matching AST children. It may return more than you intended."
  )
  @Doc(info = "Field accesses, both direct and indirect")
  def fieldAccess: Iterator[OpNodes.FieldAccess] =
    traversal.flatMap(_.fieldAccess)

  @deprecated(
    "this step is very imprecise, recursively checking for matching AST ancestors. It may return more than you intended."
  )
  @Doc(info = "Any assignments that this node is a part of (traverse up)")
  def inAssignment: Iterator[OpNodes.Assignment] = traversal.flatMap(_.inAssignment)

  @deprecated(
    "this step is very imprecise, recursively checking for matching AST ancestors. It may return more than you intended."
  )
  @Doc(info = "Any arithmetic expression that this node is a part of (traverse up)")
  def inArithmetic: Iterator[OpNodes.Arithmetic] = traversal.flatMap(_.inArithmetic)

  @deprecated(
    "this step is very imprecise, recursively checking for matching AST ancestors. It may return more than you intended."
  )
  @Doc(info = "Any array access that this node is a part of (traverse up)")
  def inArrayAccess: Iterator[OpNodes.ArrayAccess] = traversal.flatMap(_.inArrayAccess)

  @deprecated(
    "this step is very imprecise, recursively checking for matching AST ancestors. It may return more than you intended."
  )
  @Doc(info = "Any field access that this node is a part of (traverse up)")
  def inFieldAccess: Iterator[OpNodes.FieldAccess] = traversal.flatMap(_.inFieldAccess)

}
