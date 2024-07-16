package io.shiftleft.semanticcpg.language.operatorextension

import io.shiftleft.codepropertygraph.generated.help.{Doc, Traversal}
import io.shiftleft.codepropertygraph.generated.nodes.AstNode
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.codepropertygraph.generated.help.Doc

@Traversal(elementType = classOf[AstNode])
class OpAstNodeTraversal[A <: AstNode](val traversal: Iterator[A]) extends AnyVal {

  @Doc(info = "Any assignments that this node is a part of (traverse up)")
  def assignment: Iterator[OpNodes.Assignment] = traversal.flatMap(_.assignment)

  @Doc(info = "Arithmetic expressions nested in this tree")
  def arithmetic: Iterator[OpNodes.Arithmetic] = traversal.flatMap(_.arithmetic)

  @Doc(info = "All array accesses")
  def arrayAccess: Iterator[OpNodes.ArrayAccess] = traversal.flatMap(_.arrayAccess)

  @Doc(info = "Field accesses, both direct and indirect")
  def fieldAccess: Iterator[OpNodes.FieldAccess] =
    traversal.flatMap(_.fieldAccess)

  @Doc(info = "Any assignments that this node is a part of (traverse up)")
  def inAssignment: Iterator[OpNodes.Assignment] = traversal.flatMap(_.inAssignment)

  @Doc(info = "Any arithmetic expression that this node is a part of (traverse up)")
  def inArithmetic: Iterator[OpNodes.Arithmetic] = traversal.flatMap(_.inArithmetic)

  @Doc(info = "Any array access that this node is a part of (traverse up)")
  def inArrayAccess: Iterator[OpNodes.ArrayAccess] = traversal.flatMap(_.inArrayAccess)

  @Doc(info = "Any field access that this node is a part of (traverse up)")
  def inFieldAccess: Iterator[OpNodes.FieldAccess] = traversal.flatMap(_.inFieldAccess)

}
