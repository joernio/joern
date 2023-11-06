package io.shiftleft.semanticcpg.language.operatorextension

import io.shiftleft.codepropertygraph.generated.v2.nodes.AstNode
import io.shiftleft.semanticcpg.language.*
// TODO bring back: import overflowdb.traversal.help.Doc

class OpAstNodeTraversal[A <: AstNode](val traversal: Iterator[A]) extends AnyVal {

  // TODO bring back: @Doc(info = "Any assignments that this node is a part of (traverse up)")
  def assignment: Iterator[OpNodes.Assignment] = traversal.flatMap(_.assignment)

  // TODO bring back: @Doc(info = "Arithmetic expressions nested in this tree")
  def arithmetic: Iterator[OpNodes.Arithmetic] = traversal.flatMap(_.arithmetic)

  // TODO bring back: @Doc(info = "All array accesses")
  def arrayAccess: Iterator[OpNodes.ArrayAccess] = traversal.flatMap(_.arrayAccess)

  // TODO bring back: @Doc(info = "Field accesses, both direct and indirect")
  def fieldAccess: Iterator[OpNodes.FieldAccess] =
    traversal.flatMap(_.fieldAccess)

  // TODO bring back: @Doc(info = "Any assignments that this node is a part of (traverse up)")
  def inAssignment: Iterator[OpNodes.Assignment] = traversal.flatMap(_.inAssignment)

  // TODO bring back: @Doc(info = "Any arithmetic expression that this node is a part of (traverse up)")
  def inArithmetic: Iterator[OpNodes.Arithmetic] = traversal.flatMap(_.inArithmetic)

  // TODO bring back: @Doc(info = "Any array access that this node is a part of (traverse up)")
  def inArrayAccess: Iterator[OpNodes.ArrayAccess] = traversal.flatMap(_.inArrayAccess)

  // TODO bring back: @Doc(info = "Any field access that this node is a part of (traverse up)")
  def inFieldAccess: Iterator[OpNodes.FieldAccess] = traversal.flatMap(_.inFieldAccess)

}
