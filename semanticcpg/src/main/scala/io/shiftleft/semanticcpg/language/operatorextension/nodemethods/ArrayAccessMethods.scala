package io.shiftleft.semanticcpg.language.operatorextension.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes.{Expression, Identifier}
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes
import overflowdb.traversal.Traversal

class ArrayAccessMethods(val arrayAccess: OpNodes.ArrayAccess) extends AnyVal {

  def array: Expression =
    arrayAccess.argument(1)

  def offset: Expression = arrayAccess.argument(2)

  def subscript: Traversal[Identifier] =
    offset.ast.isIdentifier

  def usesConstantOffset: Boolean = {
    offset.ast.isIdentifier.nonEmpty || {
      val literalIndices = offset.ast.isLiteral.l
      literalIndices.size == 1
    }
  }

  def simpleName: Traversal[String] = {
    arrayAccess.array match {
      case id: Identifier => Traversal(id.name)
      case _              => Traversal()
    }
  }

}
