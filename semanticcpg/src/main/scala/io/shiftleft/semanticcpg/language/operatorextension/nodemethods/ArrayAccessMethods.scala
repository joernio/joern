package io.shiftleft.semanticcpg.language.operatorextension.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes.{Expression, Identifier}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes

class ArrayAccessMethods(val arrayAccess: OpNodes.ArrayAccess) extends AnyVal {

  def array: Expression =
    arrayAccess.argument(1)

  def offset: Expression = arrayAccess.argument(2)

  def subscript: Iterator[Identifier] =
    offset.ast.isIdentifier

  def usesConstantOffset: Boolean = {
    offset.ast.isIdentifier.nonEmpty || {
      val literalIndices = offset.ast.isLiteral.l
      literalIndices.size == 1
    }
  }

  def simpleName: Iterator[String] = {
    array match {
      case id: Identifier => Iterator.single(id.name)
      case _              => Iterator.empty
    }
  }

}
