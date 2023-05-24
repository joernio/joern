package io.shiftleft.semanticcpg.language.operatorextension

import io.shiftleft.codepropertygraph.generated.nodes.{Expression, Identifier}
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal._
import overflowdb.traversal.help.Doc

class ArrayAccessTraversal(val traversal: Traversal[OpNodes.ArrayAccess]) extends AnyVal {

  @Doc(info = "The expression representing the array")
  def array: Traversal[Expression] = traversal.map(_.array)

  @Doc(info = "Offset at which the array is referenced (an expression)")
  def offset: Traversal[Expression] = traversal.map(_.offset)

  @Doc(info = "All identifiers that are part of the offset")
  def subscript: Traversal[Identifier] = traversal.flatMap(_.subscript)

  @Doc(
    info = "Determine whether array access has constant offset",
    longInfo = """
        Determine if array access is at constant numeric offset, e.g.,
        `buf[10]` but not `buf[i + 10]`, and for simplicity, not even `buf[1+2]`,
        `buf[PROBABLY_A_CONSTANT]` or `buf[PROBABLY_A_CONSTANT + 1]`,
        or even `buf[PROBABLY_A_CONSTANT]`.
        """
  )
  def usesConstantOffset: Traversal[OpNodes.ArrayAccess] = traversal.filter(_.usesConstantOffset)

  @Doc(info = "If `array` is a lone identifier, return its name")
  def simpleName: Traversal[String] = traversal.flatMap(_.simpleName)
}
