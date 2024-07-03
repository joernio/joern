package io.shiftleft.semanticcpg.language.types.expressions

import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, ControlStructure, Expression}
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, Properties}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.codepropertygraph.generated.help.Doc

object ControlStructureTraversal {
  val secondChildIndex = 2
  val thirdChildIndex  = 3
}

class ControlStructureTraversal(val traversal: Iterator[ControlStructure]) extends AnyVal {
  import ControlStructureTraversal.*

  @Doc(info = "The condition associated with this control structure")
  def condition: Iterator[Expression] =
    traversal.flatMap(_.conditionOut).collectAll[Expression]

  @Doc(info = "Control structures where condition.code matches regex")
  def condition(regex: String): Iterator[ControlStructure] =
    traversal.where(_.condition.code(regex))

  @Doc(info = "Sub tree taken when condition evaluates to true")
  def whenTrue: Iterator[AstNode] =
    traversal.out.has(Properties.Order, secondChildIndex: Int).cast[AstNode]

  @Doc(info = "Sub tree taken when condition evaluates to false")
  def whenFalse: Iterator[AstNode] =
    traversal.out.has(Properties.Order, thirdChildIndex).cast[AstNode]

  @Doc(info = "Only `Try` control structures")
  def isTry: Iterator[ControlStructure] =
    traversal.controlStructureTypeExact(ControlStructureTypes.TRY)

  @Doc(info = "Only `Catch` control structures")
  def isCatch: Iterator[ControlStructure] =
    traversal.controlStructureTypeExact(ControlStructureTypes.CATCH)

  @Doc(info = "Only `Finally` control structures")
  def isFinally: Iterator[ControlStructure] =
    traversal.controlStructureTypeExact(ControlStructureTypes.FINALLY)

  @Doc(info = "Only `If` control structures")
  def isIf: Iterator[ControlStructure] =
    traversal.controlStructureTypeExact(ControlStructureTypes.IF)

  @Doc(info = "Only `Else` control structures")
  def isElse: Iterator[ControlStructure] =
    traversal.controlStructureTypeExact(ControlStructureTypes.ELSE)

  @Doc(info = "Only `Switch` control structures")
  def isSwitch: Iterator[ControlStructure] =
    traversal.controlStructureTypeExact(ControlStructureTypes.SWITCH)

  @Doc(info = "Only `Do` control structures")
  def isDo: Iterator[ControlStructure] =
    traversal.controlStructureTypeExact(ControlStructureTypes.DO)

  @Doc(info = "Only `For` control structures")
  def isFor: Iterator[ControlStructure] =
    traversal.controlStructureTypeExact(ControlStructureTypes.FOR)

  @Doc(info = "Only `While` control structures")
  def isWhile: Iterator[ControlStructure] =
    traversal.controlStructureTypeExact(ControlStructureTypes.WHILE)

  @Doc(info = "Only `Goto` control structures")
  def isGoto: Iterator[ControlStructure] =
    traversal.controlStructureTypeExact(ControlStructureTypes.GOTO)

  @Doc(info = "Only `Break` control structures")
  def isBreak: Iterator[ControlStructure] =
    traversal.controlStructureTypeExact(ControlStructureTypes.BREAK)

  @Doc(info = "Only `Continue` control structures")
  def isContinue: Iterator[ControlStructure] =
    traversal.controlStructureTypeExact(ControlStructureTypes.CONTINUE)

  @Doc(info = "Only `Throw` control structures")
  def isThrow: Iterator[ControlStructure] =
    traversal.controlStructureTypeExact(ControlStructureTypes.THROW)

}
