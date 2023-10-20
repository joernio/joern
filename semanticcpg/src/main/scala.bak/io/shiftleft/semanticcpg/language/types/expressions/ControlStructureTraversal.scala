package io.shiftleft.semanticcpg.language.types.expressions

import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, ControlStructure, Expression}
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, Properties}
import io.shiftleft.semanticcpg.language.*
// TODO bring back: import overflowdb.traversal.help.Doc

object ControlStructureTraversal {
  val secondChildIndex = 2
  val thirdChildIndex  = 3
}

class ControlStructureTraversal(val traversal: Iterator[ControlStructure]) extends AnyVal {
  import ControlStructureTraversal.*

  // TODO bring back: @Doc(info = "The condition associated with this control structure")
  def condition: Iterator[Expression] =
    traversal.flatMap(_.conditionOut).collectAll[Expression]

  // TODO bring back: @Doc(info = "Control structures where condition.code matches regex")
  def condition(regex: String): Iterator[ControlStructure] =
    traversal.where(_.condition.code(regex))

  // TODO bring back: @Doc(info = "Sub tree taken when condition evaluates to true")
  def whenTrue: Iterator[AstNode] =
    traversal.out.has(Properties.ORDER, secondChildIndex: Int).cast[AstNode]

  // TODO bring back: @Doc(info = "Sub tree taken when condition evaluates to false")
  def whenFalse: Iterator[AstNode] =
    traversal.out.has(Properties.ORDER, thirdChildIndex).cast[AstNode]

  // TODO bring back: @Doc(info = "Only `Try` control structures")
  def isTry: Iterator[ControlStructure] =
    traversal.controlStructureTypeExact(ControlStructureTypes.TRY)

  // TODO bring back: @Doc(info = "Only `If` control structures")
  def isIf: Iterator[ControlStructure] =
    traversal.controlStructureTypeExact(ControlStructureTypes.IF)

  // TODO bring back: @Doc(info = "Only `Else` control structures")
  def isElse: Iterator[ControlStructure] =
    traversal.controlStructureTypeExact(ControlStructureTypes.ELSE)

  // TODO bring back: @Doc(info = "Only `Switch` control structures")
  def isSwitch: Iterator[ControlStructure] =
    traversal.controlStructureTypeExact(ControlStructureTypes.SWITCH)

  // TODO bring back: @Doc(info = "Only `Do` control structures")
  def isDo: Iterator[ControlStructure] =
    traversal.controlStructureTypeExact(ControlStructureTypes.DO)

  // TODO bring back: @Doc(info = "Only `For` control structures")
  def isFor: Iterator[ControlStructure] =
    traversal.controlStructureTypeExact(ControlStructureTypes.FOR)

  // TODO bring back: @Doc(info = "Only `While` control structures")
  def isWhile: Iterator[ControlStructure] =
    traversal.controlStructureTypeExact(ControlStructureTypes.WHILE)

  // TODO bring back: @Doc(info = "Only `Goto` control structures")
  def isGoto: Iterator[ControlStructure] =
    traversal.controlStructureTypeExact(ControlStructureTypes.GOTO)

  // TODO bring back: @Doc(info = "Only `Break` control structures")
  def isBreak: Iterator[ControlStructure] =
    traversal.controlStructureTypeExact(ControlStructureTypes.BREAK)

  // TODO bring back: @Doc(info = "Only `Continue` control structures")
  def isContinue: Iterator[ControlStructure] =
    traversal.controlStructureTypeExact(ControlStructureTypes.CONTINUE)

  // TODO bring back: @Doc(info = "Only `Throw` control structures")
  def isThrow: Iterator[ControlStructure] =
    traversal.controlStructureTypeExact(ControlStructureTypes.THROW)

}
