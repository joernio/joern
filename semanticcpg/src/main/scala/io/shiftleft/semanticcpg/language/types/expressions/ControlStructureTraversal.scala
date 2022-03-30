package io.shiftleft.semanticcpg.language.types.expressions

import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, ControlStructure, Expression}
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, EdgeTypes, Properties}
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.help.Doc
import overflowdb.traversal.{Traversal, toElementTraversal, toNodeTraversal}

object ControlStructureTraversal {
  val secondChildIndex = 2
  val thirdChildIndex  = 3
}

class ControlStructureTraversal(val traversal: Traversal[ControlStructure]) extends AnyVal {
  import ControlStructureTraversal._

  @Doc(info = "The condition associated with this control structure")
  def condition: Traversal[Expression] =
    traversal.out(EdgeTypes.CONDITION).cast[Expression]

  @Doc(info = "Control structures where condition.code matches regex")
  def condition(regex: String): Traversal[ControlStructure] =
    traversal.where(_.condition.code(regex))

  @Doc(info = "Sub tree taken when condition evaluates to true")
  def whenTrue: Traversal[AstNode] =
    traversal.out.has(Properties.ORDER, secondChildIndex: Int).cast[AstNode]

  @Doc(info = "Sub tree taken when condition evaluates to false")
  def whenFalse: Traversal[AstNode] =
    traversal.out.has(Properties.ORDER, thirdChildIndex).cast[AstNode]

  @Doc(info = "Only `Try` control structures")
  def isTry: Traversal[ControlStructure] =
    traversal.controlStructureTypeExact(ControlStructureTypes.TRY)

  @Doc(info = "Only `If` control structures")
  def isIf: Traversal[ControlStructure] =
    traversal.controlStructureTypeExact(ControlStructureTypes.IF)

  @Doc(info = "Only `Else` control structures")
  def isElse: Traversal[ControlStructure] =
    traversal.controlStructureTypeExact(ControlStructureTypes.ELSE)

  @Doc(info = "Only `Switch` control structures")
  def isSwitch: Traversal[ControlStructure] =
    traversal.controlStructureTypeExact(ControlStructureTypes.SWITCH)

  @Doc(info = "Only `Do` control structures")
  def isDo: Traversal[ControlStructure] =
    traversal.controlStructureTypeExact(ControlStructureTypes.DO)

  @Doc(info = "Only `For` control structures")
  def isFor: Traversal[ControlStructure] =
    traversal.controlStructureTypeExact(ControlStructureTypes.FOR)

  @Doc(info = "Only `While` control structures")
  def isWhile: Traversal[ControlStructure] =
    traversal.controlStructureTypeExact(ControlStructureTypes.WHILE)

  @Doc(info = "Only `Goto` control structures")
  def isGoto: Traversal[ControlStructure] =
    traversal.controlStructureTypeExact(ControlStructureTypes.GOTO)

  @Doc(info = "Only `Break` control structures")
  def isBreak: Traversal[ControlStructure] =
    traversal.controlStructureTypeExact(ControlStructureTypes.BREAK)

  @Doc(info = "Only `Continue` control structures")
  def isContinue: Traversal[ControlStructure] =
    traversal.controlStructureTypeExact(ControlStructureTypes.CONTINUE)

  @Doc(info = "Only `Throw` control structures")
  def isThrow: Traversal[ControlStructure] =
    traversal.controlStructureTypeExact(ControlStructureTypes.THROW)

}
