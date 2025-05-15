package io.shiftleft.semanticcpg.language.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes.{Call, Expression, NewLocation}
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.semanticcpg.NodeExtension
import io.shiftleft.semanticcpg.language.*

class CallMethods(val node: Call) extends AnyVal with NodeExtension with HasLocation {

  def isStatic: Boolean =
    node.dispatchType == DispatchTypes.STATIC_DISPATCH

  def isDynamic: Boolean =
    node.dispatchType == DispatchTypes.DYNAMIC_DISPATCH

  def isInline: Boolean =
    node.dispatchType == DispatchTypes.INLINED

  def receiver: Iterator[Expression] =
    node.receiverOut.collectAll[Expression]

  def arguments(index: Int): Iterator[Expression] =
    argument.filter { expr => expr.argumentName.isEmpty && expr.argumentIndex == index }

  def arguments(pattern: String): Iterator[Expression] =
    argument.where { _.argumentName(pattern) }
  def argumentsExact(name: String): Iterator[Expression] =
    argument.filter { expr => expr.argumentName.contains(name) }

  // TODO define as named step in the schema
  def argument: Iterator[Expression] =
    node._argumentOut.collectAll[Expression]

  def argument(index: Int): Expression =
    arguments(index).next

  def argumentOption(index: Int): Option[Expression] =
    arguments(index).nextOption

  def macroExpansion: Iterator[Expression] = {
    if (node.dispatchType != DispatchTypes.INLINED) return Iterator.empty

    node.astChildren.isBlock.maxByOption(_.order).iterator.expressionDown
  }

  override def location: NewLocation =
    LocationCreator.defaultCreateLocation(node)
}
