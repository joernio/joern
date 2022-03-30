package io.shiftleft.semanticcpg.language.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes.{Call, Expression, NewLocation}
import io.shiftleft.semanticcpg.NodeExtension
import io.shiftleft.semanticcpg.language.{HasLocation, LocationCreator, _}
import overflowdb.traversal._

class CallMethods(val node: Call) extends AnyVal with NodeExtension with HasLocation {
  def receiver: Traversal[Expression] =
    node.receiverOut

  def arguments(index: Int): Traversal[Expression] =
    node._argumentOut
      .collect {
        case expr: Expression if expr.argumentIndex == index => expr
      }
      .to(Traversal)

  def argument: Traversal[Expression] =
    node._argumentOut.collectAll[Expression]

  def argument(index: Int): Expression =
    arguments(index).head

  def argumentOption(index: Int): Option[Expression] =
    node._argumentOut.collectFirst {
      case expr: Expression if expr.argumentIndex == index => expr
    }

  override def location: NewLocation = {
    LocationCreator(node, node.code, node.label, node.lineNumber, node.method)
  }
}
