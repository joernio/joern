package io.shiftleft.semanticcpg.language.nodemethods

import io.shiftleft.Implicits.JavaIteratorDeco
import io.shiftleft.codepropertygraph.generated.nodes.{
  AstNode,
  Call,
  CallRepr,
  Expression,
  Local,
  MethodParameterIn,
  Type
}
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, NodeTypes}
import io.shiftleft.semanticcpg.NodeExtension
import io.shiftleft.semanticcpg.language.ICallResolver
import io.shiftleft.semanticcpg.utils.MemberAccess
import overflowdb.traversal._

import scala.annotation.tailrec
import scala.jdk.CollectionConverters._

// Many method of this class should return individual nodes instead of Traversal[...].
// But over time through some opague implicits the versions returning Traversal[...]
// got exposed and for now we do not want to break the API.
class ExpressionMethods(val node: Expression) extends AnyVal with NodeExtension {

  /** Traverse to it's parent expression (e.g. call or return) by following the incoming AST It's continuing it's walk
    * until it hits an expression that's not a generic "member access operation", e.g., "<operator>.memberAccess".
    */
  def parentExpression: Option[Expression] = _parentExpression(node)

  @tailrec
  private final def _parentExpression(argument: AstNode): Option[Expression] = {
    val parent = argument._astIn.onlyChecked
    parent match {
      case call: Call if MemberAccess.isGenericMemberAccessName(call.name) =>
        _parentExpression(call)
      case expression: Expression =>
        Some(expression)
      case _ =>
        None
    }
  }

  def expressionUp: Traversal[Expression] = {
    Traversal(node._astIn.asScala.filterNot(_.isInstanceOf[Local])).cast[Expression]
  }

  def expressionDown: Traversal[Expression] = {
    Traversal(node._astOut.asScala.filterNot(_.isInstanceOf[Local])).cast[Expression]
  }

  def receivedCall: Traversal[Call] = {
    Traversal(node._receiverIn.asScala).cast[Call]
  }

  def isArgument: Traversal[Expression] = {
    Traversal(node._argumentIn.asScala).cast[Expression]
  }

  def inCall: Traversal[Call] = {
    Traversal(node).repeat(_.in(EdgeTypes.ARGUMENT))(_.until(_.hasLabel(NodeTypes.CALL))).cast[Call]
  }

  def parameter(implicit callResolver: ICallResolver): Traversal[MethodParameterIn] =
    for {
      call          <- node._argumentIn.asScala
      calledMethods <- callResolver.getCalledMethods(call.asInstanceOf[CallRepr])
      paramIn       <- calledMethods._astOut.asScala.collect { case node: MethodParameterIn => node }
      if paramIn.index == node.argumentIndex
    } yield paramIn

  def typ: Traversal[Type] = {
    Traversal(node._evalTypeOut).cast[Type]
  }

}
