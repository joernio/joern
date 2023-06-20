package io.shiftleft.semanticcpg.language.nodemethods

import io.shiftleft.Implicits.IterableOnceDeco
import io.shiftleft.codepropertygraph.generated.nodes._
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
      case annotationParameterAssign: AnnotationParameterAssign =>
        _parentExpression(annotationParameterAssign)
      case _ =>
        None
    }
  }

  def expressionUp: Traversal[Expression] = {
    node._astIn.collectAll[Expression]
  }

  def expressionDown: Traversal[Expression] = {
    node._astOut.collectAll[Expression]
  }

  def receivedCall: Traversal[Call] = {
    node._receiverIn.cast[Call]
  }

  def isArgument: Traversal[Expression] = {
    if (node._argumentIn.hasNext) Iterator.single(node)
    else Iterator.empty
  }

  def inCall: Traversal[Call] =
    node._argumentIn.headOption match {
      case Some(c: Call) => Iterator.single(c)
      case _             => Iterator.empty
    }

  def parameter(implicit callResolver: ICallResolver): Traversal[MethodParameterIn] =
    for {
      call          <- node._argumentIn
      calledMethods <- callResolver.getCalledMethods(call.asInstanceOf[CallRepr])
      paramIn       <- calledMethods._astOut.collectAll[MethodParameterIn]
      if paramIn.index == node.argumentIndex
    } yield paramIn

  def typ: Traversal[Type] =
    node._evalTypeOut.cast[Type]

}
