package io.shiftleft.semanticcpg.language.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.NodeExtension
import io.shiftleft.semanticcpg.language.ICallResolver
import io.shiftleft.semanticcpg.utils.MemberAccess
import io.shiftleft.semanticcpg.language.*

import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*

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
    argument.astParent match {
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

  def expressionUp: Iterator[Expression] = {
    node._astIn.collectAll[Expression]
  }

  def expressionDown: Iterator[Expression] = {
    node._astOut.collectAll[Expression]
  }

  def receivedCall: Iterator[Call] = {
    node._receiverIn.cast[Call]
  }

  def isArgument: Iterator[Expression] = {
    if (node._argumentIn.hasNext) Iterator.single(node)
    else Iterator.empty
  }

  def inCall: Iterator[Call] =
    node._argumentIn.headOption match {
      case Some(c: Call) => Iterator.single(c)
      case _             => Iterator.empty
    }

  def parameter(implicit callResolver: ICallResolver): Iterator[MethodParameterIn] = {
    val predicate: MethodParameterIn => Boolean = node.argumentName match {
      case Some(name) => _.name == name
      case None => param => param.index == node.argumentIndex || (param.isVariadic && param.index < node.argumentIndex)
    }

    for {
      // Expressions can have incoming argument edges not just from CallRepr nodes but also
      // from Return nodes for which an expansion to parameter makes no sense. So we match
      // on CallRepr.
      case call: CallRepr <- node._argumentIn

      calledMethods <- callResolver.getCalledMethods(call)
      paramIn       <- calledMethods.parameter
      if predicate(paramIn)
    } yield paramIn
  }

  def typ: Iterator[Type] =
    node._evalTypeOut.cast[Type]

}
