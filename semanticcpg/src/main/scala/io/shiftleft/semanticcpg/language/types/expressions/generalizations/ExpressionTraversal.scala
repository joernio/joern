package io.shiftleft.semanticcpg.language.types.expressions.generalizations

import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.{InitialTraversal, PathAwareTraversal, Traversal}

/** An expression (base type)
  */
class ExpressionTraversal[NodeType <: Expression](val traversal: Traversal[NodeType]) extends AnyVal {

  /** Traverse to it's parent expression (e.g. call or return) by following the incoming AST It's continuing it's walk
    * until it hits an expression that's not a generic "member access operation", e.g., "<operator>.memberAccess".
    */
  def parentExpression: Traversal[Expression] =
    traversal.flatMap(_.parentExpression)

  /** Traverse to enclosing expression
    */
  def expressionUp: Traversal[Expression] =
    traversal.flatMap(_.expressionUp)

  /** Traverse to sub expressions
    */
  def expressionDown: Traversal[Expression] =
    traversal.flatMap(_.expressionDown)

  /** If the expression is used as receiver for a call, this traverses to the call.
    */
  def receivedCall: Traversal[Call] =
    traversal.flatMap(_.receivedCall)

  /** Only those expressions which are (direct) arguments of a call
    */
  def isArgument: Traversal[Expression] =
    traversal.flatMap(_.isArgument)

  /** Traverse to surrounding call
    */
  def inCall: Traversal[Call] =
    traversal.flatMap(_.inCall)

  /** Traverse to surrounding call
    */
  @deprecated("Use inCall")
  def call: Traversal[Call] =
    inCall

  /** Traverse to related parameter
    */
  @deprecated("", "October 2019")
  def toParameter(implicit callResolver: ICallResolver): Traversal[MethodParameterIn] = parameter

  /** Traverse to related parameter, if the expression is an argument to a call and the call can be resolved.
    */
  def parameter(implicit callResolver: ICallResolver): Traversal[MethodParameterIn] =
    traversal.flatMap(_.parameter)

  /** Traverse to enclosing method
    */
  def method: Traversal[Method] =
    traversal._containsIn
      .flatMap {
        case x: Method   => x.start
        case x: TypeDecl => x.astParent
      }
      .collectAll[Method]

  /** Traverse to expression evaluation type
    */
  def typ: Traversal[Type] =
    traversal.flatMap(_._evalTypeOut).cast[Type]

}
