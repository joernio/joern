package io.shiftleft.semanticcpg.language.types.expressions

import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, NodeTypes}
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal._

/** A call site
  */
class CallTraversal(val traversal: Traversal[Call]) extends AnyVal {

  /** Only statically dispatched calls
    */
  def isStatic: Traversal[Call] =
    traversal.dispatchType("STATIC_DISPATCH")

  /** Only dynamically dispatched calls
    */
  def isDynamic: Traversal[Call] =
    traversal.dispatchType("DYNAMIC_DISPATCH")

  /** The receiver of a call if the call has a receiver associated.
    */
  def receiver: Traversal[Expression] =
    traversal.flatMap(_.receiver)

  /** Arguments of the call
    */
  def argument: Traversal[Expression] =
    traversal.flatMap(_.argument)

  /** `i'th` arguments of the call
    */
  def argument(i: Integer): Traversal[Expression] =
    traversal.flatMap(_.arguments(i))

  /** To formal method return parameter
    */
  def toMethodReturn(implicit callResolver: ICallResolver): Traversal[MethodReturn] =
    traversal
      .flatMap(callResolver.getCalledMethodsAsTraversal)
      .out(EdgeTypes.AST)
      .hasLabel(NodeTypes.METHOD_RETURN)
      .cast[MethodReturn]

}
