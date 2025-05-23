package io.shiftleft.semanticcpg.language.types.expressions

import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes.Assignment
import io.shiftleft.semanticcpg.language.operatorextension.allAssignmentTypes

/** A call site. */
class CallTraversal(val traversal: Iterator[Call]) extends AnyVal {

  /** Only statically dispatched calls */
  def isStatic: Iterator[Call] =
    traversal.filter(_.isStatic)

  /** Only dynamically dispatched calls */
  def isDynamic: Iterator[Call] =
    traversal.filter(_.isDynamic)

  /** Only dispatched calls inline */
  def isInline: Iterator[Call] =
    traversal.filter(_.isInline)

  /** Only assignment calls
    */
  def isAssignment: Iterator[Assignment] =
    traversal.methodFullNameExact(allAssignmentTypes.toSeq*).collectAll[Assignment]

  /** The receiver of a call if the call has a receiver associated.
    */
  def receiver: Iterator[Expression] =
    traversal.flatMap(_.receiver)

  /** Arguments of the calls
    */
  def argument: Iterator[Expression] =
    traversal.flatMap(_.argument)

  /** `i'th` arguments of the calls
    */
  def argument(i: Integer): Iterator[Expression] =
    traversal.flatMap(_.arguments(i))

  /** argument of the calls with matching name
    */
  def argument(pattern: String): Iterator[Expression] =
    traversal.flatMap(_.arguments(pattern))

  /** arguments of the calls with this exact name
    */
  def argumentExact(name: String): Iterator[Expression] =
    traversal.flatMap(_.argumentsExact(name))

  /** To formal method return parameter
    */
  def toMethodReturn(implicit callResolver: ICallResolver): Iterator[MethodReturn] =
    traversal
      .flatMap(callResolver.getCalledMethodsAsTraversal)
      .flatMap(_.methodReturn)

}
