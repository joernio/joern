package io.shiftleft.semanticcpg.language.callgraphextension

import io.shiftleft.codepropertygraph.generated.help.{Doc, Traversal}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.codepropertygraph.generated.help.Doc

@Traversal(elementType = classOf[Method])
class MethodTraversal(val traversal: Iterator[Method]) extends AnyVal {

  /** Intended for internal use! Traverse to direct and transitive callers of the method.
    */
  def calledByIncludingSink(sourceTrav: Iterator[Method])(implicit callResolver: ICallResolver): Iterator[Method] = {
    val sourceMethods = sourceTrav.toSet
    val sinkMethods   = traversal.dedup

    if (sourceMethods.isEmpty || sinkMethods.isEmpty) {
      Iterator.empty[Method].enablePathTracking
    } else {
      sinkMethods
        .repeat(
          _.flatMap(callResolver.getMethodCallsitesAsTraversal)._containsIn // expand to method
        )(_.dedup.emit(_.collect {
          case method: Method if sourceMethods.contains(method) => method
        }))
        .cast[Method]
    }
  }

  /** Traverse to direct callers of this method
    */
  def caller(implicit callResolver: ICallResolver): Iterator[Method] =
    callIn(callResolver).method

  /** Traverse to methods called by this method
    */
  def callee(implicit callResolver: ICallResolver): Iterator[Method] =
    call.callee(callResolver)

  /** Incoming call sites
    */
  def callIn(implicit callResolver: ICallResolver): Iterator[Call] =
    traversal.flatMap(method => callResolver.getMethodCallsitesAsTraversal(method).collectAll[Call])

  /** Traverse to direct and transitive callers of the method.
    */
  def calledBy(sourceTrav: Iterator[Method])(implicit callResolver: ICallResolver): Iterator[Method] =
    caller(callResolver).calledByIncludingSink(sourceTrav)(callResolver)

  @deprecated("Use call", "")
  def callOut: Iterator[Call] =
    call

  @deprecated("Use call", "")
  def callOutRegex(regex: String)(implicit callResolver: ICallResolver): Iterator[Call] =
    call(regex)

  /** Outgoing call sites to methods where fullName matches `regex`.
    */
  def call(regex: String)(implicit callResolver: ICallResolver): Iterator[Call] =
    call.where(_.callee.fullName(regex))

  /** Outgoing call sites
    */
  @Doc(info = "Call sites (outgoing calls)")
  def call: Iterator[Call] =
    traversal.flatMap(_._callViaContainsOut)

}
