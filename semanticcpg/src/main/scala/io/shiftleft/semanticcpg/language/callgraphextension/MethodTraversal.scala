package io.shiftleft.semanticcpg.language.callgraphextension

import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Method}
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.help.Doc
import overflowdb.traversal.{PathAwareTraversal, Traversal, toNodeTraversal}

class MethodTraversal(val traversal: Traversal[Method]) extends AnyVal {

  /** Intended for internal use! Traverse to direct and transitive callers of the method.
    */
  def calledByIncludingSink(sourceTrav: Traversal[Method])(implicit callResolver: ICallResolver): Traversal[Method] = {
    val sourceMethods = sourceTrav.toSet
    val sinkMethods   = traversal.dedup

    if (sourceMethods.isEmpty || sinkMethods.isEmpty) {
      PathAwareTraversal.empty
    } else {
      sinkMethods
        .repeat(
          _.flatMap(callResolver.getMethodCallsitesAsTraversal)
            .in(EdgeTypes.CONTAINS) // expand to method
        )(_.dedup.emit(_.collect {
          case method: Method if sourceMethods.contains(method) => method
        }))
        .cast[Method]
    }
  }

  /** Traverse to direct callers of this method
    */
  def caller(implicit callResolver: ICallResolver): Traversal[Method] =
    callIn(callResolver).method

  /** Traverse to methods called by this method
    */
  def callee(implicit callResolver: ICallResolver): Traversal[Method] =
    call.callee(callResolver)

  /** Incoming call sites
    */
  def callIn(implicit callResolver: ICallResolver): Traversal[Call] =
    traversal.flatMap(method => callResolver.getMethodCallsitesAsTraversal(method).collectAll[Call])

  /** Traverse to direct and transitive callers of the method.
    */
  def calledBy(sourceTrav: Traversal[Method])(implicit callResolver: ICallResolver): Traversal[Method] =
    caller(callResolver).calledByIncludingSink(sourceTrav)(callResolver)

  @deprecated("Use call", "")
  def callOut: Traversal[Call] =
    call

  @deprecated("Use call", "")
  def callOutRegex(regex: String)(implicit callResolver: ICallResolver): Traversal[Call] =
    call(regex)

  /** Outgoing call sites to methods where fullName matches `regex`.
    */
  def call(regex: String)(implicit callResolver: ICallResolver): Traversal[Call] =
    call.where(_.callee.fullName(regex))

  /** Outgoing call sites
    */
  @Doc(info = "Call sites (outgoing calls)")
  def call: Traversal[Call] =
    traversal.out(EdgeTypes.CONTAINS).collectAll[Call]

}
