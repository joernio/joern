package io.shiftleft.semanticcpg.language.callgraphextension

import io.shiftleft.codepropertygraph.generated.nodes.{Call, Import, Method}
import io.shiftleft.semanticcpg.language.*

class CallTraversal(val traversal: Iterator[Call]) extends AnyVal {

  @deprecated("Use callee", "")
  def calledMethod(implicit callResolver: ICallResolver): Iterator[Method] = callee

  /** The callee method */
  def callee(implicit callResolver: ICallResolver): Iterator[Method] =
    traversal.flatMap(callResolver.getCalledMethodsAsTraversal)

  def referencedImports: Iterator[Import] =
    traversal.flatMap(_._importViaIsCallForImportOut)

}
