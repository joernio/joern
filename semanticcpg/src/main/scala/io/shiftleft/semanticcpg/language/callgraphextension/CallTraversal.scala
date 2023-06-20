package io.shiftleft.semanticcpg.language.callgraphextension

import io.shiftleft.codepropertygraph.generated.nodes.{Call, Import, Method}
import io.shiftleft.semanticcpg.language._

class CallTraversal(val traversal: Traversal[Call]) extends AnyVal {

  @deprecated("Use callee", "")
  def calledMethod(implicit callResolver: ICallResolver): Traversal[Method] = callee

  /** The callee method */
  def callee(implicit callResolver: ICallResolver): Traversal[Method] =
    traversal.flatMap(callResolver.getCalledMethodsAsTraversal)

  def referencedImports: Traversal[Import] =
    traversal.flatMap(_._importViaIsCallForImportOut)

}
