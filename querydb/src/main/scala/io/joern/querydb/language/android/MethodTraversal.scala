package io.joern.querydb.language.android

import io.shiftleft.codepropertygraph.generated.nodes
import overflowdb.traversal._
import io.shiftleft.semanticcpg.language._

class MethodTraversal(val traversal: Traversal[nodes.Method]) extends AnyVal {
  def exposedToJS =
    traversal.where(_.annotation.fullNameExact("android.webkit.JavascriptInterface"))
}
