package io.shiftleft.semanticcpg.language.android

import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.semanticcpg.language.*

class MethodTraversal(val traversal: Iterator[nodes.Method]) extends AnyVal {
  def exposedToJS =
    traversal.where(_.annotation.fullNameExact("android.webkit.JavascriptInterface"))
}
