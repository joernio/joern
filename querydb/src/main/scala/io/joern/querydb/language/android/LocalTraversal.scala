package io.joern.querydb.language.android

import io.shiftleft.codepropertygraph.generated.nodes.Local
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal._

class LocalTraversal(val traversal: Traversal[Local]) extends AnyVal {
  def callsEnableJS =
    traversal
      .where(
        _.referencingIdentifiers.inCall
          .nameExact("getSettings")
          .where(
            _.inCall
              .nameExact("setJavaScriptEnabled")
              .argument
              .isLiteral
              .codeExact("true")
          )
      )

  def loadUrlCalls =
    traversal.referencingIdentifiers.inCall.nameExact("loadUrl")

  def addJavascriptInterfaceCalls =
    traversal.referencingIdentifiers.inCall.nameExact("addJavascriptInterface")
}
