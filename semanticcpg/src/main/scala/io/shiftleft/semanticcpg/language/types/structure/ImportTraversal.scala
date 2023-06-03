package io.shiftleft.semanticcpg.language.types.structure

import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Import, NamespaceBlock}
import io.shiftleft.semanticcpg.language._

class ImportTraversal(val traversal: Traversal[Import]) extends AnyVal {

  /** Traverse to the call that represents the import in the AST
    */
  def call: Traversal[Call] = traversal._isCallForImportIn.cast[Call]

  def namespaceBlock: Traversal[NamespaceBlock] =
    call.method.namespaceBlock

}
