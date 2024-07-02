package io.shiftleft.semanticcpg.language.types.structure

import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class ImportTraversal(val traversal: Iterator[Import]) extends AnyVal {

  /** Traverse to the call that represents the import in the AST
    */
  def call: Iterator[Call] = traversal._isCallForImportIn.cast[Call]

  def namespaceBlock: Iterator[NamespaceBlock] =
    call.method.namespaceBlock

}
