package io.shiftleft.semanticcpg.language.types.structure

import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Import, NamespaceBlock}
import io.shiftleft.semanticcpg.language._

class ImportTraversal(val traversal: Iterator[Import]) extends AnyVal {

  /** Traverse to the call that represents the import in the AST
    */
  def call: Iterator[Call] = traversal.in(EdgeTypes.IS_CALL_FOR_IMPORT).cast[Call]

  def namespaceBlock: Iterator[NamespaceBlock] =
    call.method.namespaceBlock

}
