package io.shiftleft.semanticcpg.language.types.structure

import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Import, NamespaceBlock}
import overflowdb.traversal._

class ImportTraversal(val traversal: Traversal[Import]) extends AnyVal {

  def call : Traversal[Call] = traversal.in(EdgeTypes.IS_CALL_FOR_IMPORT).cast[Call]

  def namespaceBlock: Traversal[NamespaceBlock] =
    call.flatMap(_.astIn).collectAll[NamespaceBlock]

}
