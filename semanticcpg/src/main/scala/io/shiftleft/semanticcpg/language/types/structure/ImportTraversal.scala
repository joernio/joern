package io.shiftleft.semanticcpg.language.types.structure

import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{Import, NamespaceBlock}
import overflowdb.traversal._

class ImportTraversal(val traversal: Traversal[Import]) extends AnyVal {

  def namespaceBlock: Traversal[NamespaceBlock] = traversal
    .in(EdgeTypes.AST)
    .collect { case x: NamespaceBlock => x }
    .cast[NamespaceBlock]

}
