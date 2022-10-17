package io.shiftleft.semanticcpg.language.types.structure

import io.shiftleft.codepropertygraph.generated.{EdgeTypes, nodes}
import io.shiftleft.codepropertygraph.generated.nodes.Import
import overflowdb.traversal.Traversal

class DependencyTraversal(val traversal: Traversal[nodes.Dependency]) extends AnyVal {
  def imports: Traversal[Import] = traversal.in(EdgeTypes.IMPORTS).cast[Import]
}
