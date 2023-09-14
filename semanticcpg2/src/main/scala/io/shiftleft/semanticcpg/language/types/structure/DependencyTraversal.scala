package io.shiftleft.semanticcpg.language.types.structure

import io.shiftleft.codepropertygraph.generated.nodes.Import
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, nodes}
import io.shiftleft.semanticcpg.language.*

class DependencyTraversal(val traversal: Iterator[nodes.Dependency]) extends AnyVal {
  def imports: Iterator[Import] = traversal.in(EdgeTypes.IMPORTS).cast[Import]
}
