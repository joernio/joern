package io.shiftleft.semanticcpg.language.types.structure

import io.shiftleft.codepropertygraph.generated.v2.nodes.*
import io.shiftleft.semanticcpg.language.*

class DependencyTraversal(val traversal: Iterator[Dependency]) extends AnyVal {
  def imports: Iterator[Import] =
    traversal._importsIn.cast[Import]
}
