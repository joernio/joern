package io.shiftleft.semanticcpg.language.types.structure

import io.shiftleft.codepropertygraph.generated.nodes.Literal
import io.shiftleft.semanticcpg.language.toLiteralMethods

class LiteralTraversal(val traversal: Iterator[Literal]) extends AnyVal {
  def strippedCode: Iterator[String] =
    traversal.map(_.strippedCode)
}
