package io.shiftleft.semanticcpg.language.types.expressions

import io.shiftleft.codepropertygraph.generated.nodes.{Declaration, Identifier}
import io.shiftleft.semanticcpg.language.*

/** An identifier, e.g., an instance of a local variable, or a temporary variable
  */
class IdentifierTraversal(val traversal: Iterator[Identifier]) extends AnyVal {

  /** Traverse to all declarations of this identifier
    */
  def refsTo: Iterator[Declaration] = {
    traversal.flatMap(_.refOut).cast[Declaration]
  }

}
