package io.shiftleft.semanticcpg.language.types.expressions

import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{Declaration, Identifier}
import overflowdb.traversal._

/** An identifier, e.g., an instance of a local variable, or a temporary variable
  */
class IdentifierTraversal(val traversal: Traversal[Identifier]) extends AnyVal {

  /** Traverse to all declarations of this identifier
    */
  def refsTo: Traversal[Declaration] =
    traversal.out(EdgeTypes.REF).cast[Declaration]

}
