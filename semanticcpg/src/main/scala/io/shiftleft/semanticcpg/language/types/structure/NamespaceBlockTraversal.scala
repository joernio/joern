package io.shiftleft.semanticcpg.language.types.structure

import io.shiftleft.codepropertygraph.generated.v2.nodes.*
import io.shiftleft.semanticcpg.language.*

class NamespaceBlockTraversal(val traversal: Iterator[NamespaceBlock]) extends AnyVal {

  /** Namespaces for namespace blocks.
   * TODO define a name in the schema
    */
  def namespace: Iterator[Namespace] =
    traversal.flatMap(_.namespaceViaRefOut)

  /** The type declarations defined in this namespace
   * TODO define a name in the schema
    */
  def typeDecl: Iterator[TypeDecl] =
    traversal.flatMap(_.typeDeclViaAstOut)

  // TODO define a name in the schema
  def method: Iterator[Method] =
    traversal.flatMap(_.methodViaAstOut)
}
