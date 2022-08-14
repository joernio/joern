package io.shiftleft.semanticcpg.language.types.structure

import io.shiftleft.codepropertygraph.generated.nodes._
import overflowdb.traversal._

class NamespaceBlockTraversal(val traversal: Traversal[NamespaceBlock]) extends AnyVal {

  /** Namespaces for namespace blocks.
    */
  def namespace: Traversal[Namespace] =
    traversal.flatMap(_.refOut)

  /** The type declarations defined in this namespace
    */
  def typeDecl: Traversal[TypeDecl] =
    traversal.flatMap(_._typeDeclViaAstOut)

  def method: Traversal[Method] =
    traversal.flatMap(_._methodViaAstOut)
}
