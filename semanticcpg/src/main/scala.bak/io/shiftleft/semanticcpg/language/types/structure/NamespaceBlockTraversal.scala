package io.shiftleft.semanticcpg.language.types.structure

import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class NamespaceBlockTraversal(val traversal: Iterator[NamespaceBlock]) extends AnyVal {

  /** Namespaces for namespace blocks.
    */
  def namespace: Iterator[Namespace] =
    traversal.flatMap(_.refOut)

  /** The type declarations defined in this namespace
    */
  def typeDecl: Iterator[TypeDecl] =
    traversal.flatMap(_._typeDeclViaAstOut)

  def method: Iterator[Method] =
    traversal.flatMap(_._methodViaAstOut)
}
