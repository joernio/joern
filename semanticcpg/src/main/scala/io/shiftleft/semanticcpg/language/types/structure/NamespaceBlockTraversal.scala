package io.shiftleft.semanticcpg.language.types.structure

import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, NodeTypes}
import overflowdb.traversal._

class NamespaceBlockTraversal(val traversal: Traversal[NamespaceBlock]) extends AnyVal {

  /** Namespaces for namespace blocks.
    */
  def namespace: Traversal[Namespace] =
    traversal.out(EdgeTypes.REF).cast[Namespace]

  /** The type declarations defined in this namespace
    */
  def typeDecl: Traversal[TypeDecl] =
    traversal
      .out(EdgeTypes.AST)
      .hasLabel(NodeTypes.TYPE_DECL)
      .cast[TypeDecl]

  def method: Traversal[Method] =
    traversal
      .out(EdgeTypes.AST)
      .hasLabel(NodeTypes.METHOD)
      .cast[Method]
}
