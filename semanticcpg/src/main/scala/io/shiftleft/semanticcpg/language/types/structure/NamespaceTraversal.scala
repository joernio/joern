package io.shiftleft.semanticcpg.language.types.structure

import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, NodeTypes}
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.{Traversal, toElementTraversal, toNodeTraversal}

/** A namespace, e.g., Java package or C# namespace
  */
class NamespaceTraversal(val traversal: Traversal[Namespace]) extends AnyVal {

  /** The type declarations defined in this namespace
    */
  def typeDecl: Traversal[TypeDecl] =
    traversal
      .in(EdgeTypes.REF)
      .out(EdgeTypes.AST)
      .hasLabel(NodeTypes.TYPE_DECL)
      .cast[TypeDecl]

  /** Methods defined in this namespace
    */
  def method: Traversal[Method] =
    traversal
      .in(EdgeTypes.REF)
      .out(EdgeTypes.AST)
      .hasLabel(NodeTypes.METHOD)
      .cast[Method]

  /** External namespaces - any namespaces which contain one or more external type.
    */
  def external: Traversal[Namespace] =
    traversal.where(_.typeDecl.external)

  /** Internal namespaces - any namespaces which contain one or more internal type
    */
  def internal: Traversal[Namespace] =
    traversal.where(_.typeDecl.internal)

}

object NamespaceTraversal {

  val globalNamespaceName = "<global>"

}
