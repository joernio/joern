package io.shiftleft.semanticcpg.language.types.structure

import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.Traversal

/** A namespace, e.g., Java package or C# namespace
  */
class NamespaceTraversal(val traversal: Traversal[Namespace]) extends AnyVal {

  /** The type declarations defined in this namespace
    */
  def typeDecl: Traversal[TypeDecl] =
    traversal.flatMap(_.refIn).flatMap(_._typeDeclViaAstOut)

  /** Methods defined in this namespace
    */
  def method: Traversal[Method] =
    traversal.flatMap(_.refIn).flatMap(_._methodViaAstOut)

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
