package io.shiftleft.semanticcpg.language.types.structure

import io.shiftleft.codepropertygraph.generated.v2.nodes.*
import io.shiftleft.semanticcpg.language.*

/** A namespace, e.g., Java package or C# namespace
  */
class NamespaceTraversal(val traversal: Iterator[Namespace]) extends AnyVal {

  /** The type declarations defined in this namespace
    */
  def typeDecl: Iterator[TypeDecl] =
    traversal._refIn._astOut.collectAll[TypeDecl]

  /** Methods defined in this namespace
    */
  def method: Iterator[Method] =
    traversal._refIn._astOut.collectAll[Method]

  /** External namespaces - any namespaces which contain one or more external type.
    */
  def external: Iterator[Namespace] =
    traversal.where(_.typeDecl.external)

  /** Internal namespaces - any namespaces which contain one or more internal type
    */
  def internal: Iterator[Namespace] =
    traversal.where(_.typeDecl.internal)

}

object NamespaceTraversal {
  val globalNamespaceName = "<global>"
}
