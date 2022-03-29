package io.shiftleft.semanticcpg.language.bindingextension

import io.shiftleft.codepropertygraph.generated.nodes.{Binding, Method, TypeDecl}
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, NodeTypes}
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal._

class MethodTraversal(val traversal: Traversal[Method]) extends AnyVal {

  /** Traverse to type decl which have this method bound to it.
    */
  def bindingTypeDecl: Traversal[TypeDecl] =
    referencingBinding.bindingTypeDecl

  /** Traverse to bindings which reference to this method.
    */
  def referencingBinding: Traversal[Binding] =
    traversal.in(EdgeTypes.REF).where(_.hasLabel(NodeTypes.BINDING)).cast[Binding]
}
