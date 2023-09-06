package io.shiftleft.semanticcpg.language.bindingextension

import io.shiftleft.codepropertygraph.generated.nodes.{Binding, Method, TypeDecl}
import io.shiftleft.semanticcpg.language.*

class MethodTraversal(val traversal: Iterator[Method]) extends AnyVal {

  /** Traverse to type decl which have this method bound to it.
    */
  def bindingTypeDecl: Iterator[TypeDecl] =
    referencingBinding.bindingTypeDecl

  /** Traverse to bindings which reference to this method.
    */
  def referencingBinding: Iterator[Binding] =
    traversal.flatMap(_._bindingViaRefIn)
}
