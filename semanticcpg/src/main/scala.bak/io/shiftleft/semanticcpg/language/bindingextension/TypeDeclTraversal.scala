package io.shiftleft.semanticcpg.language.bindingextension

import io.shiftleft.codepropertygraph.generated.nodes.{Binding, Method, TypeDecl}
import io.shiftleft.semanticcpg.language.*

class TypeDeclTraversal(val traversal: Iterator[TypeDecl]) extends AnyVal {

  /** Traverse to methods bound to this type decl.
    */
  def boundMethod: Iterator[Method] =
    methodBinding.boundMethod

  /** Traverse to the method bindings of this type declaration.
    */
  def methodBinding: Iterator[Binding] =
    traversal.canonicalType.flatMap(_.bindsOut)

}
