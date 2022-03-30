package io.shiftleft.semanticcpg.language.bindingextension

import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{Binding, Method, TypeDecl}
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal._

class TypeDeclTraversal(val traversal: Traversal[TypeDecl]) extends AnyVal {

  /** Traverse to methods bound to this type decl.
    */
  def boundMethod: Traversal[Method] =
    methodBinding.boundMethod

  /** Traverse to the method bindings of this type declaration.
    */
  def methodBinding: Traversal[Binding] =
    traversal.canonicalType.out(EdgeTypes.BINDS).cast[Binding]

}
