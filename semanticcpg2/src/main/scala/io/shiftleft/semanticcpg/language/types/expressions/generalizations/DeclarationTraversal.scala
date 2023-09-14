package io.shiftleft.semanticcpg.language.types.expressions.generalizations

import io.shiftleft.codepropertygraph.generated.nodes.{ClosureBinding, Declaration, MethodRef, TypeRef}
import io.shiftleft.semanticcpg.language.*
import overflowdb.traversal.help

/** A declaration, such as a local or parameter.
  */
@help.Traversal(elementType = classOf[Declaration])
class DeclarationTraversal[NodeType <: Declaration](val traversal: Iterator[NodeType]) extends AnyVal {

  /** The closure binding node referenced by this declaration
    */
  def closureBinding: Iterator[ClosureBinding] = traversal.flatMap(_._refIn).collectAll[ClosureBinding]

  /** Methods that capture this declaration
    */
  def capturedByMethodRef: Iterator[MethodRef] = closureBinding.flatMap(_._captureIn).collectAll[MethodRef]

  /** Types that capture this declaration
    */
  def capturedByTypeRef: Iterator[TypeRef] = closureBinding.flatMap(_._captureIn).collectAll[TypeRef]

}
