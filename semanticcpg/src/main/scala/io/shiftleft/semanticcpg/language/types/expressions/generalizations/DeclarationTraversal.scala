package io.shiftleft.semanticcpg.language.types.expressions.generalizations

import io.shiftleft.codepropertygraph.generated.nodes.{ClosureBinding, Declaration, MethodRef}
import overflowdb.traversal.{Traversal, help, jIteratortoTraversal}

/** A declaration, such as a local or parameter.
  */
@help.Traversal(elementType = classOf[Declaration])
class DeclarationTraversal[NodeType <: Declaration](val traversal: Traversal[NodeType]) extends AnyVal {

  /** The closure binding node referenced by this declaration
    */
  def closureBinding: Traversal[ClosureBinding] = traversal.flatMap(_._refIn).collectAll[ClosureBinding]

  /** Methods that capture this declaration
    */
  def capturedByMethodRef: Traversal[MethodRef] = closureBinding.flatMap(_._captureIn).collectAll[MethodRef]

  /** Types that capture this declaration
    */
  def capturedByTypeRef: Traversal[MethodRef] = closureBinding.flatMap(_._captureIn).collectAll[MethodRef]

}
