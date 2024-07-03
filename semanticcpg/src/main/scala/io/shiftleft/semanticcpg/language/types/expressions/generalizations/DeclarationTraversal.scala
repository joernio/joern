package io.shiftleft.semanticcpg.language.types.expressions.generalizations

import io.shiftleft.codepropertygraph.generated.help.{Doc, Traversal}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

/** A declaration, such as a local or parameter. */
@Traversal(elementType = classOf[Declaration])
class DeclarationTraversal[NodeType <: Declaration](val traversal: Iterator[NodeType]) extends AnyVal {

  /** The closure binding node referenced by this declaration */
  @Doc(info = "The closure binding node referenced by this declaration")
  def closureBinding: Iterator[ClosureBinding] = traversal.flatMap(_._refIn).collectAll[ClosureBinding]

  /** Methods that capture this declaration */
  @Doc(info = "Methods that capture this declaration")
  def capturedByMethodRef: Iterator[MethodRef] = closureBinding.flatMap(_._captureIn).collectAll[MethodRef]

  /** Types that capture this declaration */
  @Doc(info = "Types that capture this declaration")
  def capturedByTypeRef: Iterator[TypeRef] = closureBinding.flatMap(_._captureIn).collectAll[TypeRef]

  /** The parent method. */
  @Doc(info = "The parent method.")
  def method: Iterator[Method] = traversal.flatMap {
    case x: Local             => x.method
    case x: MethodParameterIn => x.method
    case x: Method            => Iterator(x)
    case x: TypeDecl          => x.method
    case _                    => Iterator()
  }

}
