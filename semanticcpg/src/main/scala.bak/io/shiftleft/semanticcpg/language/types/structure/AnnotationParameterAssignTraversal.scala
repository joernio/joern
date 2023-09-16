package io.shiftleft.semanticcpg.language.types.structure

import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

/** An annotation parameter-assignment, e.g., `foo=value` in @Test(foo=value)
  */
class AnnotationParameterAssignTraversal(val traversal: Iterator[AnnotationParameterAssign]) extends AnyVal {

  /** Traverse to all annotation parameters
    */
  def parameter: Iterator[AnnotationParameter] =
    traversal.flatMap(_._annotationParameterViaAstOut)

  /** Traverse to all values of annotation parameters
    */
  def value: Iterator[Expression] =
    traversal
      .flatMap(_.astOut)
      .filterNot(_.isInstanceOf[AnnotationParameter])
      .cast[Expression]
}
