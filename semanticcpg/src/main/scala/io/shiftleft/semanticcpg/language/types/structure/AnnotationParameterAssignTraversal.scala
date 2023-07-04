package io.shiftleft.semanticcpg.language.types.structure

import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

/** An annotation parameter-assignment, e.g., `foo=value` in @Test(foo=value)
  */
class AnnotationParameterAssignTraversal(val traversal: Traversal[AnnotationParameterAssign]) extends AnyVal {

  /** Traverse to all annotation parameters
    */
  def parameter: Traversal[AnnotationParameter] =
    traversal.flatMap(_._annotationParameterViaAstOut)

  /** Traverse to all values of annotation parameters
    */
  def value: Traversal[Expression] =
    traversal
      .flatMap(_.astOut)
      .filterNot(_.isInstanceOf[AnnotationParameter])
      .cast[Expression]
}
