package io.shiftleft.semanticcpg.language.types.structure

import io.shiftleft.codepropertygraph.generated.nodes
import overflowdb.traversal.Traversal

/** An annotation parameter-assignment, e.g., `foo=value` in @Test(foo=value)
  */
class AnnotationParameterAssignTraversal(val traversal: Traversal[nodes.AnnotationParameterAssign]) extends AnyVal {

  /** Traverse to all annotation parameters
    */
  def parameter: Traversal[nodes.AnnotationParameter] =
    traversal
      .flatMap(_._annotationParameterViaAstOut)

  /** Traverse to all values of annotation parameters
    */
  def value: Traversal[nodes.Expression] =
    traversal
      .flatMap(_.astOut)
      .filterNot(_.isInstanceOf[nodes.AnnotationParameter])
      .cast[nodes.Expression]
}
