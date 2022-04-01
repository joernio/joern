package io.shiftleft.semanticcpg.language.types.structure

import io.shiftleft.codepropertygraph.generated.nodes
import overflowdb.traversal._

/** An (Java-) annotation, e.g., @Test.
  */
class AnnotationTraversal(val traversal: Traversal[nodes.Annotation]) extends AnyVal {

  /** Traverse to parameter assignments
    */
  def parameterAssign: Traversal[nodes.AnnotationParameterAssign] =
    traversal.flatMap(_._annotationParameterAssignViaAstOut)

  /** Traverse to methods annotated with this annotation.
    */
  def method: Traversal[nodes.Method] =
    traversal.flatMap(_._methodViaAstIn)

  /** Traverse to type declarations annotated by this annotation
    */
  def typeDecl: Traversal[nodes.TypeDecl] =
    traversal.flatMap(_._typeDeclViaAstIn)

  /** Traverse to member annotated by this annotation
    */
  def member: Traversal[nodes.Member] =
    traversal.flatMap(_._memberViaAstIn)

  /** Traverse to parameter annotated by this annotation
    */
  def parameter: Traversal[nodes.MethodParameterIn] =
    traversal.flatMap(_._methodParameterInViaAstIn)
}
