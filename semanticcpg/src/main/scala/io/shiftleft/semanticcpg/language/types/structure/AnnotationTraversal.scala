package io.shiftleft.semanticcpg.language.types.structure

import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

/** An (Java-) annotation, e.g., @Test.
  */
class AnnotationTraversal(val traversal: Iterator[Annotation]) extends AnyVal {

  /** Traverse to parameter assignments
    */
  def parameterAssign: Iterator[AnnotationParameterAssign] =
    traversal.flatMap(_._annotationParameterAssignViaAstOut)

  /** Traverse to methods annotated with this annotation.
    */
  def method: Iterator[Method] =
    traversal.flatMap(_._methodViaAstIn)

  /** Traverse to type declarations annotated by this annotation
    */
  def typeDecl: Iterator[TypeDecl] =
    traversal.flatMap(_._typeDeclViaAstIn)

  /** Traverse to member annotated by this annotation
    */
  def member: Iterator[Member] =
    traversal.flatMap(_._memberViaAstIn)

  /** Traverse to parameter annotated by this annotation
    */
  def parameter: Iterator[MethodParameterIn] =
    traversal.flatMap(_._methodParameterInViaAstIn)
}
