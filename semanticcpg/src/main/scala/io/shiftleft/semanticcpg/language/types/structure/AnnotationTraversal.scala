package io.shiftleft.semanticcpg.language.types.structure

import io.shiftleft.codepropertygraph.generated.v2.nodes.*
import io.shiftleft.semanticcpg.language.*

/** An (Java-) annotation, e.g., @Test.
  */
class AnnotationTraversal(val traversal: Iterator[Annotation]) extends AnyVal {

  /** Traverse to parameter assignments
    */
  def parameterAssign: Iterator[AnnotationParameterAssign] =
    traversal.flatMap(_.annotationParameterAssignViaAstOut)

  /** Traverse to methods annotated with this annotation.
    */
  def method: Iterator[Method] =
    traversal.flatMap(_.methodViaAstIn)

  /** Traverse to type declarations annotated by this annotation
    */
  def typeDecl: Iterator[TypeDecl] =
    traversal.flatMap(_.typeDeclViaAstIn)

  /** Traverse to member annotated by this annotation
    */
  def member: Iterator[Member] =
    traversal.flatMap(_.memberViaAstIn)

  /** Traverse to parameter annotated by this annotation
    */
  def parameter: Iterator[MethodParameterIn] =
    traversal.flatMap(_.methodParameterInViaAstIn)
}
