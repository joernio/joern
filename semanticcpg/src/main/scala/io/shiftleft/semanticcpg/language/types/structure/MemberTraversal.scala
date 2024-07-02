package io.shiftleft.semanticcpg.language.types.structure

import io.shiftleft.codepropertygraph.generated.nodes.{Annotation, Call, Member}
import io.shiftleft.semanticcpg.language.*

/** A member variable of a class/type.
  */
class MemberTraversal(val traversal: Iterator[Member]) extends AnyVal {

  /** Traverse to annotations of member
    */
  def annotation: Iterator[Annotation] =
    traversal.flatMap(_._annotationViaAstOut)

  /** Places where
    */
  def ref: Iterator[Call] =
    traversal.flatMap(_._callViaRefIn)

}
