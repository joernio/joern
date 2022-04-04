package io.shiftleft.semanticcpg.language.types.structure

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Member}
import overflowdb.traversal._

/** A member variable of a class/type.
  */
class MemberTraversal(val traversal: Traversal[Member]) extends AnyVal {

  /** Traverse to annotations of member
    */
  def annotation: Traversal[nodes.Annotation] =
    traversal.flatMap(_._annotationViaAstOut)

  /** Places where
    */
  def ref: Traversal[Call] =
    traversal.in(EdgeTypes.REF).cast[Call]

}
