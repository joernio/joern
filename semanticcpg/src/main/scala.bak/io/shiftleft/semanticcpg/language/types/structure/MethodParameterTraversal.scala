package io.shiftleft.semanticcpg.language.types.structure

import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import overflowdb.traversal.help

import scala.jdk.CollectionConverters.*

/** Formal method input parameter
  */
@help.Traversal(elementType = classOf[MethodParameterIn])
class MethodParameterTraversal(val traversal: Iterator[MethodParameterIn]) extends AnyVal {

  /** Traverse to parameter annotations
    */
  def annotation: Iterator[Annotation] =
    traversal.flatMap(_._annotationViaAstOut)

  /** Traverse to all parameters with index greater or equal than `num`
    */
  def indexFrom(num: Int): Iterator[MethodParameterIn] =
    traversal.filter(_.index >= num)

  /** Traverse to all parameters with index smaller or equal than `num`
    */
  def indexTo(num: Int): Iterator[MethodParameterIn] =
    traversal.filter(_.index <= num)

  /** Traverse to arguments (actual parameters) associated with this formal parameter
    */
  def argument(implicit callResolver: ICallResolver): Iterator[Expression] =
    for {
      paramIn <- traversal
      call    <- callResolver.getMethodCallsites(paramIn.method)
      arg     <- call._argumentOut.collectAll[Expression]
      if arg.argumentIndex == paramIn.index
    } yield arg

}
