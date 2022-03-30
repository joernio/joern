package io.shiftleft.semanticcpg.language.types.structure

import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal._

import scala.jdk.CollectionConverters._

/** Formal method input parameter
  */
@help.Traversal(elementType = classOf[MethodParameterIn])
class MethodParameterTraversal(val traversal: Traversal[MethodParameterIn]) extends AnyVal {

  /** Traverse to all `num`th parameters
    */
  def index(num: Int): Traversal[MethodParameterIn] =
    traversal.order(num)

  /** Traverse to all parameters with index greater or equal than `num`
    */
  def indexFrom(num: Int): Traversal[MethodParameterIn] =
    traversal.filter(_.order >= num)

  /** Traverse to all parameters with index smaller or equal than `num`
    */
  def indexTo(num: Int): Traversal[MethodParameterIn] =
    traversal.filter(_.order <= num)

  /** Traverse to arguments (actual parameters) associated with this formal parameter
    */
  def argument(implicit callResolver: ICallResolver): Traversal[Expression] =
    for {
      paramIn <- traversal
      call    <- callResolver.getMethodCallsites(paramIn.method)
      arg     <- call._argumentOut.asScala.collect { case node: Expression with HasArgumentIndex => node }
      if arg.argumentIndex == paramIn.order
    } yield arg

}
