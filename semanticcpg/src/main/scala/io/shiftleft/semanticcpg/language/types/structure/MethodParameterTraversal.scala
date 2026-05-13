package io.shiftleft.semanticcpg.language.types.structure

import io.shiftleft.codepropertygraph.generated.help.{Doc, Traversal}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

import scala.jdk.CollectionConverters.*

/** Formal method input parameter */
@Traversal(elementType = classOf[MethodParameterIn])
class MethodParameterTraversal(val traversal: Iterator[MethodParameterIn]) extends AnyVal {

  /** Traverse to parameter annotations */
  @Doc(info = "Traverse to parameter annotations")
  def annotation: Iterator[Annotation] =
    traversal.flatMap(_._annotationViaAstOut)

  /** Traverse to all parameters with index greater or equal than `num` */
  @Doc(info = "Traverse to all parameters with index greater or equal than `num`")
  def indexFrom(num: Int): Iterator[MethodParameterIn] =
    traversal.filter(_.index >= num)

  def indexFrom(num: Option[Int]): Iterator[MethodParameterIn] =
    num match { case Some(value) => indexFrom(value); case None => Iterator.empty }

  /** Traverse to all parameters with index smaller or equal than `num` */
  @Doc(info = "Traverse to all parameters with index smaller or equal than `num`")
  def indexTo(num: Int): Iterator[MethodParameterIn] =
    traversal.filter(_.index <= num)

  def indexTo(num: Option[Int]): Iterator[MethodParameterIn] =
    num match { case Some(value) => indexTo(value); case None => Iterator.empty }

  /** Traverse to arguments (actual parameters) associated with this formal parameter */
  @Doc(info = "Traverse to arguments (actual parameters) associated with this formal parameter")
  def argument(implicit callResolver: ICallResolver): Iterator[Expression] = {
    for {
      paramIn <- traversal
      call    <- callResolver.getMethodCallsites(paramIn.method)
      case (arg: Expression) <- call._argumentOut
      if arg.argumentName match {
        case Some(name) => name == paramIn.name
        case None => arg.argumentIndex == paramIn.index || (paramIn.isVariadic && arg.argumentIndex > paramIn.index)
      }
    } yield arg
  }

}
