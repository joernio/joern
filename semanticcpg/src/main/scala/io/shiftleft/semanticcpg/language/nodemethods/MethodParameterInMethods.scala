package io.shiftleft.semanticcpg.language.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes.{Expression, MethodParameterIn, NewLocation}
import io.shiftleft.semanticcpg.NodeExtension
import io.shiftleft.semanticcpg.language.*

class MethodParameterInMethods(val paramIn: MethodParameterIn) extends AnyVal with NodeExtension with HasLocation {
  override def location: NewLocation = {
    LocationCreator.defaultCreateLocation(paramIn)
  }

  /** Traverse to arguments (actual parameters) associated with this formal parameter */
  def argument(implicit callResolver: ICallResolver): Iterator[Expression] = {
    for {
      call <- callResolver.getMethodCallsites(paramIn.method)
      case (arg: Expression) <- call._argumentOut
      if arg.argumentName match {
        case Some(name) => name == paramIn.name
        case None       => arg.argumentIndex == paramIn.index
      }
    } yield arg
  }
}
