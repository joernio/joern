package io.shiftleft.semanticcpg.language.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes.{MethodParameterIn, NewLocation}
import io.shiftleft.semanticcpg.NodeExtension
import io.shiftleft.semanticcpg.language.*

class MethodParameterInMethods(val paramIn: MethodParameterIn) extends AnyVal with NodeExtension with HasLocation {
  override def location: NewLocation = {
    LocationCreator(paramIn, paramIn.name, paramIn.label, paramIn.lineNumber, paramIn.method)
  }
}
