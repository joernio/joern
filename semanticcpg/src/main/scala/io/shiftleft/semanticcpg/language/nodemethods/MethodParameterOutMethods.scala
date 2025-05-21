package io.shiftleft.semanticcpg.language.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes.MethodParameterOut
import io.shiftleft.semanticcpg.NodeExtension
import io.shiftleft.semanticcpg.language.*

class MethodParameterOutMethods(val paramOut: MethodParameterOut) extends AnyVal with NodeExtension with HasLocation {
  override def location: LocationInfo = {
    Location(paramOut)
  }
}
