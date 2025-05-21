package io.shiftleft.semanticcpg.language.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes.MethodParameterIn
import io.shiftleft.semanticcpg.NodeExtension
import io.shiftleft.semanticcpg.language.*

class MethodParameterInMethods(val paramIn: MethodParameterIn)
    extends AnyVal
    with NodeExtension
    with HasLocation {
  override def location: LocationInfo = {
    Location(paramIn)
  }
}
