package io.shiftleft.semanticcpg.language.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes.{MethodParameterIn, NewLocation}
import io.shiftleft.semanticcpg.NodeExtension
import io.shiftleft.semanticcpg.language.*

class MethodParameterInMethods(val paramIn: MethodParameterIn)
    extends AnyVal
    with NodeExtension
    with HasLocation
    with HasLoc {
  @deprecated("Prefer .loc to .location")
  override def location: NewLocation = {
    LocationCreator.defaultCreateLocation(paramIn)
  }

  override def loc: LocationInfo = {
    Loc(paramIn)
  }
}
