package io.shiftleft.semanticcpg.language.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes.{MethodParameterOut, NewLocation}
import io.shiftleft.semanticcpg.NodeExtension
import io.shiftleft.semanticcpg.language.*

class MethodParameterOutMethods(val paramOut: MethodParameterOut)
    extends AnyVal
    with NodeExtension
    with HasLocation
    with HasLoc {
  @deprecated("Prefer .loc to .location")
  override def location: NewLocation = {
    LocationCreator.defaultCreateLocation(paramOut)
  }

  override def loc: LocationInfo = {
    Loc(paramOut)
  }
}
