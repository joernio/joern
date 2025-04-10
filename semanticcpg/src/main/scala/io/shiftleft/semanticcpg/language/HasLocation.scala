package io.shiftleft.semanticcpg.language

import io.shiftleft.codepropertygraph.generated.nodes.NewLocation

@deprecated("Prefer HasLoc to HasLocation: Location and NewLocation are deprecated.")
trait HasLocation extends Any {
  def location: NewLocation
}

trait HasLoc extends Any {
  def loc: Loc
}
