package io.shiftleft.semanticcpg.language

import io.shiftleft.codepropertygraph.generated.nodes.NewLocation

trait HasLocation extends Any {
  def location: NewLocation
}
