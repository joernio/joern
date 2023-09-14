package io.shiftleft.semanticcpg.language

import io.shiftleft.codepropertygraph.generated.v2.nodes.NewLocation

trait HasLocation extends Any {
  def location: NewLocation
}
