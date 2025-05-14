package io.shiftleft.semanticcpg.language.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes.{Local, Method, NewLocation}
import io.shiftleft.semanticcpg.NodeExtension
import io.shiftleft.semanticcpg.language.*

class LocalMethods(val local: Local) extends AnyVal with NodeExtension with HasLocation with HasLoc {
  @deprecated("Prefer .loc to .location")
  override def location: NewLocation = {
    LocationCreator.defaultCreateLocation(local)
  }

  override def loc: LocationInfo = {
    Loc(local)
  }

  /** The method hosting this local variable
    */
  def method: Iterator[Method] =
    Iterator.single(local).method
}
