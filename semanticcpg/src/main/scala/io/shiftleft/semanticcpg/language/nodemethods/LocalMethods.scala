package io.shiftleft.semanticcpg.language.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes.{Local, Method}
import io.shiftleft.semanticcpg.NodeExtension
import io.shiftleft.semanticcpg.language.*

class LocalMethods(val local: Local) extends AnyVal with NodeExtension with HasLocation {
  override def location: LocationInfo = {
    local.location
  }

  /** The method hosting this local variable
    */
  def method: Iterator[Method] =
    Iterator.single(local).method
}
