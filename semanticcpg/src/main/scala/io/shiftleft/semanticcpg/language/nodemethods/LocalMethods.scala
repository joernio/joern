package io.shiftleft.semanticcpg.language.nodemethods

import io.shiftleft.codepropertygraph.generated.v2.accessors.Lang.*
import io.shiftleft.codepropertygraph.generated.v2.nodes.{Local, Method, NewLocation}
import io.shiftleft.semanticcpg.NodeExtension
import io.shiftleft.semanticcpg.language.*

class LocalMethods(val local: Local) extends AnyVal with NodeExtension with HasLocation {
  override def location: NewLocation = {
    LocationCreator(local, local.name, local.label, local.lineNumber, method.head)
  }

  /** The method hosting this local variable
    */
  def method: Iterator[Method] =
    Iterator.single(local).method
}
