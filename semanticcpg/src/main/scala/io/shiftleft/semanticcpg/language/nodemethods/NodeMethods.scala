package io.shiftleft.semanticcpg.language.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.NodeExtension
import io.shiftleft.semanticcpg.language.*

class NodeMethods(val node: AbstractNode) extends AnyVal with NodeExtension {
  def location(implicit locationCreator: LocationCreator): LocationInfo =
    node match {
      case storedNode: StoredNode => locationCreator(storedNode)
      case _                      => EmptyLocation
    }
}
