package io.shiftleft.semanticcpg.language.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.NodeExtension
import io.shiftleft.semanticcpg.language.*

class NodeMethods(val node: AbstractNode) extends AnyVal with NodeExtension {
  def location(implicit finder: NodeExtensionFinder): LocationInfo =
    node match {
      case storedNode: StoredNode => Location(storedNode)
      case _                      => Location(node)
    }
}
