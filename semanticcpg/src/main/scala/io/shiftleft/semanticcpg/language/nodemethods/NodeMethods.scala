package io.shiftleft.semanticcpg.language.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes.{AbstractNode, NewLocation, StoredNode}
import io.shiftleft.semanticcpg.NodeExtension
import io.shiftleft.semanticcpg.language._

class NodeMethods(val node: AbstractNode) extends AnyVal with NodeExtension {

  def location(implicit finder: NodeExtensionFinder): NewLocation =
    node match {
      case storedNode: StoredNode => LocationCreator(storedNode)
      case _                      => LocationCreator.emptyLocation("", None)
    }

}
