package io.shiftleft.semanticcpg.language.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes.{StoredNode, Tag}
import io.shiftleft.semanticcpg.NodeExtension
import overflowdb.traversal._

import scala.jdk.CollectionConverters._

class StoredNodeMethods(val node: StoredNode) extends AnyVal with NodeExtension {
  def tag: Traversal[Tag] = {
    node._taggedByOut.asScala.map(_.asInstanceOf[Tag]).to(Traversal)
  }
}
