package io.shiftleft.semanticcpg.language.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes.{HasName, HasValue, NewTag, StoredNode, Tag}
import io.shiftleft.semanticcpg.NodeExtension
import overflowdb.traversal._

import scala.jdk.CollectionConverters._

class StoredNodeMethods(val node: StoredNode) extends AnyVal with NodeExtension {
  def tag: Traversal[Tag] = {
    node._taggedByOut.asScala.map(_.asInstanceOf[Tag]).to(Traversal)
  }

  def tagList: Traversal[NewTag] = {
    node._taggedByOut.asScala
      .map { case tagNode: HasName with HasValue =>
        (tagNode.name, Option(tagNode.value))
      }
      .distinct
      .collect { case (name, Some(value)) =>
        NewTag()
          .name(name)
          .value(value)
      }
    }
}
