package io.shiftleft.semanticcpg.language.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.NodeExtension
import io.shiftleft.semanticcpg.language._

import scala.jdk.CollectionConverters._

class StoredNodeMethods(val node: StoredNode) extends AnyVal with NodeExtension {
  def tag: Traversal[Tag] = {
    node._taggedByOut
      .cast[Tag]
      .distinctBy(tag => (tag.name, tag.value))
  }

  def file: Traversal[File] =
    Iterator.single(node).file
}
