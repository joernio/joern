package io.shiftleft.semanticcpg.language.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.NodeExtension
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal._

import scala.jdk.CollectionConverters._

class StoredNodeMethods(val node: StoredNode) extends AnyVal with NodeExtension {
  def tag: Traversal[Tag] = {
    node._taggedByOut.asScala.map(_.asInstanceOf[Tag]).to(Traversal)
  }

  def file: Traversal[File] =
    Traversal.fromSingle(node).file

  def tagList: Traversal[Tag] = {
    node._taggedByOut.to(Traversal).collectAll[Tag]
  }
}
