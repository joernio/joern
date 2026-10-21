package io.shiftleft.semanticcpg.language.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.NodeExtension
import io.shiftleft.semanticcpg.language.*

import scala.jdk.CollectionConverters.*

class StoredNodeMethods(val node: StoredNode) extends AnyVal with NodeExtension {
  def tag: Iterator[Tag] = node._taggedByOut.cast[Tag]

  def file: Iterator[File] = Iterator.single(node).file
}
