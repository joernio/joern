package io.shiftleft.semanticcpg.language.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.NodeExtension
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal._

import scala.jdk.CollectionConverters._

// Many method of this class should return individual nodes instead of Traversal[...].
// But over time through some opague implicits the versions returning Traversal[...]
// got exposed and for now we do not want to break the API.
class NodeMethods(val node: StoredNode) extends AnyVal with NodeExtension {

  def location(implicit finder: NodeExtensionFinder): NewLocation =
    LocationCreator(node)

  def file: Traversal[File] =
    Traversal.fromSingle(node).file

  def newTagNode(tagName: String): NewTagNodePairTraversal =
    newTagNodePair(tagName, "")

  def newTagNodePair(tagName: String, tagValue: String): NewTagNodePairTraversal = {
    new NewTagNodePairTraversal(
      Traversal.fromSingle(
        NewTagNodePair()
          .tag(NewTag().name(tagName).value(tagValue))
          .node(node)
      )
    )
  }

  def tagList: Traversal[NewTag] = {
    node._taggedByOut.asScala
      .map { case tagNode: HasName with HasValue =>
        (tagNode.name, Option(tagNode.value))
      }
      .distinct
      .collect { case (name, Some(value)) =>
        NewTag().name(name).value(value)
      }
    }

}
