package io.shiftleft.semanticcpg.language.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.NodeExtension
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal._

import scala.jdk.CollectionConverters._

// Many method of this class should return individual nodes instead of Traversal[...].
// But over time through some opague implicits the versions returning Traversal[...]
// got exposed and for now we do not want to break the API.
class NodeMethods(val node: AbstractNode) extends AnyVal with NodeExtension {

  def location(implicit finder: NodeExtensionFinder): NewLocation =
    node match {
      case storedNode: StoredNode => LocationCreator(storedNode)
      case _                      => LocationCreator.emptyLocation("", None)
    }

  def file: Traversal[File] =
    node match {
      case storedNode: StoredNode =>
        Traversal.fromSingle(storedNode).file
      case _ =>
        Traversal.empty
    }

  def newTagNode(tagName: String): NewTagNodePairTraversal = newTagNodePair(tagName, "")

  def newTagNodePair(tagName: String, tagValue: String): NewTagNodePairTraversal = {
    new NewTagNodePairTraversal(
      Traversal.fromSingle(
        NewTagNodePair()
          .tag(NewTag().name(tagName).value(tagValue))
          .node(node)
      )
    )
  }

  def tagList: Traversal[TagBase] =
    node match {
      case storedNode: StoredNode =>
        storedNode._taggedByOut.asScala
          .map { case tagNode: HasName with HasValue =>
            (tagNode.name, Option(tagNode.value))
          }
          .distinct
          .collect { case (name, Some(value)) =>
            NewTag()
              .name(name)
              .value(value)
              .asInstanceOf[TagBase]
          }
      case _ =>
        Traversal.empty
    }

}
