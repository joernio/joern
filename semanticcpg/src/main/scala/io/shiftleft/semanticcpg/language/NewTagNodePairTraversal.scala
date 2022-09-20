package io.shiftleft.semanticcpg.language

import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{NewNode, NewTagNodePair}
import io.shiftleft.codepropertygraph.generated.nodes.StoredNode
import overflowdb.traversal._
import overflowdb.BatchedUpdate.DiffGraphBuilder

class NewTagNodePairTraversal(traversal: Traversal[NewTagNodePair]) extends HasStoreMethod {

  override def store()(implicit diffGraph: DiffGraphBuilder): Unit = {
    traversal.foreach { tagNodePair =>
      val tag      = tagNodePair.tag
      val tagValue = tagNodePair.node
      diffGraph.addNode(tag.asInstanceOf[NewNode])
      tagValue match {
        case tagValue: StoredNode =>
          diffGraph.addEdge(tagValue, tag.asInstanceOf[NewNode], EdgeTypes.TAGGED_BY)
        case tagValue: NewNode =>
          diffGraph.addEdge(tagValue, tag.asInstanceOf[NewNode], EdgeTypes.TAGGED_BY, Nil)
      }
    }

  }

}
