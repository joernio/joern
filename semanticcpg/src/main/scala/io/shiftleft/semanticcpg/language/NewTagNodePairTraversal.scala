package io.shiftleft.semanticcpg.language

import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{NewNode, NewTagNodePair, StoredNode}
import io.shiftleft.passes.DiffGraph
import overflowdb.traversal._

class NewTagNodePairTraversal(traversal: Traversal[NewTagNodePair]) extends HasStoreMethod {

  override def store()(implicit diffGraph: DiffGraph.Builder): Unit = {
    traversal.foreach { tagNodePair =>
      val tag      = tagNodePair.tag
      val tagValue = tagNodePair.node
      diffGraph.addNode(tag.asInstanceOf[NewNode])
      tagValue match {
        case tagValue: StoredNode =>
          diffGraph.addEdgeFromOriginal(tagValue, tag.asInstanceOf[NewNode], EdgeTypes.TAGGED_BY)
        case tagValue: NewNode =>
          diffGraph.addEdge(tagValue, tag.asInstanceOf[NewNode], EdgeTypes.TAGGED_BY, Nil)
      }
    }

  }

}
