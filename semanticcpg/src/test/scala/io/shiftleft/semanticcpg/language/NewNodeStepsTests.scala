package io.shiftleft.semanticcpg.language

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{ModifierTypes, PropertyNames}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import overflowdb.BatchedUpdate.DiffGraphBuilder
import overflowdb.BatchedUpdate.applyDiff
import overflowdb.traversal.jIteratortoTraversal

import scala.jdk.CollectionConverters._

class NewNodeStepsTest extends AnyWordSpec with Matchers {
  import NewNodeNodeStepsTest._

  "stores NewNodes" in {
    implicit val diffGraphBuilder: DiffGraphBuilder = new DiffGraphBuilder
    val newNode                   = newTestNode()
    new NewNodeSteps(newNode.start).persist()
    val cpg                       = Cpg.emptyCpg

    cpg.graph.nodes.toList.size shouldBe 0
    applyDiff(cpg.graph, diffGraphBuilder)
    cpg.graph.nodes.toList.size shouldBe 1
  }

  "can access the node label" in {
    val newNode = newTestNode()
    new NewNodeSteps(newNode.start).label.l shouldBe List(newNode.label)
  }

  "stores containedNodes and connecting edge" when {

    "embedding a StoredNode and a NewNode" in {
      implicit val diffGraphBuilder: DiffGraphBuilder = new DiffGraphBuilder

      val cpg                       = Cpg.emptyCpg
      val existingContainedNode     = cpg.graph.addNode(42L, "MODIFIER").asInstanceOf[StoredNode]
      existingContainedNode.setProperty(PropertyNames.MODIFIER_TYPE, ModifierTypes.NATIVE)
      cpg.graph.V().asScala.toSet shouldBe Set(existingContainedNode)

      val newContainedNode = newTestNode()
      val newNode          = newTestNode(evidence = List(existingContainedNode, newContainedNode))
      new NewNodeSteps(newNode.start).persist()
      cpg.graph.V().asScala.length shouldBe 1

      applyDiff(cpg.graph, diffGraphBuilder)
      cpg.graph.V().asScala.length shouldBe 3
    }

    "embedding a NewNode recursively" in {
      implicit val diffGraphBuilder: DiffGraphBuilder = new DiffGraphBuilder

      val cpg                       = Cpg.emptyCpg
      val newContainedNodeL1        = newTestNode()
      val newContainedNodeL0        = newTestNode(evidence = List(newContainedNodeL1))
      val newNode                   = newTestNode(evidence = List(newContainedNodeL0))

      new NewNodeSteps(newNode.start).persist()
      cpg.graph.V().asScala.size shouldBe 0
      applyDiff(cpg.graph, diffGraphBuilder)
      cpg.graph.V().asScala.size shouldBe 3
    }

  }
}

object NewNodeNodeStepsTest {
  def newTestNode(evidence: Seq[AbstractNode] = Seq.empty): NewFinding =
    NewFinding().evidence(evidence)
}
