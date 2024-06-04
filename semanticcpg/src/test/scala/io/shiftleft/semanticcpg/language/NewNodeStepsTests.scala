package io.shiftleft.semanticcpg.language

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import overflowdb.BatchedUpdate.{DiffGraphBuilder, applyDiff}

import scala.jdk.CollectionConverters._

class NewNodeStepsTest extends AnyWordSpec with Matchers {
  import io.shiftleft.semanticcpg.language.NewNodeNodeStepsTest._

  "stores NewNodes" in {
    implicit val diffGraphBuilder: DiffGraphBuilder = new DiffGraphBuilder
    val newNode                                     = newTestNode()
    val cpg                                         = Cpg.empty
    new NewNodeSteps(newNode.start).store()

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
      val cpg                                         = Cpg.empty
      val existingContainedNode                       = cpg.graph.addNode(42L, "MODIFIER").asInstanceOf[StoredNode]
      cpg.graph.V().asScala.toSet shouldBe Set(existingContainedNode)

      val newContainedNode = newTestNode()
      val newNode          = newTestNode(evidence = List(existingContainedNode, newContainedNode))
      new NewNodeSteps(newNode.start).store()
      cpg.graph.V().asScala.length shouldBe 1
      applyDiff(cpg.graph, diffGraphBuilder)
      cpg.graph.V().asScala.length shouldBe 3
    }

    "embedding a NewNode recursively" in {
      implicit val diffGraphBuilder: DiffGraphBuilder = new DiffGraphBuilder
      val cpg                                         = Cpg.empty
      val newContainedNodeL1                          = newTestNode()
      val newContainedNodeL0                          = newTestNode(evidence = List(newContainedNodeL1))
      val newNode                                     = newTestNode(evidence = List(newContainedNodeL0))
      new NewNodeSteps(newNode.start).store()
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
