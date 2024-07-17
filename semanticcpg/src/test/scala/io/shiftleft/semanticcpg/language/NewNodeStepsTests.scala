package io.shiftleft.semanticcpg.language

import io.shiftleft.codepropertygraph.generated.{Cpg, DiffGraphBuilder}
import flatgraph.DiffGraphApplier.applyDiff
import io.shiftleft.codepropertygraph.generated.nodes.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class NewNodeStepsTest extends AnyWordSpec with Matchers {
  import io.shiftleft.semanticcpg.language.NewNodeNodeStepsTest.*

  "stores NewNodes" in {
    implicit val diffGraphBuilder: DiffGraphBuilder = Cpg.newDiffGraphBuilder
    val newNode                                     = newTestNode()
    val cpg                                         = Cpg.empty
    new NewNodeSteps(newNode.start).store()

    cpg.all.size shouldBe 0
    applyDiff(cpg.graph, diffGraphBuilder)
    cpg.all.size shouldBe 1
  }

  "can access the node label" in {
    val newNode = newTestNode()
    new NewNodeSteps(newNode.start).label.l shouldBe List(newNode.label)
  }

  "stores containedNodes and connecting edge" when {
    "embedding a StoredNode and a NewNode" in {
      val cpg         = Cpg.empty
      val newModifier = NewModifier()
      applyDiff(cpg.graph, Cpg.newDiffGraphBuilder.addNode(newModifier))
      val existingContainedNode = newModifier.storedRef.get
      cpg.graph.allNodes.toSet shouldBe Set(existingContainedNode)

      implicit val diffGraphBuilder: DiffGraphBuilder = Cpg.newDiffGraphBuilder
      val newContainedNode                            = newTestNode()
      val newNode = newTestNode(evidence = List(existingContainedNode, newContainedNode))
      new NewNodeSteps(newNode.start).store()
      cpg.all.length shouldBe 1
      applyDiff(cpg.graph, diffGraphBuilder)
      cpg.all.length shouldBe 3
    }

    "embedding a NewNode recursively" in {
      implicit val diffGraphBuilder: DiffGraphBuilder = Cpg.newDiffGraphBuilder
      val cpg                                         = Cpg.empty
      val newContainedNodeL1                          = newTestNode()
      val newContainedNodeL0                          = newTestNode(evidence = List(newContainedNodeL1))
      val newNode                                     = newTestNode(evidence = List(newContainedNodeL0))
      new NewNodeSteps(newNode.start).store()
      cpg.all.size shouldBe 0
      applyDiff(cpg.graph, diffGraphBuilder)
      cpg.all.size shouldBe 3
    }

  }
}

object NewNodeNodeStepsTest {
  def newTestNode(evidence: Seq[AbstractNode] = Seq.empty): NewFinding =
    NewFinding().evidence(evidence)
}
