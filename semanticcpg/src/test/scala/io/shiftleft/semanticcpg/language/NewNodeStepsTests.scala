package io.shiftleft.semanticcpg.language

import io.joern.odb2.DiffGraphApplier.applyDiff
import io.joern.odb2.DiffGraphBuilder
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.v2.nodes.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.jdk.CollectionConverters.*

class NewNodeStepsTest extends AnyWordSpec with Matchers {
  import io.shiftleft.semanticcpg.language.NewNodeNodeStepsTest.*

  "stores NewNodes" in {
    implicit val diffGraphBuilder: DiffGraphBuilder = new DiffGraphBuilder
    val newNode                                     = newTestNode()
    val cpg                                         = Cpg.emptyCpg
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
      val cpg                                         = Cpg.emptyCpg
      val newModifier = NewModifier()
      applyDiff(cpg.graph, DiffGraphBuilder().addNode(newModifier))
      val existingContainedNode = newModifier.storedRef.get
      cpg.graph.allNodes.toSet shouldBe Set(existingContainedNode)

      implicit val diffGraphBuilder: DiffGraphBuilder = new DiffGraphBuilder
      val newContainedNode = newTestNode()
      val newNode          = newTestNode(evidence = List(existingContainedNode, newContainedNode))
      new NewNodeSteps(newNode.start).store()
      cpg.all.length shouldBe 1
      applyDiff(cpg.graph, diffGraphBuilder)
      cpg.all.length shouldBe 3
    }

    "embedding a NewNode recursively" in {
      implicit val diffGraphBuilder: DiffGraphBuilder = new DiffGraphBuilder
      val cpg                                         = Cpg.emptyCpg
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
