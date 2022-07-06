package io.shiftleft.semanticcpg.language

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{ModifierTypes, PropertyNames}
import io.shiftleft.passes.DiffGraph
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.jdk.CollectionConverters._

class NewNodeStepsTest extends AnyWordSpec with Matchers {
  import NewNodeNodeStepsTest._

  "stores NewNodes" in {
    implicit val diffGraphBuilder = DiffGraph.newBuilder
    val newNode                   = newTestNode()
    new NewNodeSteps(newNode.start).store()
    val diffGraph = diffGraphBuilder.build()
    diffGraph.nodes.toList shouldBe List(newNode)
  }

  "can access the node label" in {
    val newNode = newTestNode()
    new NewNodeSteps(newNode.start).label.l shouldBe List(newNode.label)
  }

  "stores containedNodes and connecting edge" when {

    "embedding a StoredNode and a NewNode" in {
      implicit val diffGraphBuilder = DiffGraph.newBuilder
      val cpg                       = Cpg.emptyCpg
      val existingContainedNode     = cpg.graph.addNode(42L, "MODIFIER").asInstanceOf[StoredNode]
      existingContainedNode.setProperty(PropertyNames.MODIFIER_TYPE, ModifierTypes.NATIVE)
      cpg.graph.V().asScala.toSet shouldBe Set(existingContainedNode)

      val newContainedNode = newTestNode()
      val newNode          = newTestNode(evidence = List(existingContainedNode, newContainedNode))
      new NewNodeSteps(newNode.start).store()
      val diffGraph = diffGraphBuilder.build()
      diffGraph.nodes.toSet shouldBe Set(newNode)
      diffGraph.edges shouldBe Nil

      DiffGraph.Applier.applyDiff(diffGraph, cpg, undoable = false, keyPool = None)
      cpg.graph.V().asScala.length shouldBe 3
    }

    "embedding a NewNode recursively" in {
      implicit val diffGraphBuilder = DiffGraph.newBuilder
      val cpg                       = Cpg.emptyCpg
      val newContainedNodeL1        = newTestNode()
      val newContainedNodeL0        = newTestNode(evidence = List(newContainedNodeL1))
      val newNode                   = newTestNode(evidence = List(newContainedNodeL0))
      new NewNodeSteps(newNode.start).store()
      val diffGraph = diffGraphBuilder.build()

      diffGraph.nodes.toSet shouldBe Set(newNode)
      diffGraph.edges.toSet shouldBe Set.empty
      DiffGraph.Applier.applyDiff(diffGraph, cpg, undoable = false, keyPool = None)
      cpg.graph.V().asScala.size shouldBe 3
    }

  }
}

object NewNodeNodeStepsTest {
  def newTestNode(evidence: Seq[AbstractNode] = Seq.empty): NewFinding =
    NewFinding().evidence(evidence)
}
