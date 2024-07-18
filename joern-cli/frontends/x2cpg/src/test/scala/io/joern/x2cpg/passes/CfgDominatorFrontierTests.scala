package io.joern.x2cpg.passes

import flatgraph.misc.TestUtils.*
import io.joern.x2cpg.passes.controlflow.cfgdominator.{CfgAdapter, CfgDominator, CfgDominatorFrontier, DomTreeAdapter}
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{NewUnknown, StoredNode}
import io.shiftleft.semanticcpg.language.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scala.jdk.CollectionConverters.*

class CfgDominatorFrontierTests extends AnyWordSpec with Matchers {

  private class TestCfgAdapter extends CfgAdapter[StoredNode] {
    override def successors(node: StoredNode): Iterator[StoredNode] =
      node.out("CFG").cast[StoredNode]

    override def predecessors(node: StoredNode): Iterator[StoredNode] =
      node.in("CFG").cast[StoredNode]
  }

  private class TestDomTreeAdapter(immediateDominators: scala.collection.Map[StoredNode, StoredNode])
      extends DomTreeAdapter[StoredNode] {
    override def immediateDominator(cfgNode: StoredNode): Option[StoredNode] = {
      immediateDominators.get(cfgNode)
    }
  }

  "Cfg dominance frontier test" in {
    val cpg   = Cpg.empty
    val graph = cpg.graph

    val v0 = graph.addNode(NewUnknown())
    val v1 = graph.addNode(NewUnknown())
    val v2 = graph.addNode(NewUnknown())
    val v3 = graph.addNode(NewUnknown())
    val v4 = graph.addNode(NewUnknown())
    val v5 = graph.addNode(NewUnknown())
    val v6 = graph.addNode(NewUnknown())

    // TODO MP get arrow syntax back
//    v0 --- "CFG" --> v1
//    v1 --- "CFG" --> v2
//    v2 --- "CFG" --> v3
//    v2 --- "CFG" --> v5
//    v3 --- "CFG" --> v4
//    v4 --- "CFG" --> v2
//    v4 --- "CFG" --> v5
//    v5 --- "CFG" --> v6
    graph.applyDiff { diffGraphBuilder =>
      diffGraphBuilder.addEdge(v0, v1, EdgeTypes.CFG)
      diffGraphBuilder.addEdge(v1, v2, EdgeTypes.CFG)
      diffGraphBuilder.addEdge(v2, v3, EdgeTypes.CFG)
      diffGraphBuilder.addEdge(v2, v5, EdgeTypes.CFG)
      diffGraphBuilder.addEdge(v3, v4, EdgeTypes.CFG)
      diffGraphBuilder.addEdge(v4, v2, EdgeTypes.CFG)
      diffGraphBuilder.addEdge(v4, v5, EdgeTypes.CFG)
      diffGraphBuilder.addEdge(v5, v6, EdgeTypes.CFG)
    }

    val cfgAdapter             = new TestCfgAdapter
    val cfgDominatorCalculator = new CfgDominator(cfgAdapter)
    val immediateDominators    = cfgDominatorCalculator.calculate(v0)

    val domTreeAdapter       = new TestDomTreeAdapter(immediateDominators)
    val cfgDominatorFrontier = new CfgDominatorFrontier(cfgAdapter, domTreeAdapter)
    val dominanceFrontier    = cfgDominatorFrontier.calculate(cpg.all)

    dominanceFrontier.get(v0) shouldBe None
    dominanceFrontier.get(v1) shouldBe None
    dominanceFrontier.apply(v2) shouldBe Set(v2)
    dominanceFrontier.apply(v3) shouldBe Set(v2, v5)
    dominanceFrontier.apply(v4) shouldBe Set(v2, v5)
    dominanceFrontier.get(v5) shouldBe None
    dominanceFrontier.get(v6) shouldBe None
  }

  "Cfg domiance frontier with dead code test" in {
    val cpg   = Cpg.empty
    val graph = cpg.graph

    val v0 = graph.addNode(NewUnknown())
    val v1 = graph.addNode(NewUnknown()) // This node simulates dead code as it is not reachable from the entry v0.
    val v2 = graph.addNode(NewUnknown())

    // TODO MP get arrow syntax back
//    v0 --- "CFG" --> v2
//    v1 --- "CFG" --> v2
    graph.applyDiff { diffGraphBuilder =>
      diffGraphBuilder.addEdge(v0, v2, EdgeTypes.CFG)
      diffGraphBuilder.addEdge(v1, v2, EdgeTypes.CFG)
    }

    val cfgAdapter             = new TestCfgAdapter
    val cfgDominatorCalculator = new CfgDominator(cfgAdapter)
    val immediateDominators    = cfgDominatorCalculator.calculate(v0)

    val domTreeAdapter       = new TestDomTreeAdapter(immediateDominators)
    val cfgDominatorFrontier = new CfgDominatorFrontier(cfgAdapter, domTreeAdapter)
    val dominanceFrontier    = cfgDominatorFrontier.calculate(cpg.all)

    dominanceFrontier.get(v0) shouldBe None
    dominanceFrontier.apply(v1) shouldBe Set(v2)
    dominanceFrontier.get(v2) shouldBe None
  }

}
