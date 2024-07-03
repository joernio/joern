package io.joern.x2cpg.passes

import io.shiftleft.OverflowDbTestInstance
import io.joern.x2cpg.passes.controlflow.cfgdominator.{CfgAdapter, CfgDominator, CfgDominatorFrontier, DomTreeAdapter}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import overflowdb.*

import scala.jdk.CollectionConverters.*

class CfgDominatorFrontierTests extends AnyWordSpec with Matchers {

  private class TestCfgAdapter extends CfgAdapter[Node] {
    override def successors(node: Node): IterableOnce[Node] =
      node.out("CFG").asScala

    override def predecessors(node: Node): IterableOnce[Node] =
      node.in("CFG").asScala
  }

  private class TestDomTreeAdapter(immediateDominators: scala.collection.Map[Node, Node]) extends DomTreeAdapter[Node] {
    override def immediateDominator(cfgNode: Node): Option[Node] = {
      immediateDominators.get(cfgNode)
    }
  }

  "Cfg dominance frontier test" in {
    val graph = OverflowDbTestInstance.create

    val v0 = graph + "UNKNOWN"
    val v1 = graph + "UNKNOWN"
    val v2 = graph + "UNKNOWN"
    val v3 = graph + "UNKNOWN"
    val v4 = graph + "UNKNOWN"
    val v5 = graph + "UNKNOWN"
    val v6 = graph + "UNKNOWN"

    v0 --- "CFG" --> v1
    v1 --- "CFG" --> v2
    v2 --- "CFG" --> v3
    v2 --- "CFG" --> v5
    v3 --- "CFG" --> v4
    v4 --- "CFG" --> v2
    v4 --- "CFG" --> v5
    v5 --- "CFG" --> v6

    val cfgAdapter             = new TestCfgAdapter
    val cfgDominatorCalculator = new CfgDominator(cfgAdapter)
    val immediateDominators    = cfgDominatorCalculator.calculate(v0)

    val domTreeAdapter       = new TestDomTreeAdapter(immediateDominators)
    val cfgDominatorFrontier = new CfgDominatorFrontier(cfgAdapter, domTreeAdapter)
    val dominanceFrontier    = cfgDominatorFrontier.calculate(graph.nodes.asScala.toList)

    dominanceFrontier.get(v0) shouldBe None
    dominanceFrontier.get(v1) shouldBe None
    dominanceFrontier.apply(v2) shouldBe Set(v2)
    dominanceFrontier.apply(v3) shouldBe Set(v2, v5)
    dominanceFrontier.apply(v4) shouldBe Set(v2, v5)
    dominanceFrontier.get(v5) shouldBe None
    dominanceFrontier.get(v6) shouldBe None
  }

  "Cfg domiance frontier with dead code test" in {
    val graph = OverflowDbTestInstance.create

    val v0 = graph + "UNKNOWN"
    val v1 = graph + "UNKNOWN" // This node simulates dead code as it is not reachable from the entry v0.
    val v2 = graph + "UNKNOWN"

    v0 --- "CFG" --> v2
    v1 --- "CFG" --> v2

    val cfgAdapter             = new TestCfgAdapter
    val cfgDominatorCalculator = new CfgDominator(cfgAdapter)
    val immediateDominators    = cfgDominatorCalculator.calculate(v0)

    val domTreeAdapter       = new TestDomTreeAdapter(immediateDominators)
    val cfgDominatorFrontier = new CfgDominatorFrontier(cfgAdapter, domTreeAdapter)
    val dominanceFrontier    = cfgDominatorFrontier.calculate(graph.nodes.asScala.toList)

    dominanceFrontier.get(v0) shouldBe None
    dominanceFrontier.apply(v1) shouldBe Set(v2)
    dominanceFrontier.get(v2) shouldBe None
  }

}
