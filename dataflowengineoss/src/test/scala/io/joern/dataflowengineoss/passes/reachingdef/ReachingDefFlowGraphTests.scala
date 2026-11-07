package io.joern.dataflowengineoss.passes.reachingdef

import flatgraph.DNodeOrNode
import io.shiftleft.codepropertygraph.generated.nodes.NewIdentifier
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.testing.MockCpg
import org.scalatest.concurrent.TimeLimits
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Seconds, Span}
import org.scalatest.wordspec.AnyWordSpec

class ReachingDefFlowGraphTests extends AnyWordSpec with Matchers with TimeLimits {

  private val nodeCount = 100000

  /** `long` is one line of `nodeCount` identifiers from its entry to its return, all of them reachable. */
  private def cpgWithLongMethod(): Cpg =
    MockCpg()
      .withMetaData()
      .withMethod("long")
      .withCustom { (diffGraph, cpg) =>
        val method = cpg.method.nameExact("long").head
        method.parameter.asOutput.foreach(parameterOut => diffGraph.addEdge(method, parameterOut, EdgeTypes.AST))
        var previous: DNodeOrNode = method
        (0 until nodeCount).foreach { i =>
          val identifier = NewIdentifier().name(s"x$i").code(s"x$i")
          diffGraph.addNode(identifier)
          diffGraph.addEdge(method, identifier, EdgeTypes.CONTAINS)
          diffGraph.addEdge(previous, identifier, EdgeTypes.CFG)
          previous = identifier
        }
        diffGraph.addEdge(previous, method.methodReturn, EdgeTypes.CFG)
      }
      .cpg

  "ReachingDefFlowGraph of a method with many CFG nodes" should {
    val method = cpgWithLongMethod().method.nameExact("long").head

    "be built in time that grows with the nodes, not with their square" in {
      val flowGraph = failAfter(Span(5, Seconds)) {
        new ReachingDefFlowGraph(method)
      }
      flowGraph.nodeToNumber.size shouldBe method.cfgNode.size + 4
    }
  }
}
