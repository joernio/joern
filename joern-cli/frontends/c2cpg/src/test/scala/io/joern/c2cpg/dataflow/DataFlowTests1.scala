package io.joern.c2cpg.dataflow

import io.joern.c2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.joern.dataflowengineoss.language._
import io.joern.dataflowengineoss.queryengine.{EngineConfig, EngineContext}
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{Identifier, Literal}
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.toNodeTraversal

class DataFlowTests1 extends DataFlowCodeToCpgSuite {

  "DataFlowTest1" should {
    val cpg = code("""
        | int flow(int p0) {
        |    int a = p0;
        |    return a;
        | }""".stripMargin)

    "find flows from identifiers to return values of `flow`" in {
      def source = cpg.identifier
      def sink   = cpg.method.name("flow").methodReturn
//      println(cpg.graph) //
//      println(cpg.graph.edgeCount) //
//      println(source.size) //
//      println(sink.size) //
      def flows = sink.reachableByFlows(source)
//      println(flows.size)
//      flows.p.foreach(println)
//    def p(implicit show: Show[A] = Show.default): List[String] =
//      traversal.toList.map(show.apply)


      val show = implicitly[Show[Path]]
//      flows.foreach(flow => println(show(flow)))
      flows.toList
//      val resultPairs = flows.l.map(flowToResultPairs).distinct
//      resultPairs.size shouldBe 8
    }
  }

}
