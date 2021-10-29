package io.joern.ghidra2cpg.querying.mips

import io.joern.ghidra2cpg.fixtures.GhidraBinToCpgSuite
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.dataflowengineoss.language._
import io.shiftleft.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.shiftleft.dataflowengineoss.queryengine.EngineContext
import io.shiftleft.dataflowengineoss.semanticsloader.{Parser, Semantics}
import io.shiftleft.semanticcpg.language.{ICallResolver, _}
import io.shiftleft.semanticcpg.layers._

class DataFlowTests extends GhidraBinToCpgSuite {

  override def passes(cpg: Cpg): Unit = {
    val context = new LayerCreatorContext(cpg)
    new Base().run(context)
    new TypeRelations().run(context)
    new ControlFlow().run(context)
    new CallGraph().run(context)

    val options = new OssDataFlowOptions()
    new OssDataFlow(options).run(context)
  }

  override def beforeAll(): Unit = {
    super.beforeAll()
    buildCpgForBin("t1_to_t9")
  }

  "The data flow should contain " in {
    implicit val resolver: ICallResolver = NoResolve
    val customSemantics =
      """
        |"<operator>.addition" 1->-1 2->-1
        |"<operator>.assignment" 2->1
        |"<operator>.incBy" 1->1 2->1 3->1 4->1
        |""".stripMargin
    val semantics: Semantics = Semantics.fromList(new Parser().parse(customSemantics))
    implicit val context: EngineContext = EngineContext(semantics)

    def source = cpg.call.code("li t1,0x2a").argument(1)
    def sink = cpg.call.code("or t9,t6,zero").take(1).argument(1)
    val flows = sink.reachableByFlows(source).l

    flows.map(flowToResultPairs).toSet shouldBe
      Set(
        List("li t1,0x2a",
             "add t2,t0,t1",
             "addu t3,t2,t0",
             "addu t4,t3,t0",
             "addi t5,t4,0x1",
             "addiu t6,t5,0x1",
             "or t9,t6,zero"))
  }
}
