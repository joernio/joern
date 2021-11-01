package io.joern.ghidra2cpg.querying.mips

import io.joern.ghidra2cpg.fixtures.GhidraBinToCpgSuite
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.dataflowengineoss.language._
import io.shiftleft.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.shiftleft.dataflowengineoss.queryengine.EngineContext
import io.shiftleft.dataflowengineoss.semanticsloader.{Parser, Semantics}
import io.shiftleft.semanticcpg.language.{ICallResolver, _}
import io.shiftleft.semanticcpg.layers._

class DataFlowViaMemoryStoreTests extends GhidraBinToCpgSuite {

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
    buildCpgForBin("getenv_to_strcmp.mips")
  }

  implicit val resolver: ICallResolver = NoResolve
  val customSemantics =
    s""""<operator>.assignment" 2->1
       |"<operator>.assignmentArithmeticShiftRight" 3->1 2->1
       |"<operator>.assignmentAnd" 3->1 2->1
       |"<operator>.assignmentLogicalShiftRight" 3->1 2->1
       |"<operator>.assignmentOr" 3->1 2->1
       |"<operator>.assignmentNor" 3->1 2->1
       |"<operator>.assignmentXor" 3->1 2->1
       |"<operator>.decBy" 3->1 2->1
       |"<operator>.incBy" 1->1 2->1 3->1 4->1
       |"<operator>.rotateRight" 2->1
       |""".stripMargin
  val semantics: Semantics            = Semantics.fromList(new Parser().parse(customSemantics))
  implicit val context: EngineContext = EngineContext(semantics)

  "should find a flow from `getenv` to `strcmp`" in {
    def source = cpg.call.code("getenv")
    def sink = cpg.call.code("strcmp").argument
    val flows = sink.reachableByFlows(source).l
    flows.map(flowToResultPairs).toSet shouldBe
      Set(List("getenv", "lw a0,0x18(s8)", "strcmp"))
  }
}
