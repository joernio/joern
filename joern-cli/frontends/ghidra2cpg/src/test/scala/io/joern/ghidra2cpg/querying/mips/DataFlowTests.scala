package io.joern.ghidra2cpg.querying.mips

import io.joern.dataflowengineoss.language._
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.ghidra2cpg.fixtures.GhidraBinToCpgSuite
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.{ICallResolver, _}
import io.shiftleft.semanticcpg.layers._

class DataFlowTests extends GhidraBinToCpgSuite {

  implicit val resolver: ICallResolver = NoResolve
  implicit var context: EngineContext  = EngineContext()

  override def passes(cpg: Cpg): Unit = {
    applyDefaultOverlays(cpg)
    val context = new LayerCreatorContext(cpg)
    val options = new OssDataFlowOptions()
    new OssDataFlow(options).run(context)
  }

  override def beforeAll(): Unit = {
    super.beforeAll()
    buildCpgForBin("linux/mips/t1_to_t9")
  }

  "should find flows through `add*` instructions" in {
    def source = cpg.call.code("li t1,0x2a").argument(1)

    def sink =
      cpg.call
        .code("or t9,t6,zero")
        .where(_.cfgPrev.isCall.code("add.*"))
        .argument(1)

    val flowsThroughAddXInstructions = sink.reachableByFlows(source).l
    flowsThroughAddXInstructions.map(flowToResultPairs).toSet shouldBe
      Set(
        List(
          "li t1,0x2a",
          "add t2,t0,t1",
          "addu t3,t2,t0",
          "addu t4,t3,t0",
          "addi t5,t4,0x1",
          "addiu t6,t5,0x1",
          "or t9,t6,zero"
        )
      )
  }

  "should find flows through `and*` instructions" in {
    def source =
      cpg.call
        .code("or t1,t9,zero")
        .where(_.cfgNext.isCall.code("and.*"))
        .argument(1)

    def sink =
      cpg.call
        .code("or t9,t3,zero")
        .where(_.cfgPrev.isCall.code("and.*"))
        .argument(1)

    val flowsThroughAndXInstructions = sink.reachableByFlows(source).l
    flowsThroughAndXInstructions.map(flowToResultPairs).toSet shouldBe
      Set(List("or t1,t9,zero", "and t2,t1,t0", "andi t3,t2,0xffff", "or t9,t3,zero"))
  }

  "should find flows through `ori/nor` instructions" in {
    def source =
      cpg.call
        .code("or t1,t9,zero")
        .where(_.cfgNext.cfgNext.code("nor.*"))
        .argument(1)

    def sink =
      cpg.call
        .code("or t9,t4,zero")
        .where(_.cfgPrev.isCall.code("or.*"))
        .argument(1)

    val flowsThroughOrXInstructions = sink.reachableByFlows(source).l
    flowsThroughOrXInstructions.map(flowToResultPairs).toSet shouldBe
      Set(List("or t1,t9,zero", "nor t3,t1,t2", "ori t4,t3,0x30", "or t9,t4,zero"))
  }

  "should find flows through _shift_ instructions" in {
    def source =
      cpg.call
        .code("or t1,t9,zero")
        .where(_.cfgNext.isCall.code("sll.*"))
        .argument(1)

    def sink =
      cpg.call
        .code("or t9,t6,zero")
        .where(_.cfgPrev.isCall.code("srlv.*"))
        .argument(1)

    val flowsThroughShiftXInstructions = sink.reachableByFlows(source).l
    flowsThroughShiftXInstructions.map(flowToResultPairs).toSet shouldBe
      Set(
        List(
          "or t1,t9,zero",
          "sll t2,t1,0x1",
          "sllv t3,t2,t0",
          "sra t4,t3,0x1",
          "srav t5,t4,t0",
          "srl t6,t5,0x0",
          "srlv t7,t6,zero",
          "or t9,t6,zero"
        )
      )
  }

  "should find flows through `sub*` instructions" in {
    def source =
      cpg.call
        .code("or t1,t9,zero")
        .where(_.cfgNext.isCall.code("sub.*"))
        .argument(1)

    def sink =
      cpg.call
        .code("or t9,t3,zero")
        .where(_.cfgPrev.isCall.code("sub.*"))
        .argument(1)

    val flowsThroughSubXInstructions = sink.reachableByFlows(source).l
    flowsThroughSubXInstructions.map(flowToResultPairs).toSet shouldBe
      Set(List("or t1,t9,zero", "sub t2,t1,t0", "subu t3,t2,t0", "or t9,t3,zero"))
  }

  "should find flows through `xor*` instructions" in {
    def source =
      cpg.call
        .code("or t1,t9,zero")
        .where(_.cfgNext.isCall.code("xor.*"))
        .argument(1)

    def sink =
      cpg.call
        .code("or t9,t3,zero")
        .where(_.cfgPrev.isCall.code("xor.*"))
        .argument(1)

    val flowsThroughXorXInstructions = sink.reachableByFlows(source).l
    flowsThroughXorXInstructions.map(flowToResultPairs).toSet shouldBe
      Set(List("or t1,t9,zero", "xor t2,t1,zero", "xori t3,t2,0x4", "or t9,t3,zero"))
  }
}
