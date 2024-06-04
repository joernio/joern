package io.joern.ghidra2cpg.querying.mips

import io.joern.dataflowengineoss.language._
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.dataflowengineoss.semanticsloader.{Parser, Semantics}
import io.joern.ghidra2cpg.fixtures.GhidraBinToCpgSuite
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.layers._
class DataFlowThroughLoHiRegistersTests extends GhidraBinToCpgSuite {

  override def passes(cpg: Cpg): Unit = {
    applyDefaultOverlays(cpg)
    val context = new LayerCreatorContext(cpg)
    val options = new OssDataFlowOptions()
    new OssDataFlow(options).run(context)
  }

  override def beforeAll(): Unit = {
    super.beforeAll()
    buildCpgForBin("linux/mips/84_div_2")
  }

  implicit val resolver: ICallResolver = NoResolve
  val customSemantics: String =
    s""""<operator>.assignment" 2->1
       |"<operator>.assignmentArithmeticShiftRight" 3->1 2->1
       |"<operator>.assignmentAnd" 3->1 2->1
       |"<operator>.assignmentLogicalShiftRight" 3->1 2->1
       |"<operator>.assignmentDivision" 2->-1 1->-1
       |"<operator>.assignmentOr" 3->1 2->1
       |"<operator>.assignmentNor" 3->1 2->1
       |"<operator>.assignmentXor" 3->1 2->1
       |"<operator>.decBy" 3->1 2->1
       |"<operator>.incBy" 1->1 2->1 3->1 4->1
       |"<operator>.rotateRight" 2->1
       |""".stripMargin
  implicit val semantics: Semantics   = Semantics.fromList(new Parser().parse(customSemantics))
  implicit val context: EngineContext = EngineContext(semantics)

  "should find flows through `div*` instructions" in {
    def source                       = cpg.call.code("_div t1,t0")
    def sink                         = cpg.call.code("mflo t2")
    def flowsThroughDivXInstructions = sink.reachableByFlows(source).l
    flowsThroughDivXInstructions.map(flowToResultPairs).toSet shouldBe
      Set(List("_div t1,t0", "mflo t2"))
  }
}
