package io.joern.ghidra2cpg.querying.mips

import io.joern.ghidra2cpg.fixtures.GhidraBinToCpgSuite
import io.shiftleft.codepropertygraph.Cpg
import io.joern.dataflowengineoss.language._
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.dataflowengineoss.semanticsloader.{Parser, Semantics}
import io.shiftleft.semanticcpg.language.{ICallResolver, _}
import io.shiftleft.semanticcpg.layers._
import io.shiftleft.utils.ProjectRoot

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
  }

  implicit val resolver: ICallResolver = NoResolve
  val semanticsFilename = ProjectRoot.relativise("dataflowengineoss/src/test/resources/default.semantics")
  val semList = new Parser().parseFile(semanticsFilename)
  val semantics: Semantics = Semantics.fromList(semList)
  implicit var context: EngineContext = EngineContext(semantics)

  "should find a flow from `sw v0,0x18(s8)` to `lw v0,0x18(s8)` in getenv_to_strcmp" in {
    buildCpgForBin("getenv_to_strcmp.mips")
    def source = cpg.call.codeExact("sw v0,0x18(s8)").argument(2)
    def sink = cpg.call.codeExact("lw v0,0x18(s8)").argument(2)
    val flows = sink.reachableByFlows(source).l
    flows.map(flowToResultPairs).toSet shouldBe
      Set(List("sw v0,0x18(s8)", "lw v0,0x18(s8)"))
  }
}
