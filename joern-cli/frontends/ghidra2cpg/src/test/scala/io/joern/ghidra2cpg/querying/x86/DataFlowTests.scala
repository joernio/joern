package io.joern.ghidra2cpg.querying.x86

import io.joern.ghidra2cpg.fixtures.GhidraBinToCpgSuite
import io.shiftleft.codepropertygraph.Cpg
import io.joern.dataflowengineoss.language._
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.dataflowengineoss.semanticsloader.{Parser, Semantics}
import io.joern.x2cpg.layers.{Base, CallGraph, ControlFlow, TypeRelations}
import io.shiftleft.semanticcpg.language.{ICallResolver, _}
import io.shiftleft.semanticcpg.layers._
import io.shiftleft.utils.ProjectRoot

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
    buildCpgForBin("linux/x86/64/x86_64.bin")
  }

  "The data flow should contain " in {
    implicit val resolver: ICallResolver = NoResolve
    val semanticsFilename                = ProjectRoot.relativise("joern-cli/src/main/resources/default.semantics")
    val semantics: Semantics             = Semantics.fromList(new Parser().parseFile(semanticsFilename))
    implicit var context: EngineContext  = EngineContext(semantics)

    def source = cpg.method.name("dataflow").call.argument.code("1")
    def sink =
      cpg.method
        .name("dataflow")
        .call
        .where(_.argument.order(2).code("ECX"))
        .argument
        .order(1)
        .code("EAX")
    val flows = sink.reachableByFlows(source).l

    flows.map(flowToResultPairs).toSet shouldBe
      Set(List("ADD EAX,0x1", "MOV EDX,EAX", "MOV ECX,EDX", "MOV EAX,ECX"))
  }
}
