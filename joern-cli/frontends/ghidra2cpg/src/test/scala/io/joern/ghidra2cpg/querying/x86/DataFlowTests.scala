package io.joern.ghidra2cpg.querying.x86

import io.joern.ghidra2cpg.fixtures.GhidraBinToCpgSuite
import io.shiftleft.codepropertygraph.generated.Cpg
import io.joern.dataflowengineoss.language.*
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.x2cpg.layers.{Base, CallGraph, ControlFlow, TypeRelations}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.layers.*

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
    val semantics: Semantics            = DefaultSemantics()
    implicit val context: EngineContext = EngineContext(semantics)

    def source = cpg.method.name("dataflow").call.code("MOV EDX,EAX").argument.code("EAX")
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
      Set(List("MOV EDX,EAX", "MOV ECX,EDX", "MOV EAX,ECX"))
  }
}
