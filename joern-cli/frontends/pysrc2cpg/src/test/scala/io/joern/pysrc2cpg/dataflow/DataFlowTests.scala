package io.joern.pysrc2cpg.dataflow

import io.joern.dataflowengineoss.language.toExtendedCfgNode
import io.joern.pysrc2cpg.PySrc2CpgFixture
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.language._

class DataFlowTests extends PySrc2CpgFixture(withOssDataflow = true) {

  "intra-procedural" in {
    val cpg: Cpg = code("""
      |a = 42
      |c = foo(a, b)
      |print(c)
      |""".stripMargin)
    val source = cpg.literal("42")
    val sink   = cpg.call.code("print.*").argument
    sink.reachableByFlows(source).size shouldBe 1
  }

  "chained call" in {
    val cpg: Cpg = code("""
      |a = 42
      |c = foo(a).bar()
      |sink(c)
      |""".stripMargin)
    def source = cpg.literal("42")
    def sink   = cpg.call("sink").argument
    source.size shouldBe 1
    sink.size shouldBe 1
    sink.reachableByFlows(source).size shouldBe 1
  }
}
