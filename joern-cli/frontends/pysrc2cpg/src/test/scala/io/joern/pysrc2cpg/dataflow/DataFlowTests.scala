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
    sink.reachableByFlows(source).size shouldBe 1
  }

  "inter procedural call 1" in {
    val cpg: Cpg = code("""
      |def foo():
      |    return 42
      |bar = foo()
      |print(bar)
      |""".stripMargin)
    def source = cpg.literal("42")
    def sink   = cpg.call("print")
    sink.reachableByFlows(source).size shouldBe 1
  }

  "inter procedural call 2" in {
    val cpg: Cpg = code("""
      |def foo(input):
      |    sink(input)
      |def main():
      |    source = 42
      |    foo(source)
      |""".stripMargin)
    def source = cpg.literal("42")
    def sink   = cpg.call("sink")
    sink.reachableByFlows(source).size shouldBe 1
  }

  "flow from class variable to sink" in {
    val cpg: Cpg = code("""
                          |class Foo():
                          |    x = 'sensitive'
                          |    def foo(self):
                          |        sink(self.x)
                          |""".stripMargin)
    val source = cpg.member(".*x.*").l
    val sink   = cpg.call(".*sink").argument(1).l
    source.size shouldBe 1
    sink.size shouldBe 1
    sink.reachableByFlows(source).size shouldBe 1
  }

  "flow from class variable to sink in assignment" in {
    val cpg: Cpg = code("""
        |class Foo():
        |    x = 'sensitive'
        |    def foo(self):
        |        a = sink(self.x)
        |""".stripMargin)
    val source = cpg.member(".*x.*").l
    val sink   = cpg.call(".*sink").argument(1).l
    source.size shouldBe 1
    sink.size shouldBe 1
    sink.reachableByFlows(source).size shouldBe 1
  }

}
