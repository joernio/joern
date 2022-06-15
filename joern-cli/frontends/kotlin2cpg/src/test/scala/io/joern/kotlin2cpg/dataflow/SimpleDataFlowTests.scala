package io.joern.kotlin2cpg.dataflow

import io.joern.kotlin2cpg.TestContext
import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers

class SimpleDataFlowTests extends DataFlowTestSuite with Matchers {
  "CPG for code with simple function" - {
    lazy val cpg = TestContext.buildCpg("""
        |package mypkg
        |fun doSomething(x: Int): Int {  return x + 1 }
        |""".stripMargin)
    createOssDataflowLayer(cpg)

    "should find a flow from method parameter to method return" in {
      def source = cpg.method.name("doSomething").parameter
      def sink   = cpg.method.name("doSomething").methodReturn
      flowsFrom(source, sink).size shouldBe 1
    }
  }
}
