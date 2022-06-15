package io.joern.kotlin2cpg.dataflow

import io.joern.dataflowengineoss.language.toExtendedCfgNode
import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language._

class SimpleDataFlowTests extends KotlinCode2CpgFixture(withOssDataflow = true) {
  "CPG for code with simple function" should {
    lazy val cpg = code("""
        |package mypkg
        |fun doSomething(x: Int): Int {  return x + 1 }
        |""".stripMargin)

    "should find a flow from method parameter to method return" in {
      def source = cpg.method.name("doSomething").parameter
      def sink   = cpg.method.name("doSomething").methodReturn
      sink.reachableByFlows(source).size shouldBe 1
    }
  }
}
