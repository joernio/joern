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

  "CPG for code with simple arithmetic operators" should {
    val cpg = code("""
        |package mypkg
        |
        |fun doSomething(x: Int): Int {
        |  val add41 = x + 41
        |  val subtract41 = x - 41
        |  val multiplyBy41 = x * 41
        |  val divideBy41 = x / 41
        |  return 41
        |}
        |""".stripMargin)

    def source = cpg.method.name("doSomething").parameter

    "should find a flow through addition" in {
      val sink = cpg.identifier.name("add41")
      sink.reachableByFlows(source).toSeq should not be Seq()
    }

    "should find a flow through subtraction" in {
      val sink = cpg.identifier.name("subtract41")
      sink.reachableByFlows(source).toSeq should not be Seq()
    }

    "should find a flow through multiplication" in {
      val sink = cpg.identifier.name("multiplyBy41")
      sink.reachableByFlows(source).toSeq should not be Seq()
    }

    "should find a flow through division" in {
      val sink = cpg.identifier.name("divideBy41")
      sink.reachableByFlows(source).toSeq should not be Seq()
    }
  }

  "CPG for code with simple assignment operators" should {
    val cpg = code("""
        |package mypkg
        |
        |fun doSomething(x: Int): Int {
        |  val modifyBy = 41
        |
        |  val x = 1
        |  x += modifyBy
        |  val addEq = x
        |
        |  val y = 1
        |  y -= modifyBy
        |  val subtractEq = y
        |
        |  val z = 1
        |  z *= modifyBy
        |  val multiplyEq = z
        |
        |  val p = 1
        |  p /= modifyBy
        |  val divideEq = p
        |
        |  return 41
        |}
        |""".stripMargin)

    def source = cpg.literal.code("41")

    "should find a flow through assign-addition" in {
      val sink = cpg.identifier.name("addEq")
      sink.reachableByFlows(source).toSeq should not be Seq()
    }

    "should find a flow through assign-subtraction" in {
      val sink = cpg.identifier.name("subtractEq")
      sink.reachableByFlows(source).toSeq should not be Seq()
    }

    "should find a flow through assign-multiplication" in {
      val sink = cpg.identifier.name("multiplyEq")
      sink.reachableByFlows(source).toSeq should not be Seq()
    }

    "should find a flow through assign-division" in {
      val sink = cpg.identifier.name("divideEq")
      sink.reachableByFlows(source).toSeq should not be Seq()
    }
  }
}
