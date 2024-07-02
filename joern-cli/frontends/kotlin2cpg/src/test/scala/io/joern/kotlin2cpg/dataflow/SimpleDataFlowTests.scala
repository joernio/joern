package io.joern.kotlin2cpg.dataflow

import io.joern.dataflowengineoss.language.toExtendedCfgNode
import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class SimpleDataFlowTests extends KotlinCode2CpgFixture(withOssDataflow = true) {
  implicit val resolver: ICallResolver = NoResolve

  "CPG for code with simple function" should {
    val cpg = code("""fun f1(p: Int): Int { return p + 1 }""".stripMargin)

    "should find a flow from method parameter to method return" in {
      val source = cpg.method.name("f1").parameter
      val sink   = cpg.method.name("f1").ast.isReturn
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(List(("f1(p)", Some(1)), ("p + 1", Some(1)), ("return p + 1", Some(1))))
    }
  }

  "CPG for code with parenthesized call" should {
    val cpg = code("""
        |fun f1(p: String) {
        |  val x = ("prefix_" + p).toLowerCase()
        |  println(x)
        |}
        |""".stripMargin)

    "should find a flow from method parameter to the call's argument" in {
      val source = cpg.method.name("f1").parameter
      val sink   = cpg.method.name("println").callIn.argument
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("f1(p)", Some(2)),
            ("\"prefix_\" + p", Some(3)),
            ("(\"prefix_\" + p).toLowerCase()", Some(3)),
            ("val x = (\"prefix_\" + p).toLowerCase()", Some(3)),
            ("println(x)", Some(4))
          )
        )
    }
  }
}
