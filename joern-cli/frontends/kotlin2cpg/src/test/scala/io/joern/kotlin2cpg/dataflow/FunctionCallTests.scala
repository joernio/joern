package io.joern.kotlin2cpg.dataflow

import io.joern.dataflowengineoss.language.toExtendedCfgNode
import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class FunctionCallTests extends KotlinCode2CpgFixture(withOssDataflow = true) {
  implicit val resolver: ICallResolver = NoResolve

  "CPG for code with call to Java's `System.out.println`" should {
    val cpg = code("""fun f1(p: String) { System.out.println(p) }""".stripMargin)

    "should find a flow from method parameter to the call's argument" in {
      val source = cpg.method.name("f1").parameter
      val sink   = cpg.method.name("println").callIn.argument
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(List(("f1(p)", Some(1)), ("System.out.println(p)", Some(1))))
    }
  }

  "CPG for code with call to Java's `Runtime.getRuntime().exec`" should {
    val cpg = code("""
        |fun f1(p: String) { Runtime.getRuntime().exec(p) }
        |fun main() { f1("ls") }
        |""".stripMargin)

    "should find a flow from method parameter to the call's argument" in {
      val source = cpg.method.name("f1").parameter
      val sink   = cpg.method.name("exec").callIn.argument
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(("f1(p)", Some(2)), ("Runtime.getRuntime().exec(p)", Some(2))),
          List(("f1(p)", Some(2)), ("Runtime.getRuntime().exec(p)", Some(2)), ("Runtime.getRuntime()", Some(2)))
        )
    }
  }
}
