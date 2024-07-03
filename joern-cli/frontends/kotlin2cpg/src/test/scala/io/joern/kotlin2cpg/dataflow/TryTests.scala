package io.joern.kotlin2cpg.dataflow

import io.joern.dataflowengineoss.language.toExtendedCfgNode
import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class TryTests extends KotlinCode2CpgFixture(withOssDataflow = true) {
  implicit val resolver: ICallResolver = NoResolve

  "CPG for code with `try` control structures" should {
    val cpg = code("""
      |fun f1(p: Int) {
      |  var tick1 = p
      |  try {
      |    41 / 0
      |  } catch(e: Exception) {
      |    println(tick1 + 1)
      |  }
      |
      |  try {
      |    println(tick1 + 2)
      |  } catch(e: Exception) {
      |    println("NOTHING")
      |  }
      |}
      |""".stripMargin)

    "find a flow through all the branches of the control structure" in {
      val source = cpg.method.name("f1").parameter
      val sink   = cpg.method.name("println").callIn.argument
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(("f1(p)", Some(2)), ("var tick1 = p", Some(3)), ("tick1 + 1", Some(7))),
          List(("f1(p)", Some(2)), ("var tick1 = p", Some(3)), ("tick1 + 2", Some(11)))
        )
    }
  }
}
