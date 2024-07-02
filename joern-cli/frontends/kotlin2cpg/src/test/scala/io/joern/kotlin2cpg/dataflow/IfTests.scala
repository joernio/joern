package io.joern.kotlin2cpg.dataflow

import io.joern.dataflowengineoss.language.toExtendedCfgNode
import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class IfTests extends KotlinCode2CpgFixture(withOssDataflow = true) {
  implicit val resolver: ICallResolver = NoResolve

  "CPG for code with a an `if` control structure" should {
    val cpg = code("""
      |fun f1(p: Int) {
      |  val tick1 = p
      |  if (2 % 3 == 0) {
      |    println(tick1 + 1)
      |  } else if (2 % 3 == 1) {
      |    println(tick1 + 2)
      |  } else  {
      |    println(tick1 + 3)
      |  }
      |}
      |""".stripMargin)

    "find a flow through all the branches of the control structure" in {
      val source = cpg.method.name("f1").parameter
      val sink   = cpg.method.name("println").callIn.argument
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(("f1(p)", Some(2)), ("val tick1 = p", Some(3)), ("tick1 + 1", Some(5))),
          List(("f1(p)", Some(2)), ("val tick1 = p", Some(3)), ("tick1 + 2", Some(7))),
          List(("f1(p)", Some(2)), ("val tick1 = p", Some(3)), ("tick1 + 3", Some(9)))
        )
    }
  }
}
