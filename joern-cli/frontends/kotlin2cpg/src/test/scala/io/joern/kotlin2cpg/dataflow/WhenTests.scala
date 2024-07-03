package io.joern.kotlin2cpg.dataflow

import io.joern.dataflowengineoss.language.toExtendedCfgNode
import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class WhenTests extends KotlinCode2CpgFixture(withOssDataflow = true) {
  implicit val resolver: ICallResolver = NoResolve

  "CPG for code with a `when` control structure with two branches and an else" should {
    val cpg = code("""package mypkg
      |fun f1(p: String) {
      |    when (p) {
      |        "first" -> println("1st$p")
      |        "second" -> println("2nd$p")
      |        else -> {
      |            println("3rd$p")
      |        }
      |    }
      |}
      |""".stripMargin)

    "find a flow through the first branch of the `when`" in {
      val source = cpg.method.name("f1").parameter
      val sink   = cpg.method.fullName(".*println.*").callIn.code(".*1st.*").argument
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(List(("f1(p)", Some(2)), ("p", Some(4)), ("\"1st$p\"", Some(4))))
    }

    "find a flow through the second branch of the `when`" in {
      val source = cpg.method.name("f1").parameter
      val sink   = cpg.method.fullName(".*println.*").callIn.code(".*2nd.*").argument
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(List(("f1(p)", Some(2)), ("p", Some(4)), ("p", Some(5)), ("\"2nd$p\"", Some(5))))
    }

    "find a flow through the else of the `when`" in {
      val source = cpg.method.name("f1").parameter
      val sink   = cpg.method.fullName(".*println.*").callIn.code(".*3rd.*").argument
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(List(("f1(p)", Some(2)), ("p", Some(4)), ("p", Some(5)), ("p", Some(7)), ("\"3rd$p\"", Some(7))))
    }
  }
}
