package io.joern.kotlin2cpg.dataflow

import io.joern.dataflowengineoss.language.toExtendedCfgNode
import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class WhileTests extends KotlinCode2CpgFixture(withOssDataflow = true) {
  implicit val resolver: ICallResolver = NoResolve

  "CPG for code with `while` control structure" should {
    val cpg = code("""
      |fun f1(p: Int) {
      |  var someVal = p
      |  while (true) {
      |    someVal += 1
      |  }
      |  println(someVal)
      |}
      |""".stripMargin)

    "should find a flow through body of the control structure" in {
      val source = cpg.method.name("f1").parameter
      val sink   = cpg.method.name("println").callIn.argument
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("f1(p)", Some(2)),
            ("var someVal = p", Some(3)),
            ("someVal += 1", Some(5)),
            ("println(someVal)", Some(7))
          )
        )
    }
  }

  "CPG for code with `do-while` control structure" should {
    val cpg = code("""
      |fun f1(p: Int) {
      |  var someVal = p
      |  do {
      |    someVal += 1
      |  } while(1)
      |  println(someVal)
      |}
      |""".stripMargin)

    "should find a flow through body of the control structure" in {
      val source = cpg.method.name("f1").parameter
      val sink   = cpg.method.name("println").callIn.argument
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("f1(p)", Some(2)),
            ("var someVal = p", Some(3)),
            ("someVal += 1", Some(5)),
            ("println(someVal)", Some(7))
          )
        )
    }
  }
}
