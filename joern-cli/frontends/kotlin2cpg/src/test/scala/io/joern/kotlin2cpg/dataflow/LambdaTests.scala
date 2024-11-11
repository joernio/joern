package io.joern.kotlin2cpg.dataflow

import io.joern.dataflowengineoss.language.toExtendedCfgNode
import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class LambdaTests extends KotlinCode2CpgFixture(withOssDataflow = true) {
  implicit val resolver: ICallResolver = NoResolve

  "CPG for code containing a lambda with parameter destructuring" should {
    val cpg = code("""
         |package mypkg
         |
         |fun f1(p: String) {
         |    val m = mapOf(p to 1, "two" to 2, "three" to 3)
         |    m.forEach { (k, v) ->  println(k) }
         |}
         |""".stripMargin)

    "find a flow we want" in {
      val source = cpg.method.name("f1").parameter
      val sink   = cpg.call.methodFullName(".*println.*").argument
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("f1(p)", Some(4)),
            ("tmp_1 = it", None),
            ("tmp_1.component1()", Some(6)),
            ("k = tmp_1.component1()", Some(6)),
            ("println(k)", Some(6))
          )
        )
    }
  }
}
