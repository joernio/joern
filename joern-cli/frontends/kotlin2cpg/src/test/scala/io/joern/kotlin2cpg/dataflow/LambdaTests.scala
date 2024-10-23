package io.joern.kotlin2cpg.dataflow

import io.joern.dataflowengineoss.language.toExtendedCfgNode
import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class LambdaTests extends KotlinCode2CpgFixture(withOssDataflow = true) {
  implicit val resolver: ICallResolver = NoResolve

  "CPG for code containing a lambda with parameter destructuring" should {
    val cpg = code("""|package mypkg
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
      // fixme: The nature of destructed parameters causes a loss in granularity here, what we see
      //  is the over-approximation of container `m` tainting called method-ref parameters
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(("f1(p)", Some(3)), ("println(k)", Some(5)))
//          List(
//            ("f1(p)", Some(3)),
//            ("p to 1", Some(4)),
//            ("mapOf(p to 1, \"two\" to 2, \"three\" to 3)", Some(4)),
//            ("val m = mapOf(p to 1, \"two\" to 2, \"three\" to 3)", Some(4)),
//            ("m.forEach { (k, v) ->  println(k) }", Some(5)),
//            ("<lambda>0(k, v)", Some(5)),
//            ("println(k)", Some(5))
//          )
        )
    }
  }
}
