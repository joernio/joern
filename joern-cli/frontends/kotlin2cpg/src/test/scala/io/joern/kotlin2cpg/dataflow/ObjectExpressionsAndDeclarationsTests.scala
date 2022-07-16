package io.joern.kotlin2cpg.dataflow

import io.joern.dataflowengineoss.language.toExtendedCfgNode
import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language._

class ObjectExpressionsAndDeclarationsTests extends KotlinCode2CpgFixture(withOssDataflow = true) {
  implicit val resolver: ICallResolver = NoResolve

  "CPG for code with object expression containing one member defined inline" should {
    val cpg = code("""
      |fun f1(p: String) {
      |    val anObject = object { var x: String = "INITIAL_VALUE" }
      |    anObject.x = p
      |    println(anObject.x)
      |}
      |""".stripMargin)

    "should find a flow through an assignment call of its member" in {
      val source = cpg.method.name("f1").parameter
      val sink   = cpg.method.name("println").callIn.argument
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(List(("f1(p)", Some(2)), ("anObject.x = p", Some(4)), ("println(anObject.x)", Some(5))))
    }
  }

  "CPG for code with object declaration containing one member defined inline" should {
    val cpg = code("""
      |object AnObject { var x: String = "INITIAL_VALUE" }
      |fun f1(p: String) {
      |    val anObject = AnObject
      |    anObject.x = p
      |    println(anObject.x)
      |}
      |""".stripMargin)

    "should find a flow through an assignment call of its member" in {
      val source = cpg.method.name("f1").parameter
      val sink   = cpg.method.name("println").callIn.argument
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(List(("f1(p)", Some(3)), ("anObject.x = p", Some(5)), ("println(anObject.x)", Some(6))))
    }
  }

}
