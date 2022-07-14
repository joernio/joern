package io.joern.kotlin2cpg.dataflow

import io.joern.dataflowengineoss.language.toExtendedCfgNode
import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language._

class TypeDeclTests extends KotlinCode2CpgFixture(withOssDataflow = true) {
  implicit val resolver: ICallResolver = NoResolve

  "CPG for code with user-defined class containing one member defined inline" should {
    val cpg = code("""
      |class AClass {
      |    var x: String = "INITIAL"
      |}
      |
      |fun f1(p: String) {
      |    val aClass = AClass()
      |    aClass.x = p
      |    println(aClass.x)
      |}
      |""".stripMargin)

    "should find a flow through an assignment call of its member" in {
      val source = cpg.method.name("f1").parameter
      val sink   = cpg.method.name("println").callIn.argument
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(List(("f1(p)", Some(6)), ("aClass.x = p", Some(8)), ("println(aClass.x)", Some(9))))
    }
  }

  "CPG for code with user-defined class containing one member defined inside its ctor" should {
    val cpg = code("""
      |class AClass(var x: String)
      |fun f1(p: String) {
      |    val aClass = AClass()
      |    aClass.x = p
      |    println(aClass.x)
      |}
      |""".stripMargin)

    "should find a flow through an assignment call of its member" in {
      val source = cpg.method.name("f1").parameter
      val sink   = cpg.method.name("println").callIn.argument
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(List(("f1(p)", Some(3)), ("aClass.x = p", Some(5)), ("println(aClass.x)", Some(6))))
    }
  }

  "CPG for code with member assignment" should {
    val cpg = code("""
      |class AClass {
      |    var x: String = "INITIAL"
      |}
      |
      |fun doSomething(p1: String): String {
      |    val aClass = AClass()
      |    aClass.x = p1
      |    val aVal = aClass.x
      |    return "NOTHING"
      |}
      |""".stripMargin)

    "should find a flow through the assignment" in {
      val source = cpg.method.name("doSomething").parameter
      val sink   = cpg.identifier.code("aVal")
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(List(("doSomething(p1)", Some(6)), ("aClass.x = p1", Some(8)), ("val aVal = aClass.x", Some(9))))
    }
  }
}
