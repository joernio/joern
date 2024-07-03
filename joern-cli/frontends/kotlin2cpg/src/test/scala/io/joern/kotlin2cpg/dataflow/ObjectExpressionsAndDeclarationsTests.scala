package io.joern.kotlin2cpg.dataflow

import io.joern.dataflowengineoss.language.toExtendedCfgNode
import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class ObjectExpressionsAndDeclarationsTests extends KotlinCode2CpgFixture(withOssDataflow = true) {
  implicit val resolver: ICallResolver = NoResolve

  "CPG for code with simple object expression" should {
    val cpg = code("""|package mypkg
        |
        |fun f1(p: String) {
        |    val o = object {
        |        val m = "meow"
        |        fun printWithSuffix(suffix: String) {
        |            println(suffix + m)
        |        }
        |    }
        |    o.printWithSuffix(p)
        |}
        |""".stripMargin)

    "should find a flow through a method defined on the object" in {
      val source = cpg.method.name("f1").parameter
      val sink   = cpg.method.name("println").callIn.argument
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("f1(p)", Some(3)),
            ("o.printWithSuffix(p)", Some(10)),
            ("printWithSuffix(this, suffix)", Some(6)),
            ("suffix + m", Some(7))
          )
        )

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

  "CPG for code with object declaration as argument of call" should {
    val cpg = code("""
        |interface AnInterface { fun doSomething(x: String) }
        |fun does(x: AnInterface, p: String) {
        |    x.doSomething(p)
        |}
        |fun f1(p: String) {
        |    does(object : AnInterface { override fun doSomething(x: String) { println(x) }}, p)
        |}
        |""".stripMargin)

    "should find a flow through an assignment call of its member" in {
      val source = cpg.method.name("f1").parameter
      val sink   = cpg.method.name("println").callIn.argument
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("f1(p)", Some(6)),
            ("does(object : AnInterface { override fun doSomething(x: String) { println(x) }}, p)", Some(7)),
            ("does(x, p)", Some(3)),
            ("x.doSomething(p)", Some(4)),
            ("doSomething(this, x)", Some(7)),
            ("println(x)", Some(7))
          )
        )
    }
  }

}
