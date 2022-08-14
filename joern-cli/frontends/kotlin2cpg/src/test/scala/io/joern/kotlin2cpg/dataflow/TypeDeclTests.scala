package io.joern.kotlin2cpg.dataflow

import io.joern.dataflowengineoss.language.toExtendedCfgNode
import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language._

class TypeDeclTests extends KotlinCode2CpgFixture(withOssDataflow = true) {
  implicit val resolver: ICallResolver = NoResolve

  "CPG for code with user-defined class containing one member defined inline" should {
    val cpg = code("""
      |class AClass { var x: String = "INITIAL" }
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

  "CPG for code with user-defined class with one method" should {
    val cpg = code("""
      |class AClass { fun itsFn(q: String) { println(q) } }
      |fun f1(p: String) {
      |    val aClass = AClass()
      |    aClass.itsFn(p)
      |}
      |""".stripMargin)

    "should find a flow through the explicit parameter of the method" in {
      val source = cpg.method.name("f1").parameter
      val sink   = cpg.method.name("println").callIn.argument
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(("f1(p)", Some(3)), ("aClass.itsFn(p)", Some(5)), ("itsFn(this, q)", Some(2)), ("println(q)", Some(2)))
        )
    }
  }

  "CPG for code with user-defined class with one method without paramters" should {
    val cpg = code("class AClass { fun printThis() { println(this) } }")

    "should find a flow from the method's implicit _this_ to the call using _this_" in {
      val source = cpg.method.name("printThis").parameter
      val sink   = cpg.method.name("println").callIn.argument
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(List(("printThis(this)", Some(1)), ("println(this)", Some(1))))
    }
  }

  "CPG for code with user-defined class with one extension function" should {
    val cpg = code("""
      |class AClass
      |fun AClass.itsFn(q: String) { println(q) }
      |fun f1(p: String) {
      |    val aClass = AClass()
      |    aClass.itsFn(p)
      |}
      |""".stripMargin)

    "should find a flow through the extension function" in {
      val source = cpg.method.name("f1").parameter
      val sink   = cpg.method.name("println").callIn.argument
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(List(("f1(p)", Some(4)), ("aClass.itsFn(p)", Some(6)), ("itsFn(q)", Some(3)), ("println(q)", Some(3))))
    }
  }

  "CPG for code with call to ctor" should {
    val cpg = code("""
        |class AClass(val x: String)
        |fun f1(p: String) {
        |  val aClass = AClass(p)
        |  println(aClass.x)
        |}
        |""".stripMargin)

    "should find a flow through the constructor" in {
      val source = cpg.method.name("f1").parameter
      val sink   = cpg.method.name("println").callIn.argument
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("f1(p)", Some(3)),
            ("AClass(p)", Some(4)),
            ("<init>(this, x)", Some(2)),
            ("this.x = x", Some(-1)),
            ("void", Some(-1)),
            ("AClass(p)", Some(4)),
            ("println(aClass.x)", Some(5))
          )
        )
    }
  }

  "CPG for code with call to secondary ctor" should {
    val cpg = code("""
        |class AClass {
        |  constructor(x: String) {
        |    println(x)
        |  }
        |}
        |
        |fun f1(p: String) {
        |  val aClass = AClass(p)
        |}
        |""".stripMargin)

    "should find a flow through " in {
      val source = cpg.method.name("f1").parameter
      val sink   = cpg.method.name("println").callIn.argument
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(List(("f1(p)", Some(8)), ("AClass(p)", Some(9)), ("<init>(this, x)", Some(3)), ("println(x)", Some(4))))
    }
  }
}
