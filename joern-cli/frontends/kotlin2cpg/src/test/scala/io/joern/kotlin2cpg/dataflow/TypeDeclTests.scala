package io.joern.kotlin2cpg.dataflow

import io.joern.dataflowengineoss.language.toExtendedCfgNode
import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language._

class TypeDeclTests extends KotlinCode2CpgFixture(withOssDataflow = true) {
  implicit val resolver: ICallResolver = NoResolve

  "CPG for code with class definition with member defined inside ctor" should {
    val cpg = code("""
        |class AClass(var x: String) {
        |    fun printX() = println(this.x)
        |}
        |fun f1(p: String) {
        |    val aClass = AClass(p)
        |    aClass.printX()
        |}
        |fun main() = f1("SOMETHING")
        |""".stripMargin)

    "should find a flow through member assigned in ctor" in {
      val source = cpg.method.name("f1").parameter
      val sink   = cpg.call.methodFullName(".*println.*").argument
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("f1(p)", Some(5)),
            ("AClass(p)", Some(6)),
            ("<init>(this, x)", Some(2)),
            ("this.x = x", Some(-1)),
            ("void", Some(-1)),
            ("AClass(p)", Some(6)),
            ("aClass.printX()", Some(7)),
            ("printX(this)", Some(3)),
            ("println(this.x)", Some(3))
          )
        )
    }
  }

  "CPG for code with type alias of class definition with member defined inside ctor" should {
    val cpg = code("""
      |typealias AnAlias = AClass
      |class AClass {
      |    var x: String
      |    constructor(q: String) {
      |        this.x = q
      |    }
      |    fun printX() = println(this.x)
      |}
      |fun f1(p: String) {
      |    val aClass = AnAlias(p)
      |    aClass.printX()
      |}
      |fun main() = f1("SOMETHING"
      |""".stripMargin)

    "should find a flow through member assigned in ctor" in {
      val source = cpg.method.name("f1").parameter
      val sink   = cpg.call.methodFullName(".*println.*").argument
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("f1(p)", Some(10)),
            ("AnAlias(p)", Some(11)),
            ("<init>(this, q)", Some(5)),
            ("this.x = q", Some(6)),
            ("void", Some(-1)),
            ("AnAlias(p)", Some(11)),
            ("aClass.printX()", Some(12)),
            ("printX(this)", Some(8)),
            ("println(this.x)", Some(8))
          )
        )
    }
  }

  "CPG for code with class definition with member assignment inside secondary ctor" should {
    val cpg = code("""
      |class AClass {
      |    var x: String
      |    constructor(q: String) {
      |        this.x = q
      |    }
      |    fun printX() = println(this.x)
      |}
      |fun f1(p: String) {
      |    val aClass = AClass(p)
      |    aClass.printX()
      |}
      |""".stripMargin)

    "should find a flow through member assigned in ctor" in {
      val source = cpg.method.name("f1").parameter
      val sink   = cpg.call.methodFullName(".*println.*").argument
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("f1(p)", Some(9)),
            ("AClass(p)", Some(10)),
            ("<init>(this, q)", Some(4)),
            ("this.x = q", Some(5)),
            ("void", Some(-1)),
            ("AClass(p)", Some(10)),
            ("aClass.printX()", Some(11)),
            ("printX(this)", Some(7)),
            ("println(this.x)", Some(7))
          )
        )
    }
  }

  "CPG for code with class extending another class" should {
    val cpg = code("""
       |open class AClass { open fun printP(p: String) = println(p) }
       |class BClass : AClass() { override fun printP(p: String) = super.printP(p + "_") }
       |fun doSomething(x: String) {
       |    val b = BClass()
       |    b.printP(x)
       |}
       |""".stripMargin)

    "should find a flow through fn of superclass" in {
      val source = cpg.method.name("doSomething").parameter
      val sink   = cpg.call.code("println.*").argument
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("doSomething(x)", Some(4)),
            ("b.printP(x)", Some(6)),
            ("printP(this, p)", Some(3)),
            ("p + \"_\"", Some(3)),
            ("printP(this, p)", Some(2)),
            ("println(p)", Some(2))
          )
        )
    }
  }

}
