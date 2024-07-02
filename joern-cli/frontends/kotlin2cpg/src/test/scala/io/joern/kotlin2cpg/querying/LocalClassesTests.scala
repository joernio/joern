package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.TypeDecl
import io.shiftleft.semanticcpg.language.*

class LocalClassesTests extends KotlinCode2CpgFixture(withOssDataflow = false) {

  implicit val resolver: ICallResolver = NoResolve

  "CPG for code with a simple local fn declaration" should {
    val cpg = code("""
        |package mypkg
        |
        |fun sink(x: String) = println(x)
        |fun main() {
        |    class AClass {
        |        fun printQ(q: String) = sink(q)
        |    }
        |    val a = AClass()
        |    a.printQ("AMESSAGE")
        |}
        |""".stripMargin)

    "contain a TYPE_DECL node for the local class with the correct PROPS set" in {
      val List(td: TypeDecl) = cpg.typeDecl.nameExact("AClass").l
      td.fullName shouldBe "mypkg.main.AClass"
      td.method.filterNot { m => m.fullName.startsWith("mypkg.main.AClass") }.fullName.l shouldBe List()
      td.inheritsFromTypeFullName.l shouldBe List("java.lang.Object")
    }
  }

  "CPG for code with a deeply-nested local class declaration" should {
    val cpg = code("""
        |package mypkg
        |fun sink(x: String) = println(x)
        |fun f1(p: String) {
        |    class AClass {
        |        fun doSomething(r: String) {
        |            class BClass {
        |                fun doSomethingElse(s: String) {
        |                    sink(s)
        |                }
        |            }
        |            val bClass = BClass()
        |            bClass.doSomethingElse(r)
        |        }
        |    }
        |    val aClass = AClass()
        |    aClass.doSomething(p)
        |}
        |
        |""".stripMargin)

    "contain a TYPE_DECL node for the deeply-nested local class with the correct PROPS set" in {
      val List(td: TypeDecl) = cpg.typeDecl.nameExact("BClass").l
      td.fullName shouldBe "mypkg.f1.AClass.doSomething.BClass"
      td.method
        .filterNot { m => m.fullName.startsWith("mypkg.f1.AClass.doSomething.BClass") }
        .fullName
        .l shouldBe List()
      td.inheritsFromTypeFullName.l shouldBe List("java.lang.Object")
    }
  }
}
