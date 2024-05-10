package io.joern.kotlin2cpg.validation

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.codepropertygraph.generated.nodes.TypeDecl
import io.shiftleft.semanticcpg.language.*

class NestedDeclarationsTests extends KotlinCode2CpgFixture(withOssDataflow = false) {
  "CPG for code with a simple local fn declaration" should {
    val cpg = code("""
        |package mypkg
        |
        |fun f1(p: String) {
        |    fun f2(q: String) {
        |        println(q)
        |    }
        |    f2(p)
        |}
        |""".stripMargin)

    "contain METHODs node for the local fns with the correct `astParent`s set" in {
      val List(m: Method)         = cpg.method.nameExact("f2").l
      val List(astParent: Method) = m.astIn.l: @unchecked
      astParent.fullName shouldBe cpg.method.nameExact("f1").fullName.head
    }
  }

  "CPG for code with deeply-nested local fn declarations" should {
    val cpg = code("""
        |package mypkg
        |
        |fun f1(p: String) {
        |    fun f2(q: String) {
        |        fun f3(r: String) {
        |            fun f4(s: String) {
        |                println(s)
        |            }
        |            f4(r)
        |        }
        |        f3(q)
        |    }
        |    f2(p)
        |}
        |""".stripMargin)

    "contain METHOD nodes for the local fns with the correct `astParent`s set" in {
      val List(m2: Method)         = cpg.method.nameExact("f2").l
      val List(astParent2: Method) = m2.astIn.l: @unchecked
      astParent2.fullName shouldBe cpg.method.nameExact("f1").fullName.head

      val List(m3: Method)         = cpg.method.nameExact("f3").l
      val List(astParent3: Method) = m3.astIn.l: @unchecked
      astParent3.fullName shouldBe cpg.method.nameExact("f2").fullName.head

      val List(m4: Method)         = cpg.method.nameExact("f4").l
      val List(astParent4: Method) = m4.astIn.l: @unchecked
      astParent4.fullName shouldBe cpg.method.nameExact("f3").fullName.head
    }
  }

  "CPG for code with a simple local class declaration" should {
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

    "contain TYPE_DECL nodes for the local classes with the correct `astParent`s set" in {
      val List(td: TypeDecl)      = cpg.typeDecl.nameExact("AClass").l
      val List(astParent: Method) = td.astIn.l: @unchecked
      astParent.fullName shouldBe cpg.method.nameExact("main").fullName.head
    }
  }

  "CPG for code with deeply nested local class declarations" should {
    val cpg = code("""
        |package mypkg
        |
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
        |""".stripMargin)

    "contain TYPE_DECL nodes for the local classes with the correct `astParent`s set" in {
      val List(td: TypeDecl)      = cpg.typeDecl.nameExact("AClass").l
      val List(astParent: Method) = td.astIn.l: @unchecked
      astParent.fullName shouldBe cpg.method.nameExact("f1").fullName.head

      val List(td2: TypeDecl)      = cpg.typeDecl.nameExact("BClass").l
      val List(astParent2: Method) = td2.astIn.l: @unchecked
      astParent2.fullName shouldBe cpg.method.nameExact("doSomething").fullName.head
    }
  }

}
