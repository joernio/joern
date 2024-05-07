package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.codepropertygraph.generated.nodes.TypeDecl
import io.shiftleft.semanticcpg.language.*

class LocalFunctionsTests extends KotlinCode2CpgFixture(withOssDataflow = false) {

  implicit val resolver: ICallResolver = NoResolve

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

    "contain a METHOD node for the local function with the correct PROPS set" in {
      val List(m: Method) = cpg.method.nameExact("f2").l
      m.fullName shouldBe "mypkg.f1.f2:void(java.lang.String)"
      m.signature shouldBe "void(java.lang.String)"
      m.parameter.size shouldBe 1
      m.block.size shouldBe 1
      m.block.expressionDown.size shouldBe 1
    }

    "contain TYPE_DECL nodes for the functions with the correct PROPS set" in {
      val List(m1: Method)   = cpg.method.nameExact("f1").l
      val List(m2: Method)   = cpg.method.nameExact("f2").l
      val List(t1: TypeDecl) = cpg.typeDecl.internal.nameExact("f1").l
      t1.fullName shouldBe "mypkg.f1:void(java.lang.String)"
      t1.bindsOut.flatMap(_.refOut).l should contain(m1)
      val List(t2: TypeDecl) = cpg.typeDecl.internal.nameExact("f2").l
      t2.fullName shouldBe "mypkg.f1.f2:void(java.lang.String)"
      t2.bindsOut.flatMap(_.refOut).l should contain(m2)
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

    "contain METHOD nodes for the local fns with the correct props set" in {
      val List(m1: Method) = cpg.method.nameExact("f1").l
      m1.fullName shouldBe "mypkg.f1:void(java.lang.String)"
      m1.signature shouldBe "void(java.lang.String)"
      m1.parameter.size shouldBe 1
      m1.block.size shouldBe 1
      m1.block.expressionDown.size shouldBe 1

      val List(m2: Method) = cpg.method.nameExact("f2").l
      m2.fullName shouldBe "mypkg.f1.f2:void(java.lang.String)"
      m2.signature shouldBe "void(java.lang.String)"
      m2.parameter.size shouldBe 1
      m2.block.size shouldBe 1
      m2.block.expressionDown.size shouldBe 1

      val List(m3: Method) = cpg.method.nameExact("f3").l
      m3.fullName shouldBe "mypkg.f1.f2.f3:void(java.lang.String)"
      m3.signature shouldBe "void(java.lang.String)"
      m3.parameter.size shouldBe 1
      m3.block.size shouldBe 1
      m3.block.expressionDown.size shouldBe 1

      val List(m4: Method) = cpg.method.nameExact("f4").l
      m4.fullName shouldBe "mypkg.f1.f2.f3.f4:void(java.lang.String)"
      m4.signature shouldBe "void(java.lang.String)"
      m4.parameter.size shouldBe 1
      m4.block.size shouldBe 1
      m4.block.expressionDown.size shouldBe 1
    }
  }
}
