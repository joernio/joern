package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.semanticcpg.language._

class LocalFunctionsTests extends KotlinCode2CpgFixture(withOssDataflow = false) {

  implicit val resolver = NoResolve

  "CPG for code with a simple local fn definition" should {
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

    "should contain a METHOD node for the local function with the correct PROPS set" in {
      val List(m: Method) = cpg.method.nameExact("f2").l
      m.fullName shouldBe "mypkg.f1.f2:void(java.lang.String)"
      m.signature shouldBe "void(java.lang.String)"
      m.parameter.size shouldBe 1
      m.block.size shouldBe 1
      m.block.expressionDown.size shouldBe 1
    }
  }
}
