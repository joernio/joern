package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class GlobalsTests extends KotlinCode2CpgFixture(withOssDataflow = false) {
  "CPG for code simple global declaration" should {
    val cpg = code("""
        |val AGLOBAL = "A_GLOBAL"
        |fun f1() {
        |    println(AGLOBAL)
        |}
        |""".stripMargin)

    "should contain a LOCAL node for `AGLOBAL` in <global> with the correct props set" in {
      val List(globalMethod) = cpg.method.nameExact("<global>").l
      val List(aGlobal)      = globalMethod.local.nameExact("AGLOBAL").l
      aGlobal.code shouldBe "AGLOBAL"
      aGlobal.typeFullName shouldBe "java.lang.String"
      aGlobal.closureBindingId shouldBe None
    }

    "should contain a captured LOCAL node for `AGLOBAL` in `f1`" in {
      val List(f1Local) = cpg.method.nameExact("f1").local.nameExact("AGLOBAL").l
      f1Local.typeFullName shouldBe "java.lang.String"
      f1Local.closureBindingId should not be None
    }

    "should have `AGLOBAL` identifier in `f1` referencing the local in `f1`" in {
      val List(idInF1)    = cpg.method.nameExact("f1").ast.isIdentifier.nameExact("AGLOBAL").l
      val List(localInF1) = cpg.method.nameExact("f1").local.nameExact("AGLOBAL").l
      idInF1.refsTo.l shouldBe List(localInF1)
    }
  }
}
