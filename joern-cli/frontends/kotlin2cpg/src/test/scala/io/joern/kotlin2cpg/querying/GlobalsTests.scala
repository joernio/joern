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

    "should contain a LOCAL node for `AGLOBAL` with the correct props set" in {
      val List(a) = cpg.local.nameExact("AGLOBAL").l
      a.code shouldBe "AGLOBAL"
      a.typeFullName shouldBe "java.lang.String"
    }
  }
}
