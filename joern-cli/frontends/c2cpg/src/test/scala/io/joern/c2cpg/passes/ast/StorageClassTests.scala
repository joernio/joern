package io.joern.c2cpg.passes.ast

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.shiftleft.semanticcpg.language.*

class StorageClassTests extends C2CpgSuite {

  "Local declarations" should {
    "preserve static storage on every declarator" in {
      val cpg = code("""
        |int value(void) {
        |  static int first = 0, second = 1;
        |  int automatic = 0;
        |  return first + second + automatic;
        |}
        |""".stripMargin)

      cpg.method.nameExact("value").local.nameExact("first").tag.nameExact("STORAGE_CLASS").value.l shouldBe List(
        "static"
      )
      cpg.method.nameExact("value").local.nameExact("second").tag.nameExact("STORAGE_CLASS").value.l shouldBe List(
        "static"
      )
      cpg.method.nameExact("value").local.nameExact("automatic").tag.nameExact("STORAGE_CLASS").l shouldBe empty
    }

    "preserve extern storage independently of an initializer" in {
      val cpg = code("""
        |int global;
        |int value(void) {
        |  extern int global;
        |  return global;
        |}
        |""".stripMargin)

      cpg.method.nameExact("value").local.nameExact("global").tag.nameExact("STORAGE_CLASS").value.l shouldBe List(
        "extern"
      )
    }
  }
}
