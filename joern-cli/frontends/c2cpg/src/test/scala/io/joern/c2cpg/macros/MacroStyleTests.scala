package io.joern.c2cpg.macros

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.shiftleft.semanticcpg.language.*

class MacroStyleTests extends C2CpgSuite {

  "Macro calls" should {
    "distinguish object definitions from zero argument function definitions" in {
      val cpg = code("""
        |#define M_WAITOK 1
        |#define ZERO() 1
        |int object(void) { return M_WAITOK; }
        |int function(void) { return ZERO(); }
        |""".stripMargin)
      val objectMacro   = cpg.call.nameExact("M_WAITOK").head
      val functionMacro = cpg.call.nameExact("ZERO").head

      objectMacro.argument.size shouldBe 0
      functionMacro.argument.size shouldBe 0
      objectMacro.tag.nameExact("MACRO_STYLE").value.l shouldBe List("object")
      functionMacro.tag.nameExact("MACRO_STYLE").value.l shouldBe List("function")
    }
  }
}
