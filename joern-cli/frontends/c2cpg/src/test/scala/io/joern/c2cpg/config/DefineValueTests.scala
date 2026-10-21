package io.joern.c2cpg.config

import io.joern.c2cpg.Config
import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.shiftleft.semanticcpg.language.*

class DefineValueTests extends C2CpgSuite {

  "a define whose value contains '='" should {
    val cpg = code("""
        |const char *with_eq = PROMPT_EQ;
        |const char *without_eq = PROMPT_PLAIN;
        |int after(void) { return 1; }
        |""".stripMargin).withConfig(Config().withDefines(Set("PROMPT_EQ=\"=> \"", "PROMPT_PLAIN=\"> \"")))

    "keep the whole value" in {
      cpg.literal.code.l should contain allOf ("\"=> \"", "\"> \"")
    }
  }

  "a define with an empty value" should {
    val cpg = code("""
        |int f(void) { return 1 EMPTY; }
        |""".stripMargin).withConfig(Config().withDefines(Set("EMPTY=")))

    "be accepted and expand to nothing" in {
      cpg.method.nameExact("f").ast.isReturn.code.l shouldBe List("return 1 EMPTY;")
      cpg.method.nameExact("f").ast.isLiteral.code.l shouldBe List("1")
    }
  }

}
