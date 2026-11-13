package io.joern.c2cpg.passes.ast

import io.joern.c2cpg.testfixtures.AstC2CpgSuite
import io.shiftleft.semanticcpg.language.*

class AttributeTests extends AstC2CpgSuite {

  "Attributes" should {

    "be recorded as an annotation on a function definition" in {
      val cpg = code(
        """
          |[[nodiscard]] int mustCheck() { return 1; }
          |[[deprecated]] void doNotCall() {}
          |int plain() { return 2; }
          |""".stripMargin,
        "test.cpp"
      )
      cpg.method.nameExact("mustCheck").annotation.name.l shouldBe List("nodiscard")
      cpg.method.nameExact("doNotCall").annotation.name.l shouldBe List("deprecated")
      cpg.method.nameExact("plain").annotation.l shouldBe empty
    }

    "keep the scope of a scoped attribute in the full name" in {
      val cpg = code(
        """
          |[[gnu::always_inline]] int inlined() { return 1; }
          |""".stripMargin,
        "test.cpp"
      )
      inside(cpg.method.nameExact("inlined").annotation.l) { case List(annotation) =>
        annotation.name shouldBe "always_inline"
        annotation.fullName shouldBe "gnu::always_inline"
      }
    }

    "be recorded for every attribute when a function carries more than one" in {
      val cpg = code(
        """
          |[[nodiscard, deprecated]] int both() { return 1; }
          |""".stripMargin,
        "test.cpp"
      )
      cpg.method.nameExact("both").annotation.name.sorted.l shouldBe List("deprecated", "nodiscard")
    }

    "be absent for a function that carries none, and for C" in {
      val cpg = code("int cppPlain() { return 1; }", "test.cpp")
        .moreCode("int cPlain() { return 1; }", "test.c")
      cpg.method.nameExact("cppPlain").annotation.l shouldBe empty
      cpg.method.nameExact("cPlain").annotation.l shouldBe empty
    }

  }

}
