package io.joern.c2cpg.passes.ast

import io.joern.c2cpg.Config
import io.joern.c2cpg.testfixtures.AstC2CpgSuite
import io.shiftleft.semanticcpg.language.*

class StandardTypeMacrosTests extends AstC2CpgSuite {

  "Standard types declared through a compiler's type macros" should {

    "be correct for typedefs in C" in {
      val cpg = code("""
         |typedef __SIZE_TYPE__ size_t;
         |typedef unsigned long size_t_NATIVE;
         |typedef __UINT32_TYPE__ uint32_t;
         |typedef unsigned int uint32_t_NATIVE;
         |""".stripMargin)
      def alias(name: String) = cpg.typeDecl.nameExact(name).aliasTypeFullName.l
      Seq("size_t", "uint32_t").foreach { name =>
        alias(name) should not be empty
        alias(name) shouldBe alias(s"${name}_NATIVE")
      }
    }

    "be correct for a variable whose type is deduced from them in C++" in {
      val cpg = code(
        """
         |typedef __SIZE_TYPE__ size_t;
         |void foo(size_t n) {
         |  auto m = n + 1;
         |}
         |""".stripMargin,
        "file.cpp"
      )
      cpg.method.nameExact("foo").local.nameExact("m").typeFullName.l shouldBe List("size_t")
    }

    "give precedence to user defines" in {
      val cpg = code("""
         |typedef __SIZE_TYPE__ size_t;
         |typedef unsigned int size_t_AS_DEFINED;
         |""".stripMargin).withConfig(Config().withDefines(Set("__SIZE_TYPE__=unsigned int")))
      cpg.typeDecl.nameExact("size_t").aliasTypeFullName.l should not be empty
      cpg.typeDecl.nameExact("size_t").aliasTypeFullName.l shouldBe
        cpg.typeDecl.nameExact("size_t_AS_DEFINED").aliasTypeFullName.l
    }
  }

}
