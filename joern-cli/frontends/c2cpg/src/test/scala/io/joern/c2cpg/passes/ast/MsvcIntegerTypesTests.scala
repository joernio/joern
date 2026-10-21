package io.joern.c2cpg.passes.ast

import io.joern.c2cpg.Config
import io.joern.c2cpg.testfixtures.AstC2CpgSuite
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*

class MsvcIntegerTypesTests extends AstC2CpgSuite {

  "Using MSVC integer types" should {

    "be correct for typedefs in C" in {
      val cpg = code("""
         |typedef signed __int8 INT8;
         |typedef signed char INT8_NATIVE;
         |typedef unsigned __int16 UINT16;
         |typedef unsigned short UINT16_NATIVE;
         |typedef __int32 INT32;
         |typedef int INT32_NATIVE;
         |typedef unsigned __int64 UINT64;
         |typedef unsigned long long UINT64_NATIVE;
         |""".stripMargin)
      def alias(name: String) = cpg.typeDecl.nameExact(name).aliasTypeFullName.l
      Seq("INT8", "UINT16", "INT32", "UINT64").foreach { name =>
        alias(name) should not be empty
        alias(name) shouldBe alias(s"${name}_NATIVE")
      }
    }

    "be correct for casts to a typedef built on them in C" in {
      val cpg = code("""
         |typedef unsigned __int64 UINT64;
         |typedef unsigned long UINTN;
         |void foo(UINT64 *o, int x) {
         |  *o = (UINT64)(UINTN)&x;
         |}
         |""".stripMargin)
      cpg.method.nameExact("foo").ast.isCall.nameExact(Operators.and).l shouldBe empty
      cpg.method.nameExact("foo").ast.isCall.nameExact(Operators.cast).code.l shouldBe List(
        "(UINT64)(UINTN)&x",
        "(UINTN)&x"
      )
      cpg.call.nameExact("__int64").l shouldBe empty
    }

    "be correct for casts to a typedef built on them in C++" in {
      val cpg = code(
        """
         |typedef unsigned __int64 UINT64;
         |typedef unsigned long long UINT64_NATIVE;
         |typedef unsigned long UINTN;
         |void foo(UINT64 *o, int x) {
         |  *o = (UINT64)(UINTN)&x;
         |}
         |""".stripMargin,
        "file.cpp"
      )
      cpg.typeDecl.nameExact("UINT64").aliasTypeFullName.l should not be empty
      cpg.typeDecl.nameExact("UINT64").aliasTypeFullName.l shouldBe
        cpg.typeDecl.nameExact("UINT64_NATIVE").aliasTypeFullName.l
      cpg.method.nameExact("foo").ast.isCall.nameExact(Operators.cast).code.l shouldBe List(
        "(UINT64)(UINTN)&x",
        "(UINTN)&x"
      )
    }

    "give precedence to user defines" in {
      val cpg = code("""
         |typedef unsigned __int64 UINT64;
         |typedef unsigned long UINT64_AS_DEFINED;
         |""".stripMargin).withConfig(Config().withDefines(Set("__int64=long")))
      cpg.typeDecl.nameExact("UINT64").aliasTypeFullName.l should not be empty
      cpg.typeDecl.nameExact("UINT64").aliasTypeFullName.l shouldBe
        cpg.typeDecl.nameExact("UINT64_AS_DEFINED").aliasTypeFullName.l
    }
  }

}
