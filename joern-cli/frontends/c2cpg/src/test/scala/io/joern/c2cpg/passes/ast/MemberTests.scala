package io.joern.c2cpg.passes.ast

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.shiftleft.semanticcpg.language.*

class MemberTests extends C2CpgSuite {

  private val cpg = code("""
      |struct foo {
      |  int x;
      |}
      |""".stripMargin)

  "should contain MEMBER node with correct properties" in {
    val List(x) = cpg.member.l
    x.name shouldBe "x"
    x.code shouldBe "x"
    x.typeFullName shouldBe "int"
    x.order shouldBe 1
  }

  "should allow traversing from MEMBER to TYPE_DECL" in {
    val List(x) = cpg.member.typeDecl.l
    x.name shouldBe "foo"
  }

  "should type bit-field members, where `int` and `signed int` are the same type" in {
    val bitFields = code(
      """
        |struct bits {
        |  int a: 8;
        |  signed int b: 8;
        |  unsigned int c: 8;
        |  signed d: 8;
        |  unsigned e: 8;
        |  signed char f: 8;
        |  char g: 8;
        |};
        |""".stripMargin,
      "bits.c"
    )
    bitFields.member.map(member => member.code -> member.typeFullName).l shouldBe List(
      "a: 8" -> "int",
      "b: 8" -> "int",
      "c: 8" -> "unsigned int",
      "d: 8" -> "int",
      "e: 8" -> "unsigned int",
      "f: 8" -> "signed char",
      "g: 8" -> "char"
    )
  }

}
