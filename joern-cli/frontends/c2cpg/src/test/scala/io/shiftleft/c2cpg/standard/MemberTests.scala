package io.shiftleft.c2cpg.standard

import io.shiftleft.c2cpg.testfixtures.CCodeToCpgSuite
import io.shiftleft.semanticcpg.language._

class MemberTests extends CCodeToCpgSuite {

  override val code: String =
    """
      |struct foo {
      |  int x;
      |}
      |""".stripMargin

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

}
