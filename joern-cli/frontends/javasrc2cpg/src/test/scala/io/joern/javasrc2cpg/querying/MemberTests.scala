package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCodeToCpgFixture
import io.shiftleft.semanticcpg.language._
import org.scalatest.Ignore

class MemberTests extends JavaSrcCodeToCpgFixture {

  override val code: String =
    """
      |class Foo {
      |  int x;
      |}
      |""".stripMargin

  "should contain MEMBER node with correct properties" in {
    val List(x) = cpg.member("x").l
    x.name shouldBe "x"
    x.code shouldBe "int x"
    x.typeFullName shouldBe "int"
    x.order shouldBe 1
  }

  "should allow traversing from MEMBER to TYPE_DECL" in {
    val List(x) = cpg.member.typeDecl.l
    x.name shouldBe "Foo"
  }
}
