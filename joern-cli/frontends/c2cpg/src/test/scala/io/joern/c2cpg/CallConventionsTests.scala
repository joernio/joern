package io.joern.c2cpg

import io.joern.c2cpg.fixtures.TestAstOnlyFixture
import io.shiftleft.semanticcpg.language._
import org.scalatest.Inside
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CallConventionsTests extends AnyWordSpec with Matchers with Inside with TestAstOnlyFixture {

  "Using calling conventions" should {

    "be correct for methods" in TestAstOnlyFixture("""
        |int __stdcall foo1() {
        |	strstr(a, "a");
        |}
        |
        |int __cdecl foo2() {
        |	strstr(a, "a");
        |}
        |
        |int __fastcall foo3() {
        |	strstr(a, "a");
        |}
        |
        |int __pascal foo4() {
        |	strstr(a, "a");
        |}
        |""".stripMargin) { cpg =>
      inside(cpg.method.nameNot("<global>").l) { case List(foo1, foo2, foo3, foo4) =>
        foo1.name shouldBe "foo1"
        foo1.ast.isCall.name.l shouldBe List("strstr")
        foo2.name shouldBe "foo2"
        foo2.ast.isCall.name.l shouldBe List("strstr")
        foo3.name shouldBe "foo3"
        foo3.ast.isCall.name.l shouldBe List("strstr")
        foo4.name shouldBe "foo4"
        foo4.ast.isCall.name.l shouldBe List("strstr")
      }
    }

  }

}
