package io.joern.c2cpg.passes.ast

import io.joern.c2cpg.testfixtures.AstC2CpgSuite
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

class CallConventionsTests extends AstC2CpgSuite {

  "Using calling conventions" should {

    "be correct for methods" in {
      val cpg = code("""
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
         |""".stripMargin)
      inside(cpg.method.nameNot(NamespaceTraversal.globalNamespaceName).l) { case List(foo1, foo2, foo3, foo4) =>
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
