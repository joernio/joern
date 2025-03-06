package io.joern.c2cpg.passes

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

class MethodFullNameUniquenessPassTests extends C2CpgSuite {

  "the MethodUniquenessPass" should {
    "create truly unique method fullnames for multiple C functions" in {
      val cpg = code(
        """
          |#include "h1.h"
          |void main() {
          |  foo(1, 2, 3);
          |}
          """.stripMargin,
        "main.c"
      ).moreCode(
        """
          |void foo(int a, int b, int c) {}
          |""".stripMargin,
        "h1.h"
      ).moreCode(
        """
          |void foo(float a, float b, float c) {}
          |""".stripMargin,
        "h2.h"
      )
      cpg.method.fullName.size shouldBe cpg.method.fullName.toSet.size
      cpg.method.nameNot(NamespaceTraversal.globalNamespaceName).fullName.sorted.l shouldBe List(
        "foo",
        "foo<duplicate>0",
        "main"
      )
    }

    "respect  method fullnames for const C++ functions" in {
      val cpg = code(
        """
          |class Foo {
          |  public:
          |    int foo(int a) { return 0; }
          |    int foo(int a) const { return 0; }
          |}
          |
          |void main() {
          |  Foo f1 = new Foo();
          |  const Foo f2 = new Foo();
          |  f1.foo(1);
          |  f2.foo(2);
          |}
          |""".stripMargin,
        "main.cpp"
      )
      cpg.method.fullName.size shouldBe cpg.method.fullName.toSet.size
      cpg.method.nameExact("foo").fullName.sorted.l shouldBe List("Foo.foo:int(int)", "Foo.foo<const>:int(int)")
      val List(call1, call2) = cpg.call.name("foo").sortBy(_.lineNumber).l
      call1.methodFullName shouldBe "Foo.foo:int(int)"
      call2.methodFullName shouldBe "Foo.foo<const>:int(int)"
    }
  }

}
