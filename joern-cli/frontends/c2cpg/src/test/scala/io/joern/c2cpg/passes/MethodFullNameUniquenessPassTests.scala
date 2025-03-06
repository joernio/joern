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

    "create truly unique method fullnames for const C++ functions" in {
      val cpg = code(
        """
          |class Foo {
          |  public:
          |    int foo(int a) {}
          |    int foo(int a) const {}
          |}
          |""".stripMargin,
        "main.cpp"
      )
      cpg.method.fullName.size shouldBe cpg.method.fullName.toSet.size
      cpg.method.nameNot(NamespaceTraversal.globalNamespaceName).fullName.sorted.l shouldBe List(
        // We can not distinguish between the two methods as they have the same signature.
        // `const` is not part of the signature.
        "Foo.foo:int(int)",
        "Foo.foo<duplicate>0:int(int)"
      )
    }
  }

}
