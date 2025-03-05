package io.joern.c2cpg.passes

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

class MethodFullNameUniquenessPassTests extends C2CpgSuite {

  "the MethodUniquenessPass" should {
    val cpg = code(
      """
        |void main() {
        |  foo(1, 2, 3);
        |}
    """.stripMargin,
      "main.c"
    ).moreCode(
      """
        |void foo(float a, float b, float c) {}
        |""".stripMargin,
      "h1.h"
    ).moreCode(
      """
        |void foo(int a, int b, int c) {}
        |""".stripMargin,
      "h2.h"
    )

    "create truly unique method fullnames" in {
      cpg.method.fullName.size shouldBe cpg.method.fullName.toSet.size
      cpg.method.nameNot(NamespaceTraversal.globalNamespaceName).fullName.sorted.l shouldBe List(
        "foo",
        "foo<duplicate>1",
        "main"
      )
    }
  }

}
