package io.joern.c2cpg.passes.ast

import better.files.File
import io.joern.c2cpg.testfixtures.CCodeToCpgSuite
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

class HeaderAstCreationPassTests extends CCodeToCpgSuite {

  "HeaderAstCreationPass" should {
    val cpg = code(
      """
        |#include "main.h"
        |
        |int main() {
        |	printf("Hello World\n");
        |	return 0;
        |}
        |""".stripMargin,
      "main.c"
    ).moreCode(
      """
        |int main();
        |void bar() { return; };
        |""".stripMargin,
      "main.h"
    ).moreCode(
      """
        |int foo();
        |""".stripMargin,
      "other.h"
    )

    "create all source and header files correctly" in {
      val fileNames = cpg.file.nameNot(".*<includes>|<unknown>").name.sorted.map(File(_).name)
      fileNames shouldBe Seq("main.c", "main.h", "other.h")
    }

    "de-duplicate content correctly" in {
      inside(cpg.method.nameNot(NamespaceTraversal.globalNamespaceName).sortBy(_.fullName)) {
        case Seq(bar, foo, m1, m2, printf) =>
          // note that we don't see bar twice even so it is contained
          // in main.h and included in main.c and we do scan both
          bar.fullName shouldBe "bar"
          bar.filename should endWith("main.h")
          foo.fullName shouldBe "foo"
          foo.filename should endWith("other.h")
          // main is include twice. First time for the header file,
          // second time for the actual implementation in the source file
          // We do not de-duplicate this as line/column numbers differ
          m1.fullName shouldBe "main"
          m1.filename should endWith("main.h")
          m2.fullName shouldBe "main"
          m2.filename should endWith("main.c")
          printf.fullName shouldBe "printf"
      }
    }

  }

}
