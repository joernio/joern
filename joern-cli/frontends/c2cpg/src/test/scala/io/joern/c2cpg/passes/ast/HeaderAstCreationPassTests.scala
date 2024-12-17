package io.joern.c2cpg.passes.ast

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

class HeaderAstCreationPassTests extends C2CpgSuite {

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
      val fileNames = cpg.file.nameNot(".*<includes>|<unknown>").name.sorted
      fileNames shouldBe Seq("main.c", "main.h", "other.h")
    }

    "de-duplicate content correctly" in {
      inside(cpg.method.nameNot(NamespaceTraversal.globalNamespaceName).sortBy(_.fullName)) {
        case Seq(bar, foo, m, printf) =>
          // note that we don't see bar twice even so it is contained
          // in main.h and included in main.c and we do scan both
          bar.fullName shouldBe "bar"
          bar.filename shouldBe "main.h"
          foo.fullName shouldBe "foo"
          foo.filename shouldBe "other.h"
          // main is also deduplicated. It is defined within the header file,
          // and has an actual implementation in the source file
          m.fullName shouldBe "main"
          m.filename shouldBe "main.c"
          printf.fullName shouldBe "printf"
      }
    }

    "link includes correctly" in {
      inside(cpg.dependency.l) { case List(mainDep) =>
        mainDep.name shouldBe "main.h"
        mainDep.version shouldBe "include"
        mainDep.dependencyGroupId shouldBe Some("main.h")
        inside(cpg.imports.l) { case List(mainImport) =>
          mainImport.code shouldBe "#include \"main.h\""
          mainImport.importedEntity shouldBe Some("main.h")
          mainImport.importedAs shouldBe Some("main.h")
          mainImport._dependencyViaImportsOut.head shouldBe mainDep
          mainImport.file.name.l shouldBe List("main.c")
        }
      }
    }

  }

}
