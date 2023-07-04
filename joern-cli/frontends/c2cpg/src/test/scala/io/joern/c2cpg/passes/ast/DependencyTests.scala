package io.joern.c2cpg.passes.ast

import io.joern.c2cpg.testfixtures.AbstractPassTest

import io.shiftleft.semanticcpg.language._

class DependencyTests extends AbstractPassTest {

  "Dependency and imports" should {

    "be correct for includes" in AstFixture("""
        |#include "./folder/sub/foo.h"
        |#include <io.h>
        |""".stripMargin) { cpg =>
      inside(cpg.dependency.l) { case List(fooDep, ioDep) =>
        fooDep.name shouldBe "./folder/sub/foo.h"
        fooDep.version shouldBe "include"
        fooDep.dependencyGroupId shouldBe Some("./folder/sub/foo.h")
        ioDep.name shouldBe "io.h"
        ioDep.version shouldBe "include"
        ioDep.dependencyGroupId shouldBe Some("io.h")
        inside(cpg.imports.l) { case List(fooImport, ioImport) =>
          fooImport.code shouldBe "#include \"./folder/sub/foo.h\""
          fooImport.importedEntity shouldBe Some("./folder/sub/foo.h")
          fooImport.importedAs shouldBe Some("./folder/sub/foo.h")
          fooImport._dependencyViaImportsOut.head shouldBe fooDep
          ioImport.code shouldBe "#include <io.h>"
          ioImport.importedEntity shouldBe Some("io.h")
          ioImport.importedAs shouldBe Some("io.h")
          ioImport._dependencyViaImportsOut.head shouldBe ioDep
        }
      }
    }
  }

}
