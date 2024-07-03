package io.joern.c2cpg.passes.ast

import io.joern.c2cpg.testfixtures.AstC2CpgSuite

import io.shiftleft.semanticcpg.language.*

class DependencyTests extends AstC2CpgSuite {

  "Dependency and imports" should {

    "be correct for includes" in {
      val cpg = code("""
        |#include "./folder/sub/foo.h"
        |#include <io.h>
        |""".stripMargin)
      inside(cpg.dependency.l) { case List(fooDep, ioDep) =>
        fooDep.name shouldBe "./folder/sub/foo.h"
        fooDep.version shouldBe "include"
        fooDep.dependencyGroupId shouldBe Option("./folder/sub/foo.h")
        ioDep.name shouldBe "io.h"
        ioDep.version shouldBe "include"
        ioDep.dependencyGroupId shouldBe Option("io.h")
        inside(cpg.imports.l) { case List(fooImport, ioImport) =>
          fooImport.code shouldBe "#include \"./folder/sub/foo.h\""
          fooImport.importedEntity shouldBe Option("./folder/sub/foo.h")
          fooImport.importedAs shouldBe Option("./folder/sub/foo.h")
          fooImport._dependencyViaImportsOut.head shouldBe fooDep
          ioImport.code shouldBe "#include <io.h>"
          ioImport.importedEntity shouldBe Option("io.h")
          ioImport.importedAs shouldBe Option("io.h")
          ioImport._dependencyViaImportsOut.head shouldBe ioDep
        }
      }
    }
  }

}
