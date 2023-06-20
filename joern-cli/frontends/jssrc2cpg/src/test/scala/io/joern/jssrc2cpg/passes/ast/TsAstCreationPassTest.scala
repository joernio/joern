package io.joern.jssrc2cpg.passes.ast

import io.joern.jssrc2cpg.passes.AbstractPassTest
import io.joern.jssrc2cpg.passes.Defines
import io.shiftleft.semanticcpg.language._

class TsAstCreationPassTest extends AbstractPassTest {

  "AST generation for simple TS constructs" should {

    "create methods for const exports" in TsAstFixture(
      "export const getApiA = (req: Request) => { const user = req.user as UserDocument; }"
    ) { cpg =>
      cpg.method.name.sorted.l shouldBe List(":program", "anonymous")
      cpg.assignment.code.l shouldBe List(
        "const user = req.user as UserDocument",
        "const getApiA = (req: Request) => { const user = req.user as UserDocument; }",
        "exports.getApiA = getApiA"
      )
      inside(cpg.method.name("anonymous").l) { case List(anon) =>
        anon.fullName shouldBe "code.ts::program:anonymous"
        anon.ast.isIdentifier.name.l shouldBe List("user", "req")
      }
    }

    "have correct structure for import assignments" in TsAstFixture("""
        |import fs = require('fs');
        |import models = require('../models/index');
        |""".stripMargin) { cpg =>
      cpg.assignment.code.l shouldBe List("var fs = require(\"fs\")", "var models = require(\"../models/index\")")
      cpg.local.code.l shouldBe List("fs", "models")
      val List(fsDep, modelsDep) = cpg.dependency.l
      fsDep.name shouldBe "fs"
      fsDep.dependencyGroupId shouldBe Option("fs")
      modelsDep.name shouldBe "models"
      modelsDep.dependencyGroupId shouldBe Option("../models/index")

      val List(fs, models) = cpg.imports.l
      fs.code shouldBe "import fs = require('fs')"
      fs.importedEntity shouldBe Option("fs")
      fs.importedAs shouldBe Option("fs")
      models.code shouldBe "import models = require('../models/index')"
      models.importedEntity shouldBe Option("../models/index")
      models.importedAs shouldBe Option("models")
    }

    "have correct structure for declared functions" in TsAstFixture("declare function foo(arg: string): string") {
      cpg =>
        val List(func) = cpg.method("foo").l
        func.code shouldBe "declare function foo(arg: string): string"
        func.name shouldBe "foo"
        func.fullName shouldBe "code.ts::program:foo"
        val List(_, arg) = cpg.method("foo").parameter.l
        arg.name shouldBe "arg"
        arg.typeFullName shouldBe Defines.String
        arg.code shouldBe "arg: string"
        arg.index shouldBe 1
        val List(parentTypeDecl) = cpg.typeDecl.name(":program").l
        parentTypeDecl.bindsOut.flatMap(_.refOut).l should contain(func)
    }

    "have correct structure for type assertion" in TsAstFixture("let emptyArray = <VNode[]>[];") { cpg =>
      cpg.assignment.code.l shouldBe List("let emptyArray = <VNode[]>[]")
    }

    "have correct structure for satisfies expressions" in TsAstFixture("let x = y satisfies T;") { cpg =>
      val List(assignment) = cpg.assignment.l
      val List(x, y)       = assignment.argument.l
      assignment.code shouldBe "let x = y satisfies T"
      x.code shouldBe "x"
      y.code shouldBe "y"
    }
  }

}
