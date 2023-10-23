package io.joern.rubysrc2cpg.deprecated.passes.ast

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class ImportAstCreationTest extends RubyCode2CpgFixture(useDeprecatedFrontend = true) {

  "Ast creation for import node" should {
    val cpg = code("""
        |require "dummy_logger"
        |require_relative "util/help.rb"
        |load "mymodule.rb"
        |""".stripMargin)
    val imports = cpg.imports.l
    val calls   = cpg.call("require|require_relative|load").l
    "have a valid import node" in {
      imports.importedEntity.l shouldBe List("dummy_logger", "util/help.rb", "mymodule.rb")
      imports.importedAs.l shouldBe List("dummy_logger", "util/help.rb", "mymodule.rb")
    }

    "have a valid call node" in {
      calls.code.l shouldBe List(
        "require \"dummy_logger\"",
        "require_relative \"util/help.rb\"",
        "load \"mymodule.rb\""
      )
    }

    "have a valid linking" in {
      calls.referencedImports.importedEntity.l shouldBe List("dummy_logger", "util/help.rb", "mymodule.rb")
    }
  }
}
