package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class ImportTests extends RubyCode2CpgFixture {
  "`require 'test'` is a CALL node with an IMPORT node pointing to it" in {
    val cpg = code("""
                     |require 'test'
                     |""".stripMargin)
    val List(importNode) = cpg.imports.l
    importNode.importedEntity shouldBe Some("test")
    importNode.importedAs shouldBe Some("") // TODO: change X2Cpg import utility to allow optionals for importAs
    val List(call) = importNode.call.l
    call.callee.name.l shouldBe List("require")
    call.argument.code.l shouldBe List("'test'")
  }
}
