package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.passes.Defines as RubyDefines
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class ModuleTests extends RubyCode2CpgFixture {

  "`module M ... end` is represented by a TYPE_DECL node" in {
    val cpg = code("""
                     |module M
                     |end
                     |""".stripMargin)

    val List(m) = cpg.typeDecl.name("M").l

    m.fullName shouldBe s"Test0.rb:${RubyDefines.Program}.M"
    m.lineNumber shouldBe Some(2)
    m.baseType.l shouldBe List()
    m.member.l shouldBe List()
    m.method.l shouldBe List()
  }
}
