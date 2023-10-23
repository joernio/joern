package io.joern.rubysrc2cpg.deprecated.passes.ast

import io.joern.rubysrc2cpg.deprecated.passes.Defines
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*
class LiteralCpgTests extends RubyCode2CpgFixture(useDeprecatedFrontend = true) {

  "A here doc string literal" should {
    val cpg = code("""<<-SQL
          |SELECT * FROM food
          |WHERE healthy = true
          |SQL
          |""".stripMargin)

    "be interpreted as a single literal string" in {
      val List(sql) = cpg.literal.l: @unchecked
      sql.code shouldBe
        """SELECT * FROM food
          |WHERE healthy = true
          |""".stripMargin.trim
      sql.lineNumber shouldBe Option(1)
      sql.columnNumber shouldBe Option(0)
      sql.typeFullName shouldBe Defines.String
    }
  }

}
