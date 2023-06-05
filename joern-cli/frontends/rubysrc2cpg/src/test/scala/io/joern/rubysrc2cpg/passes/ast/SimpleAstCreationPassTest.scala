package io.joern.rubysrc2cpg.passes.ast

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language._

class SimpleAstCreationPassTest extends RubyCode2CpgFixture {

  "AST generation for simple fragments" should {

    "have correct structure for a single command call" in {
      val cpg = code("""puts 123""")

      val List(commandCall) = cpg.call.l
      val List(arg)         = commandCall.argument.isLiteral.l

      commandCall.code shouldBe "puts 123"
      commandCall.lineNumber shouldBe Some(1)

      arg.code shouldBe "123"
      arg.lineNumber shouldBe Some(1)
      arg.columnNumber shouldBe Some(5)
    }
  }

}
