package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*

class ConditionalTests extends RubyCode2CpgFixture {

  "`x ? y : z` is represented by a `conditional` operator call" in {
    val cpg = code("""
                     |x ? y : z
                     |""".stripMargin)

    val List(conditional) = cpg.call.name(Operators.conditional).l

    conditional.methodFullName shouldBe Operators.conditional
    conditional.code shouldBe "x ? y : z"
    conditional.lineNumber shouldBe Some(2)

    val List(test, trueExpr, falseExpr) = conditional.argument.l

    test.code shouldBe "x"
    test.lineNumber shouldBe Some(2)

    trueExpr.code shouldBe "y"
    trueExpr.lineNumber shouldBe Some(2)

    falseExpr.code shouldBe "z"
    falseExpr.lineNumber shouldBe Some(2)
  }

}
