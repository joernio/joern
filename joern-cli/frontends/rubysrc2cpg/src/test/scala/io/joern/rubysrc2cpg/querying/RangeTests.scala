package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*

class RangeTests extends RubyCode2CpgFixture {

  "`0..1` is represented by a `range` operator call" in {
    val cpg = code("""
                     |0..1
                     |""".stripMargin)

    val List(range) = cpg.call(Operators.range).l

    range.methodFullName shouldBe Operators.range
    range.code shouldBe "0..1"
    range.lineNumber shouldBe Some(2)

    val List(lowerBound, upperBound) = range.argument.l

    lowerBound.code shouldBe "0"
    upperBound.code shouldBe "1"
  }

}
