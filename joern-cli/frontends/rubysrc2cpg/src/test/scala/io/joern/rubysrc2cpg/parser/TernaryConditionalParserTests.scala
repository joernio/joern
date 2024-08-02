package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class TernaryConditionalParserTests extends RubyParserFixture with Matchers {
  "ternary conditional expressions" in {
    test("x ? y : z")
    test(
      """x ?
        | y
        |: z
        |""".stripMargin,
      "x ? y : z"
    )
  }
}
