package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class RangeParserTests extends RubyParserFixture with Matchers {
  "fixme" ignore {
    test("""0...
        |; a...
        |; c
        |""".stripMargin) // Syntax error
  }

  "Range Operator" in {
    test("1..2")
    test(
      """0..
        |4
        |a..
        |b
        |c
        |""".stripMargin,
      """0..4
        |a..b
        |c""".stripMargin
    )
  }
}
