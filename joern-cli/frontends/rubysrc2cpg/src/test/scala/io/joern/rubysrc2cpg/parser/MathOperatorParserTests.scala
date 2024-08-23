package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class MathOperatorParserTests extends RubyParserFixture with Matchers {
  "Math operators" in {
    test("1 + 2")
    test("1 - 2")
    test("1 * 2")
    test("1 / 2")
    test("1 ** 2")
    test("1 + 2 - 3 * 4 / 5 ** 2")
  }
}
