package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class BitwiseOperatorParserTests extends RubyParserFixture with Matchers {
  "Bitwise operators" in {
    test("1 & 3")
    test("2 & 4 & 3")
    test("1 | 9")
    test("1 ^ 20")
    test("1 >> 2")
    test("1 << 2")
  }
}
