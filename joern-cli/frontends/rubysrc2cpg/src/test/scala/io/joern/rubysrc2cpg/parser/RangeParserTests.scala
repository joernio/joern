package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class RangeParserTests extends RubyParserFixture(newMatch = true) with Matchers {
  "Range Operator" in {
    test("1..2")
  }
}
