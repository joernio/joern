package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class YieldParserTests extends RubyParserFixture with Matchers {
  "Yield tests" in {
    test("yield y(z: 1)")
    test("yield 1, :z => 1", "yield 1,:z=> 1")
  }
}
