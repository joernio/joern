package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class FieldAccessParserTests extends RubyParserFixture with Matchers {
  "Normal field access" in {
    test("x.y")
    test("self.x")
  }
}
