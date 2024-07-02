package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class HashLiteralParserTests extends RubyParserFixture with Matchers {
  "hash-literal" in {
    test("{ }")
    test("{**x}")
    test("{**x, **y}")
    test("{**x, y => 1, **z}")
    test("{**group_by_type(some)}")
  }
}
