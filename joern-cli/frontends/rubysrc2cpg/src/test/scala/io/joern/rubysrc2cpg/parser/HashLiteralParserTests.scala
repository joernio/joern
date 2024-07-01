package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class HashLiteralParserTests extends RubyParserFixture with Matchers {
  "fix tests" in {
    // TODO: Syntax error
    test("{**group_by_type(some)}")
  }

  "hash-literal" in {
    test("{ }")
    test("{**x}")
    test("{**x, **y}")
    test("{**x, y => 1, **z}")
  }
}
