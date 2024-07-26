package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class ReturnParserTests extends RubyParserFixture(newMatch = true) with Matchers {
  "Standalone return statement" in {
    test("return")
    test("return ::X.y()", "return self::X.y()")
    test("return(0)", "return 0")
  }
}
