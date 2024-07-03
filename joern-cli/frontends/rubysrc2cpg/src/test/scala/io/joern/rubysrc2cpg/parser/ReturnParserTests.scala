package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class ReturnParserTests extends RubyParserFixture with Matchers {
  "Standalone return statement" in {
    test("return")
    test("return ::X.y()")
    test("return(0)")
  }
}
