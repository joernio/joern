package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class ReturnParserTests extends RubyParserFixture with Matchers {
  "Standalone return statement" in {
    test("return")
    test("return ::X.y()", "return self::X.y()")
    test("return(0)", "return 0")
    test("return y(z:1)", "return y(z: 1)")
    test("return y(z=> 1)")
    test("return 1, :z => 1", "return 1,:z=> 1")
    test("return render 1,2,3")
  }
}
