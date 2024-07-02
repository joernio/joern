package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class BeginExpressionParserTests extends RubyParserFixture with Matchers {
  "Begin expression" in {
    test("""begin
        |1/0
        |rescue ZeroDivisionError => e
        |end""".stripMargin)
  }
}
