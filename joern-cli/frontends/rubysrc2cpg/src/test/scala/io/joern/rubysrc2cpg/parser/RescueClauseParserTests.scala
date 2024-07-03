package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class RescueClauseParserTests extends RubyParserFixture with Matchers {
  "resuce statement" in {
    test("""begin
        |1/0
        |rescue ZeroDivisionError => e
        |end
        |""".stripMargin)

    test("""def foo;
        |1/0
        |rescue ZeroDivisionError => e
        |end
        |""".stripMargin)

    test("""foo x do |y|
        |y/0
        |rescue ZeroDivisionError => e
        |end
        |""".stripMargin)
  }

}
