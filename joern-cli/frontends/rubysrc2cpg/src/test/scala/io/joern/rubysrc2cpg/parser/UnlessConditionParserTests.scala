package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class UnlessConditionParserTests extends RubyParserFixture with Matchers {
  "Unless expression" in {
    test("""unless foo
        | bar
        |end
        |""".stripMargin)

    test("""unless foo; bar
        |end
        |""".stripMargin)

    test("""unless foo then
        | bar
        |end
        |""".stripMargin)

    test("""unless __LINE__ == 0 then
        |else
        |end
        |""".stripMargin)

    test("return(value) unless item")
  }
}
