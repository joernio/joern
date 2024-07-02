package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class CaseConditionParserTests extends RubyParserFixture with Matchers {
  "A case expression" in {
    test("""case something
        | when 1
        |   puts 2
        |end
        |""".stripMargin)

    test("""case something
        | when 1
        | else
        | end
        |""".stripMargin)

    test("""case something
        | when 1 then
        | end
        |""".stripMargin)

    test("""case x
        | when 1 then 2
        | when 2 then 3
        | end
        |""".stripMargin)
  }
}
