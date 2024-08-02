package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class ClassDefinitionParserTests extends RubyParserFixture with Matchers {
  "class definitions" in {
    test(
      "class << self ; end",
      """class << self.<anon-class-0>
        |end""".stripMargin
    )
    test(
      "class X 1 end",
      """class X
        |def <body>
        |1
        |end
        |end""".stripMargin
    )
    test(
      """class << x
        | def show; puts self; end
        |end
        |""".stripMargin,
      """class << x
        |def show
        |puts self
        |end
        |end""".stripMargin
    )
  }
}
