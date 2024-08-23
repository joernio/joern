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
    test(
      """class Foo
        | def self.show
        | end
        |end
        |""".stripMargin,
      """class Foo
        |def <body>
        |
        |end
        |def self.show
        | end
        |end""".stripMargin
    )
  }

  "class definitions with comments" in {
    test(
      """#blah 1
        |#blah 2
        |class X
        |#blah 3
        |def blah
        |#blah4
        |end
        |end
        |""".stripMargin,
      """class X
        |def <body>
        |
        |end
        |def blah
        |#blah4
        |end
        |end""".stripMargin
    )
  }
}
