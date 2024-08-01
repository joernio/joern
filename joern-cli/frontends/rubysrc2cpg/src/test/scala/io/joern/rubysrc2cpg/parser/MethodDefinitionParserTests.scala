package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class MethodDefinitionParserTests extends RubyParserFixture with Matchers {
  "single line method definition" in {
    test(
      "def foo; end",
      """def foo
        |end""".stripMargin
    )

    test(
      "def foo(x); end",
      """def foo(x)
        |end""".stripMargin
    )

    test(
      "def foo(x=1); end",
      """def foo(x=1)
        |end""".stripMargin
    )

    test(
      "def foo(x, &y); end",
      """def foo(x,&y)
        |end""".stripMargin
    )

    test(
      "def foo(*arr); end",
      """def foo(*arr)
        |end""".stripMargin
    )

    test(
      "def foo(**hash); end",
      """def foo(**hash)
        |end""".stripMargin
    )

    test(
      "def foo(*arr, **hash); end",
      """def foo(*arr,**hash)
        |end""".stripMargin
    )

    test(
      "def foo(x=1, y); end",
      """def foo(x=1,y)
        |end""".stripMargin
    )

    test(
      "def foo(x: 1); end",
      """def foo(x:1)
        |end""".stripMargin
    )

    test(
      "def foo(x:); end",
      """def foo(x:)
        |end""".stripMargin
    )

    test(
      "def foo(name:, surname:); end",
      """def foo(name:,surname:)
        |end""".stripMargin
    )
  }

  "multi-line method definition" in {
    test(
      """def foo
        | 1/0
        | rescue ZeroDivisionError => e
        |end
        |""".stripMargin,
      """def foo
        |1 / 0
        |rescue ZeroDivisionError => e
        |end""".stripMargin
    )
  }

  "endless method definition" in {
    test("def foo = x")
    test("def foo =\n x", "def foo = x")
    test("def foo = \"something\"")
    test("def id(x) = x")
  }

  "method def with proc params" in {
    test(
      """def foo(&block)
        |   yield
        |end
        |""".stripMargin,
      """def foo(&block)
        |yield
        |end""".stripMargin
    )

  }

  "method def for mandatory parameters" in {
    test(
      "def foo(bar:) end",
      """def foo(bar:)
        |end""".stripMargin
    )

    test(
      """
           |class SampleClass
           |  def sample_method (first_param:, second_param:)
           |  end
           |end
           |""".stripMargin,
      """class SampleClass
        |def <body>
        |
        |end
        |def sample_method (first_param:, second_param:)
        |  end
        |end""".stripMargin
    )
  }

  "fixme" ignore {
    // Initialize params / statements not being moved into the <body> method
    test("""
           |class SomeClass
           | def initialize(
           |   name, age)
           | end
           |end
           |""".stripMargin)

    // Initialize params / statements not being moved into the <body> method
    test("""
           |class SomeClass
           | def initialize(
           |             name: nil, age
           |             )
           | end
           |end
           |""".stripMargin)

  }
}
