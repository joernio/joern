package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class MethodDefinitionParserTests extends RubyParserFixture with Matchers {
  "fixme" ignore {
    test("""def a(...)
        |b(...)
        |end""".stripMargin) // Syntax error
  }

  "single line method definition" in {
    test(
      "def f(a=1, *b, c) end",
      """def f(a=1,*b,c)
        |end""".stripMargin
    )

    test(
      "def foo; end",
      """def foo
        |end""".stripMargin
    )

    test(
      """def foo arg = false
        |end""".stripMargin,
      """def foo(arg=false)
        |end""".stripMargin
    )

    test(
      "def foo(x); end",
      """def foo(x)
        |end""".stripMargin
    )

    test(
      "def f(a=nil, b) end",
      """def f(a=nil,b)
        |end""".stripMargin
    )

    test(
      "def f(a, b = :c, d) end",
      """def f(a,b=:c,d)
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

    test(
      "def f(*,a) end",
      """def f(*,a)
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

    test("""def x(y)
        |p(y)
        |y *= 2
        |return y
        |end""".stripMargin)

    test("""def test(**testing)
        |test_splat(**testing)
        |end""".stripMargin)

    test(
      """def fun(kw: :val)
        |kw
        |end""".stripMargin,
      """def fun(kw::val)
        |kw
        |end""".stripMargin
    )

    test(
      """def x a:, b:
        |end""".stripMargin,
      """def x(a:,b:)
        |end""".stripMargin
    )

    test(
      """def exec(cmd)
        |system(cmd)
        |rescue
        |nil
        |end""".stripMargin,
      """def exec(cmd)
        |system(cmd)
        |rescue
        |nil
        |end""".stripMargin
    )
  }

  "endless method definition" in {
    test("def foo = x")
    test("def foo =\n x", "def foo = x")
    test("def foo = \"something\"")
    test("def id(x) = x")
    test("def foo = bar 42")
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

  "method defs in classes" in {
    test(
      """
           |class SomeClass
           | def initialize(
           |   name, age)
           | end
           |end
           |""".stripMargin,
      """class SomeClass
        |def <body>
        |
        |end
        |def initialize(
        |   name, age)
        | end
        |end""".stripMargin
    )

    test(
      """
           |class SomeClass
           | def initialize(
           |             name: nil, age
           |             )
           | end
           |end
           |""".stripMargin,
      """class SomeClass
        |def <body>
        |
        |end
        |def initialize(
        |             name: nil, age
        |             )
        | end
        |end""".stripMargin
    )
  }

  "alias method" in {
    test("alias :start :on")
  }
}
