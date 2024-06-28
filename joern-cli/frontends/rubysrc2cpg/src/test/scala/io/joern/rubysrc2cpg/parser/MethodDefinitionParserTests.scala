package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class MethodDefinitionParserTests extends RubyParserFixture with Matchers {
  "single line method definition" in {
    test("def foo; end")
    test("def foo(x); end")
    test("def foo(x=1); end")
    test("def foo(x, &y); end")
    test("def foo(*arr); end")
    test("def foo(**hash); end")
    test("def foo(*arr, **hash); end")
    test("def foo(x=1, y); end")
    test("def foo(x: 1); end")
    test("def foo(x:); end")
    test("def foo(name:, surname:); end")
  }

  "multi-line method definition" in {
    test("""def foo
        | 1/0
        | rescue ZeroDivisionError => e
        |end
        |""".stripMargin)
  }

  "endless method definition" in {
    test("def foo = x")
    test("def foo =\n x")
    test("def foo = \"something\"")
    test("def id(x) = x")
  }

  "method def with proc params" in {
    test("""def foo(&block)
        |   yield
        |end
        |""".stripMargin)

  }

  "method def for mandatory parameters" in {
    test("def foo(bar:) end")

    test("""
        |class SampleClass
        |  def sample_method (first_param:, second_param:)
        |  end
        |end
        |""".stripMargin)

    test("""
        |class SomeClass
        | def initialize(
        |   name, age)
        | end
        |end
        |""".stripMargin)

    test("""
        |class SomeClass
        | def initialize(
        |       name, age
        |       )
        | end
        |end
        |""".stripMargin)

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
