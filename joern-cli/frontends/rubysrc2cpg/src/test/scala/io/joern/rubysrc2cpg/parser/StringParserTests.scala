package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class StringParserTests extends RubyParserFixture with Matchers {
  "single quoted literal" in {
    test("''")
    test("'x' 'y'")
    test("""'x' \
        | 'y'
        |""".stripMargin)
    test("""'x' \
        | 'y' \
        | 'z'""".stripMargin)
  }

  "non expanded `%q` literal" in {
    test("%q()")
    test("%q[]")
    test("%q{}")
    test("%q<>")
    test("%q##")
    test("%q(x)")
    test("%q[x]")
    test("%q#x#")
    test("%q(\\()")
    test("%q[\\]]")
    test("%q#\\##")
    test("%q(foo)")
    test("%q( () )")
    test("%q( (\\)) )")
    test("%q< <\\>> >")
  }

  "expanded `%Q` literal" in {
    test("%Q()")
    test("%Q{text=#{1}}")
    test("%Q[#{1}#{2}]")
  }

  "expanded `%(` string literal" in {
    test("%()")
    test("%(text=#{1})")
    test("%(#{1}#{2})")
    test("puts %()")
  }

  "double quoted string literal" in {
    test("\"\"")
    test("\"x\" \"y\"")
    test("""
        |"x" \
        | "y"""".stripMargin)
  }

  "double quoted string interpolation" in {
    test("\"#{1}#{2}\"")
    test(""""#{10} \
        |  is a number."""".stripMargin)
  }

  "Expanded `%x` external command literal" in {
    test("%x//")
    test("%x{l#{'s'}}")
  }
}
