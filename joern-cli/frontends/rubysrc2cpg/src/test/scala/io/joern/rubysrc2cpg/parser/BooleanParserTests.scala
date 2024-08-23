package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class BooleanParserTests extends RubyParserFixture with Matchers {
  "Boolean word operators" in {
    test("1 or 2")
    test("1 and 2")
    test("not 1")
    test("not 1 and 2")
    test("1 and not 2")
    test("1 or 2 or 3")
    test("1 and 2 and 3")
  }

  "Boolean sign operators" in {
    test("1 || 2")
    test("1 && 2")
    test("!1")
    test("!1 && 2")
    test("1 && !2")
    test("1 || 2 || 3")
    test("1 && 2 && 3")
    test("1 != 2")
    test("1 == [:b, :c]", "1 == [:b,:c]")
    test("! foo 1", "!foo 1")
  }

  "Spaceship" in {
    test("a <=> b")
  }
}
