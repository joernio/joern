package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class AssignmentParserTests extends RubyParserFixture with Matchers {
  "Single assignment" in {
    test("x=1")
  }

  "Multiple assignment" in {
    test("p, q = [foo(), bar()]")
  }

  "Destructured Assignment" in {
    test("a, b, c = 1, 2, 3")
    test("a, b, c, d = 1, 2, 3")
    test("a, b, *c = 1, 2, 3, 4")
    test("a, *b, c = 1, 2, 3")
    test("*a, b, c = 1, 2, 3, 4")
    test("a = 1, 2, 3, 4")
    test("a, b, c = 1, 2, *list")
    test("a, b, c = 1, *list")
  }
}
