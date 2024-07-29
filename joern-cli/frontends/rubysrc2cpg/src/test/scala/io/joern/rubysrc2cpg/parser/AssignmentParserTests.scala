package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class AssignmentParserTests extends RubyParserFixture(newMatch = true) with Matchers {
  "fixme" ignore {
    test("a = 1, 2, 3, 4") // Going into SimpleCommand instead of an Assignment
  }

  "Single assignment" in {
    test("x=1", "x = 1")
  }

  "Multiple assignment" in {
    test("p, q = [foo(), bar()]")
  }

  // TODO: Implement Multiple Assignment
  "Destructured Assignment" in {
    test("a, b, c = 1, 2, 3")
    test("a, b, c, d = 1, 2, 3")
    test("a, b, *c = 1, 2, 3, 4")
    test("a, *b, c = 1, 2, 3")
    test("*a, b, c = 1, 2, 3, 4")
    test("a, b, c = 1, 2, *list")
    test("a, b, c = 1, *list")
  }
}
