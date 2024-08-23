package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class AssignmentParserTests extends RubyParserFixture with Matchers {
  "fixme" ignore {
    test("A.B ||= c 1")      // Possible string issue - result is missing A.B on LHS
    test("a[:b] ||= c 1, 2") // Possible string issue - result is missing a[:b] on LHS
    test("A::b += 1")        // Possible string issue - result is missing + in += operator
    test("A::B *= c d")      // Possible string issue - result is missing A::B *= on LHS
    test("A::b *= c d")      // Possible string issue - result is missing A::B *= on LHS
    test("a.b ||= c 1")      // Possible string issue - result is missing a.b ||= on LHS
    test("a = b, *c, d")     // Syntax error
    test("*, a = b")         // Syntax error
    test("*, x, y, z = f")   // Syntax error
  }

  "Single assignment" in {
    test("x=1", "x = 1")
    test("hash[:sym] = s[:sym]")
    test("a = 1, 2, 3, 4")
    test("a = b.c 1")
    test("a ||= b")
    test("a &&= b")
    test("a += 1")
    test("a /= 1")
    test("a[[1, 2]] = 3", "a[[1,2]] = 3")
    test("a[] += b")
    test("@a = 42")
    test("a&.b = 1", "a&.b= 1")
    test("c = a&.b")
  }

  "Multiple assignment" in {
    test("p, q = [foo(), bar()]")
    test("a, b::c = d")
    test("a, b.C = d")
    test("::A, ::B = 1, 2")
    test("[1,2,3,4][from..to] = [\"a\",\"b\",\"c\"]")
    test("a, = b.c 1")
    test("(a, b) = c.d")
    test("a ||= b.c 2")
    test("a, b, c, * = f")
    test("a, b, c, *s = f")
    test("*s, x, y, z = f")
    test("a = b 1 rescue 2")
  }

  "Destructured Assignment" in {
    test("a, b, c = 1, 2, 3")
    test("a, b, c, d = 1, 2, 3")
    test("a, b, *c = 1, 2, 3, 4")
    test("a, *b, c = 1, 2, 3")
    test("*a, b, c = 1, 2, 3, 4")
    test("a, b, c = 1, 2, *list")
    test("a, b, c = 1, *list")
  }

  "Class Constant Assign" in {
    test("A::b = 1")
    test("a.B = 1")
  }

  "Assignment with block" in {
    test(
      """h[k]=begin
        |42
        |end
        |""".stripMargin,
      """h[k] = begin
        |42
        |end""".stripMargin
    )
  }

  "Assignment with rescue" in {
    test("a = 1 rescue 2")
    test("a = b(1) rescue 2")
  }
}
