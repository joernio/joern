package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class AssignmentParserTests extends RubyParserFixture with Matchers {
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
    test("a.b ||= c 1", "if a.b.nil? then a.b = c 1 end")
    test("A.B ||= c 1", "if A.B.nil? then A.B = c 1 end")
    test("A::b += 1")
    test("A::b *= c d")
    test("a[:b] ||= c 1, 2", "if a[:b].nil? then a[:b] = c 1, 2 end")
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
    test("*, a = b")
    test("*, x, y, z = f")
  }

  "Destructured Assignment" in {
    test("a, b, c = 1, 2, 3")
    test("a, b, c, d = 1, 2, 3")
    test("a, b, *c = 1, 2, 3, 4")
    test("a, *b, c = 1, 2, 3")
    test("*a, b, c = 1, 2, 3, 4")
    test("a, b, c = 1, 2, *list")
    test("a, b, c = 1, *list")
    test("a = b, *c, d")
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
