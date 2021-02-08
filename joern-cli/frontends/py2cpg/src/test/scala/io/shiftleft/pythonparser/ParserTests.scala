package io.shiftleft.pythonparser

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ParserTests extends AnyFreeSpec with Matchers {
  // code is parsed and the resulting AST is printed and compared against code.
  // expected is optional and if present the printed AST is compare against it
  // instead of code.
  def test(code: String, expected: String = null): Unit = {
    val ast = PyParser.parse(code)
    val compareTo = if (expected != null) expected else code

    val astPrinter = new AstPrinter("\t")
    astPrinter.print(ast) shouldBe compareTo
  }

  "statements" - {
    "return statement tests" in {
      test("return")
      test("return x")
      test("return x, y", "return (x,y)")
      test("return *x")
      test("return *x, *y", "return (*x,*y)")
    }

    "import statement tests" in {
      test("import x")
      test("import x, y")
      test("import x.y")
      test("import x.y, z.a")
      test("import x as y")
      test("import x as y, z as a")
      test("import x.y as z")
      test("from . import x")
      test("from .. import x")
      test("from ... import x")
      test("from .... import x")
      test("from x import y")
      test("from . x import y")
      test("from x import y, z")
      test("from x import (y, z)", "from x import y, z")
      test("from x import (y, z,)", "from x import y, z")
      test("from x import y as z")
      test("from x import (y as z, a as b)", "from x import y as z, a as b")
    }

    "raise statement tests" in {
      test("raise")
      test("raise x")
      test("raise x from y")
    }

    "pass statement tests" in {
      test("pass")
    }

    "del statement tests" in {
      //TODO
    }

    "yield statement tests" in {
      test("yield")
      test("yield x")
      test("yield x, y", "yield (x,y)")
      test("yield from x")
    }

    "assert statement tests" in {
      test("assert x")
      test("assert x, y")
    }

    "break statement tests" in {
      test("break")
    }

    "continue statement tests" in {
      test("continue")
    }

    "global statement tests" in {
      test("global x")
      test("global x, y")
    }

    "nonlocal statement tests" in {
      test("nonlocal x")
      test("nonlocal x, y")
    }

    "if statement tests" in {
      test("if x: y;", "if x:\n\ty")
      test("if x:\n\ty")
      test("if x:\n\ty\nelse:\n\tz")
      test("if x:\n\ty\nelif z:\n\ta")
    }

    "class def statement tests" in {
      test("class x():\n\tpass")
      test("class x(y):\n\tpass")
      test("class x(y, z):\n\tpass")
      test("class x(y = z):\n\tpass")
      test("class x(y, z = a):\n\tpass")
      test("@x\nclass y():\n\tpass")
      test("@x\n@y\nclass z():\n\tpass")
    }

    "try statement tests" in {
      test("try:\n\tpass\nexcept:\n\tpass")
      // TODO more try tests
    }

    "while statement tests" in {
      test("while x: y;", "while x:\n\ty")
      test("while x:\n\ty")
      test("while x:\n\twhile y:\n\t\tz")
      test("while x:\n\ty\nelse:\n\tz")
    }
  }

  "error recovery tests" in {
    test("<error>", "")
    test("x;<error>", "x")
    test("<error>;x", "x")
    test("x;<error>;y", "x\ny")
    test("x;<error>;y;<error>", "x\ny")
    test("x\n<error>", "x")
    test("<error>\nx", "x")
    test("x\n<error>\ny", "x\ny")
    test("x\n<error>\ny\n<error>", "x\ny")
  }

  "parser tests" in {
    test("x,y = z", "(x,y) = z")
    test("x if y else z")
    test("x or y")
    test("x and y")
    test("x and y or z")
  }

  "inversion rule tests" in {
    test("not x")
    test("not not x")
  }

  "comparison rule tests" in {
    test("x == y")
    test("x != y")
    test("x < y")
    test("x <= y")
    test("x > y")
    test("x >= y")
    test("x is y")
    test("x is not y")
    test("x in y")
    test("x not in y")
  }

  "bitwiseOr rule tests" in {
    test("x | y")
    test("x | y | z")
  }

  "bitwiseXor rule tests" in {
    test("x ^ y")
    test("x ^ y ^ z")
  }

  "bitwiseAnd rule tests" in {
    test("x & y")
    test("x & y & z")
  }

  "primary rule tests" in {
    test("x.y")
    test("x.y.z")
    test("func(x)")
    test("func(x, y)")
    test("func(*x)")
    test("func(*x, *y)")
    test("func(x, *y)")
    test("func(*x, y)")
    test("func(x := y)")
    test("func(x := y, z)")
    test("func(x, y := z)")
    test("func(x,)", "func(x)")
    test("func(x = y)")
    test("func(x = y, z = a)")
    test("func(**x)")
    test("func(**x, **y)")
    test("func(x, y = z, **a)")
    test("obj[x]")
    test("obj[x, y]", "obj[(x,y)]")
    test("obj[:]")
    test("obj[::]", "obj[:]")
    test("obj[x::]", "obj[x:]")
    test("obj[:y:]", "obj[:y]")
    test("obj[::z]")
    test("obj[x:y:]", "obj[x:y]")
    test("obj[:y:z]")
    test("obj[x:y:z]")
  }

  "atom rule tests" in {
    test("x")
    test("True")
    test("False")
    test("None")
    test("123")
    test("...")
  }

  "extra" in {
  }
}