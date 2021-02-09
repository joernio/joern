package io.shiftleft.pythonparser

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ParserTests extends AnyFreeSpec with Matchers {
  // code is parsed and the resulting AST is printed and compared against code.
  // expected is optional and if present the printed AST is compare against it
  // instead of code.
  def test(code: String, expected: String, indentStr: String): Unit = {
    val ast = PyParser.parse(code)
    val compareTo = if (expected != null) expected else code

    val astPrinter = new AstPrinter(indentStr)
    astPrinter.print(ast) shouldBe compareTo
  }

  def testS(code: String, expected: String = null): Unit = {
    test(code, expected, "  ")
  }

  def testT(code: String, expected: String = null): Unit = {
    test(code, expected, "\t")
  }

  "statements" - {
    "return statement tests" in {
      testT("return")
      testT("return x")
      testT("return x, y", "return (x,y)")
      testT("return *x")
      testT("return *x, *y", "return (*x,*y)")
    }

    "import statement tests" in {
      testT("import x")
      testT("import x, y")
      testT("import x.y")
      testT("import x.y, z.a")
      testT("import x as y")
      testT("import x as y, z as a")
      testT("import x.y as z")
      testT("from . import x")
      testT("from .. import x")
      testT("from ... import x")
      testT("from .... import x")
      testT("from x import y")
      testT("from . x import y")
      testT("from x import y, z")
      testT("from x import (y, z)", "from x import y, z")
      testT("from x import (y, z,)", "from x import y, z")
      testT("from x import y as z")
      testT("from x import (y as z, a as b)", "from x import y as z, a as b")
    }

    "raise statement tests" in {
      testT("raise")
      testT("raise x")
      testT("raise x from y")
    }

    "pass statement tests" in {
      testT("pass")
    }

    "del statement tests" in {
      //TODO
    }

    "yield statement tests" in {
      testT("yield")
      testT("yield x")
      testT("yield x, y", "yield (x,y)")
      testT("yield from x")
    }

    "assert statement tests" in {
      testT("assert x")
      testT("assert x, y")
    }

    "break statement tests" in {
      testT("break")
    }

    "continue statement tests" in {
      testT("continue")
    }

    "global statement tests" in {
      testT("global x")
      testT("global x, y")
    }

    "nonlocal statement tests" in {
      testT("nonlocal x")
      testT("nonlocal x, y")
    }

    "function def statement tests" in {
      testT("def func():\n\tpass")
      testT("def func(x):\n\tpass")
      testT("def func(x,):\n\tpass", "def func(x):\n\tpass")
      testT("def func(x = 1):\n\tpass")
      testT("def func(x = 1,):\n\tpass", "def func(x = 1):\n\tpass")
      testT("def func(x, y):\n\tpass")
      testT("def func(x, y = 2):\n\tpass")
      testT("def func(x = 1, y = 2):\n\tpass")
      testT("def func(x, y,):\n\tpass", "def func(x, y):\n\tpass")
      testT("def func(x, /, y):\n\tpass")
      testT("def func(x, /):\n\tpass")
      testT("def func(x, /,):\n\tpass", "def func(x, /):\n\tpass")
      testT("def func(x, *y):\n\tpass")
      testT("def func(x, *y, z):\n\tpass")
      testT("def func(x, *y, z, **a):\n\tpass")
      testT("def func(x, *y, **z):\n\tpass")
      testT("def func(x, **y):\n\tpass")

      testT("def func(*x):\n\tpass")
      testT("def func(*x,):\n\tpass", "def func(*x):\n\tpass")
      testT("def func(*x, y):\n\tpass")
      testT("def func(*x, y, ):\n\tpass", "def func(*x, y):\n\tpass")
      testT("def func(*x, y = 1):\n\tpass")
      testT("def func(*x, y, z):\n\tpass")
      testT("def func(*x, y = 1, z = 2):\n\tpass")
      testT("def func(*x, y = 1, z):\n\tpass")
      testT("def func(*x, y, z = 2):\n\tpass")
      testT("def func(*x, y, **z):\n\tpass")
      testT("def func(*x, **z):\n\tpass")
      testT("def func(*, x):\n\tpass")
      testT("def func(*, x, ):\n\tpass", "def func(*, x):\n\tpass")
      testT("def func(*, x, **y):\n\tpass")
      testT("def func(**x):\n\tpass")
      testT("def func(**x, ):\n\tpass", "def func(**x):\n\tpass")

      testT("@x\ndef func():\n\tpass")
      testT("@x\n@y\ndef func():\n\tpass")
    }

    "if statement tests" in {
      testT("if x: y;", "if x:\n\ty")
      testT("if x:\n\ty")
      testT("if x:\n\ty\nelse:\n\tz")
      testT("if x:\n\ty\nelif z:\n\ta")
    }

    "class def statement tests" in {
      testT("class x():\n\tpass")
      testT("class x(y):\n\tpass")
      testT("class x(y, z):\n\tpass")
      testT("class x(y = z):\n\tpass")
      testT("class x(y, z = a):\n\tpass")
      testT("@x\nclass y():\n\tpass")
      testT("@x\n@y\nclass z():\n\tpass")
    }

    "try statement tests" in {
      testS(
        """try:
          |  x
          |except:
          |  y""".stripMargin)
      testS(
        """try:
          |  x
          |except e:
          |  y""".stripMargin)
      testS(
        """try:
          |  x
          |except e as f:
          |  y""".stripMargin)
      testS(
        """try:
          |  x
          |except e as f:
          |  y
          |except g as h:
          |  z""".stripMargin)
      testS(
        """try:
          |  x
          |finally:
          |  y""".stripMargin)
      testS(
        """try:
          |  x
          |except e as f:
          |  y
          |else:
          |  z
          |finally:
          |  a""".stripMargin)
    }

    "while statement tests" in {
      testT("while x: y;", "while x:\n\ty")
      testT("while x:\n\ty")
      testT("while x:\n\twhile y:\n\t\tz")
      testT("while x:\n\ty\nelse:\n\tz")
    }
  }

  "error recovery tests" in {
    testT("<error>", "")
    testT("x;<error>", "x")
    testT("<error>;x", "x")
    testT("x;<error>;y", "x\ny")
    testT("x;<error>;y;<error>", "x\ny")
    testT("x\n<error>", "x")
    testT("<error>\nx", "x")
    testT("x\n<error>\ny", "x\ny")
    testT("x\n<error>\ny\n<error>", "x\ny")
  }

  "parser tests" in {
    testT("x,y = z", "(x,y) = z")
    testT("x if y else z")
    testT("x or y")
    testT("x and y")
    testT("x and y or z")
  }

  "inversion rule tests" in {
    testT("not x")
    testT("not not x")
  }

  "comparison rule tests" in {
    testT("x == y")
    testT("x != y")
    testT("x < y")
    testT("x <= y")
    testT("x > y")
    testT("x >= y")
    testT("x is y")
    testT("x is not y")
    testT("x in y")
    testT("x not in y")
  }

  "bitwiseOr rule tests" in {
    testT("x | y")
    testT("x | y | z")
  }

  "bitwiseXor rule tests" in {
    testT("x ^ y")
    testT("x ^ y ^ z")
  }

  "bitwiseAnd rule tests" in {
    testT("x & y")
    testT("x & y & z")
  }

  "shiftExpr rule tests" in {
    testT("x << y")
    testT("x >> y")
    testT("x << y << z")
    testT("x >> y >> z")
  }

  "sum rule tests" in {
    testT("x + y")
    testT("x - y")
    testT("x + y + z")
    testT("x - y - z")
  }

  "term rule tests" in {
    testT("x * y")
    testT("x / y")
    testT("x // y")
    testT("x % y")
    testT("x @ y")
    testT("x * y / z")
  }

  "factor rule tests" in {
    testT("+x")
    testT("-x")
    testT("~x")
    testT("+-x")
  }

  "power rule tests" in {
    testT("x ** y")
    testT("x ** -y")
  }

  "await primary rule tests" in {
    testT("await x")
  }

  "primary rule tests" in {
    testT("x.y")
    testT("x.y.z")
    testT("func(x)")
    testT("func(x, y)")
    testT("func(*x)")
    testT("func(*x, *y)")
    testT("func(x, *y)")
    testT("func(*x, y)")
    testT("func(x := y)")
    testT("func(x := y, z)")
    testT("func(x, y := z)")
    testT("func(x,)", "func(x)")
    testT("func(x = y)")
    testT("func(x = y, z = a)")
    testT("func(**x)")
    testT("func(**x, **y)")
    testT("func(x, y = z, **a)")
    testT("obj[x]")
    testT("obj[x, y]", "obj[(x,y)]")
    testT("obj[:]")
    testT("obj[::]", "obj[:]")
    testT("obj[x::]", "obj[x:]")
    testT("obj[:y:]", "obj[:y]")
    testT("obj[::z]")
    testT("obj[x:y:]", "obj[x:y]")
    testT("obj[:y:z]")
    testT("obj[x:y:z]")
  }

  "atom rule tests" in {
    testT("x")
    testT("True")
    testT("False")
    testT("None")
    testT("123")
    testT("...")
  }

  "extra" in {
  }
}