package io.joern.pythonparser

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ParserTests extends AnyFreeSpec with Matchers {

  // code is parsed and the resulting AST is printed and compared against code.
  // expected is optional and if present the printed AST is compare against it
  // instead of code.
  def test(code: String, expected: String, indentStr: String): Unit = {
    val ast       = new PyParser().parse(code)
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
      // Python2 style raise syntax.
      testT("raise x, y")
      testT("raise x, y, z")
    }

    "pass statement tests" in {
      testT("pass")
    }

    "del statement tests" in {
      testT("del x")
      testT("del x,", "del x")
      testT("del x, y")
      testT("del x, y,", "del x, y")
      testT("del x.y")
      testT("del x().y")
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
      testT("def func(x,):\n\tpass", s"def func(x):\n\tpass")
      testT("def func(x = 1):\n\tpass")
      testT("def func(x = 1,):\n\tpass", s"def func(x = 1):\n\tpass")
      testT("def func(x, y):\n\tpass")
      testT("def func(x, y = 2):\n\tpass")
      testT("def func(x = 1, y = 2):\n\tpass")
      testT("def func(x, y,):\n\tpass", s"def func(x, y):\n\tpass")
      testT("def func(x, /, y):\n\tpass")
      testT("def func(x, /):\n\tpass")
      testT("def func(x, /,):\n\tpass", s"def func(x, /):\n\tpass")
      testT("def func(x, *y):\n\tpass")
      testT("def func(x, *y, z):\n\tpass")
      testT("def func(x, *y, z, **a):\n\tpass")
      testT("def func(x, *y, **z):\n\tpass")
      testT("def func(x, **y):\n\tpass")

      testT("def func(*x):\n\tpass")
      testT("def func(*x,):\n\tpass", s"def func(*x):\n\tpass")
      testT("def func(*x, y):\n\tpass")
      testT("def func(*x, y, ):\n\tpass", s"def func(*x, y):\n\tpass")
      testT("def func(*x, y = 1):\n\tpass")
      testT("def func(*x, y, z):\n\tpass")
      testT("def func(*x, y = 1, z = 2):\n\tpass")
      testT("def func(*x, y = 1, z):\n\tpass")
      testT("def func(*x, y, z = 2):\n\tpass")
      testT("def func(*x, y, **z):\n\tpass")
      testT("def func(*x, **z):\n\tpass")
      testT("def func(*, x):\n\tpass")
      testT("def func(*, x, ):\n\tpass", s"def func(*, x):\n\tpass")
      testT("def func(*, x, **y):\n\tpass")
      testT("def func(**x):\n\tpass")
      testT("def func(**x, ):\n\tpass", s"def func(**x):\n\tpass")

      testT("def func(x: y):\n\tpass")
      testT("def func(x: y = z):\n\tpass")
      testT("def func() -> x:\n\tpass")

      testT("@x\ndef func():\n\tpass")
      testT("@x\n@y\ndef func():\n\tpass")

      testT("async def func():\n\tpass")
      testT("@x\nasync def func():\n\tpass")
    }

    "if statement tests" in {
      testT("if x: y;", s"if x:\n\ty")
      testT("if x:\n\ty")
      testT("if x:\n\ty\nelse:\n\tz")
      testT("if x:\n\ty\nelif z:\n\ta")
    }

    "class def statement tests" in {
      testT("class x:\n\tpass", s"class x():\n\tpass")
      testT("class x():\n\tpass")
      testT("class x(y):\n\tpass")
      testT("class x(y, z):\n\tpass")
      testT("class x(y = z):\n\tpass")
      testT("class x(y, z = a):\n\tpass")
      testT("@x\nclass y():\n\tpass")
      testT("@x\n@y\nclass z():\n\tpass")
    }

    "try statement tests" in {
      testS("""try:
          |  x
          |except:
          |  y""".stripMargin)
      testS("""try:
          |  x
          |except e:
          |  y""".stripMargin)
      testS("""try:
          |  x
          |except e as f:
          |  y""".stripMargin)
      testS("""try:
          |  x
          |except e as f:
          |  y
          |except g as h:
          |  z""".stripMargin)
      testS("""try:
          |  x
          |finally:
          |  y""".stripMargin)
      testS("""try:
          |  x
          |except e as f:
          |  y
          |else:
          |  z
          |finally:
          |  a""".stripMargin)
      // Python2 style COMMA syntax.
      testS(
        """try:
          |  x
          |except e, f:
          |  y""".stripMargin,
        """try:
          |  x
          |except e as f:
          |  y""".stripMargin
      )
    }

    "while statement tests" in {
      testT("while x: y;", s"while x:\n\ty")
      testT("while x:\n\ty")
      testT("while x:\n\twhile y:\n\t\tz")
      testT("while x:\n\ty\nelse:\n\tz")
    }

    "with statement tests" in {
      testT("with x:\n\tpass")
      testT("with x, :\n\tpass", s"with x:\n\tpass")
      testT("with x, y:\n\tpass")
      testT("with x, y, z:\n\tpass")
      testT("with (x):\n\tpass", s"with x:\n\tpass")
      testT("with (x,):\n\tpass", s"with x:\n\tpass")
      testT("with (x, y):\n\tpass", s"with x, y:\n\tpass")
      testT("with (x, y, z):\n\tpass", s"with x, y, z:\n\tpass")
      testT("with x + 1:\n\tpass")
      testT("with x + 1, y + 2:\n\tpass")
      testT("with (x + 1):\n\tpass", s"with x + 1:\n\tpass")
      testT("with (x + 1, y + 2):\n\tpass", s"with x + 1, y + 2:\n\tpass")
      testT("with x as y:\n\tpass")
      testT("with x as *y:\n\tpass")
      testT("with x as y, z:\n\tpass")
      testT("with x as y, z as a:\n\tpass")
      testT("with x as y, z as a,:\n\tpass", s"with x as y, z as a:\n\tpass")
    }

    "for statement tests" in {
      testT("for x in l:\n\tpass")
      testT("for x, in l:\n\tpass", s"for (x,) in l:\n\tpass")
      testT("for x, y in l:\n\tpass", s"for (x,y) in l:\n\tpass")
      testT("for x, y, in l:\n\tpass", s"for (x,y) in l:\n\tpass")
      testT("for *x in l:\n\tpass")
      testT("for x.y in l:\n\tpass")
      testT("for x in l,:\n\tpass", s"for x in (l,):\n\tpass")
      testT("for x in *l:\n\tpass")
      testT("for x in l.m:\n\tpass")
      testT("for x in l, m:\n\tpass", s"for x in (l,m):\n\tpass")
      testT("for x in l, m,:\n\tpass", s"for x in (l,m):\n\tpass")
      // TODO test with parenthesized target an iter
    }

    "assign statement tests" in {
      testT("x = 1")
      testT("x = y = 1")
      testT("x = y = z = 1")
      testT("x, = 1", "(x,) = 1")
      testT("x,y = 1", "(x,y) = 1")
      testT("*x = 1")
      testT("*x, *y = 1", "(*x,*y) = 1")
      testT("x = yield y")
      testT("x = y, z = 1", "x = (y,z) = 1")
    }

    "annotated assign statement tests" in {
      testT("x: y")
      testT("x: y = 1")
      testT("x: y = *z")
      testT("x: y = yield z")
      testT("x, y: z = 1", "(x,y): z = 1")
    }

    "augmented assign statement tests" in {
      testT("x += 1")
      testT("x += *y")
      testT("x += yield y")

      testT("x -= 1")
      testT("x *= 1")
      testT("x @= 1")
      testT("x /= 1")
      testT("x %= 1")
      testT("x &= 1")
      testT("x |= 1")
      testT("x ^= 1")
      testT("x <<= 1")
      testT("x >>= 1")
      testT("x **= 1")
      testT("x //= 1")
    }
  }

  "error recovery tests" in {
    testT("<someErr>", "<error>")
    testT("x;<someErr>", s"x\n<error>")
    testT("x;<someErr>;", s"x\n<error>")
    testT("<someErr>;x", s"<error>\nx")
    testT("<someErr>;x;", s"<error>\nx")
    testT("x;<someErr>;y", s"x\n<error>\ny")
    testT("x;<someErr>;y;<someErr>", s"x\n<error>\ny\n<error>")
    testT("x\n<someErr>", s"x\n<error>")
    testT("<someErr>\nx", s"<error>\nx")
    testT("x\n<someErr>\ny", s"x\n<error>\ny")
    testT("x\n<someErr>\ny\n<someErr>", s"x\n<error>\ny\n<error>")
    testT("print x = y", "<error>")
  }

  "parser tests" in {
    testT("x,y = z", "(x,y) = z")
    testT("x if y else z")
    testT("x or y")
    testT("x and y")
    testT("x and y or z")
  }

  "comment tests" in {
    testT("#comment", "")
    testT("x#comment", "x")
    testT("x#comment\ny", s"x\ny")
    testT("x#comment\ny#comment\nz", s"x\ny\nz")
    testT("x\n#comment", "x")
    testT("x\n  #comment", "x")
    testT("x\n  #comment\ny", s"x\ny")
    testT("#\u2265", "")
  }

  "indentation tests" in {
    testS("""if True:
        |  x
        |  if True:
        |    y
        |z""".stripMargin)
    testT("if True:\n\t x\n \t y", s"if True:\n\tx\n\ty")
    testS(
      """if True:
        |  z = (x
        |,y)
        |a""".stripMargin,
      """if True:
        |  z = (x,y)
        |a""".stripMargin
    )
  }

  "explicit line joining tests" in {
    testS(
      """if True:
        |  z = x + \
        |y
        |  a""".stripMargin,
      """if True:
        |  z = x + y
        |  a""".stripMargin
    )
  }

  "implicit line joining tests" in {
    testS(
      """if True:
        |  z = (x
        |,y)
        |  a""".stripMargin,
      """if True:
        |  z = (x,y)
        |  a""".stripMargin
    )
    testS(
      """if True:
        |  z = {x
        |,y}
        |  a""".stripMargin,
      """if True:
        |  z = {x, y}
        |  a""".stripMargin
    )
    testS(
      """if True:
        |  z = [x
        |,y]
        |  a""".stripMargin,
      """if True:
        |  z = [x, y]
        |  a""".stripMargin
    )
    testS(
      """if True:
        |  z = [(x
        |,y)
        |   ]
        |  a""".stripMargin,
      """if True:
        |  z = [(x,y)]
        |  a""".stripMargin
    )
  }

  "blank line tests" in {
    testS(
      """if True:
        |  z = x + y
        |
        |  a""".stripMargin,
      """if True:
        |  z = x + y
        |  a""".stripMargin
    )
  }

  "multi character new line tests" in {
    testT("if True:\n\r\tx", s"if True:\n\tx")
    testT("if True:\r\n\tx", s"if True:\n\tx")
    testT("if True:\n\n\r\tx", s"if True:\n\tx")
    testT("if True:\n\r\n\tx", s"if True:\n\tx")
    testT("if True:\n\t(x,\n\ry)", s"if True:\n\t(x,y)")
    testT("if True:\n\t(x,\r\ny)", s"if True:\n\t(x,y)")
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
    testT("func(x for x in y)")
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
    testT("obj[x,y]", "obj[(x,y)]")
    testT("obj[x,]", "obj[(x,)]")
  }

  "atom rule tests" in {
    testT("x")
    testT("True")
    testT("False")
    testT("None")
    testT("123")
    testT("...")
  }

  "lambda rule tests" in {
    testT("lambda: e")
    testT("lambda x: e")
    testT("lambda x,: e", "lambda x: e")
    testT("lambda x = 1: e")
    testT("lambda x = 1,: e", "lambda x = 1: e")
    testT("lambda x, y: e")
    testT("lambda x, y = 2: e")
    testT("lambda x = 1, y = 2: e")
    testT("lambda x, y,: e", "lambda x, y: e")
    testT("lambda x, /, y: e")
    testT("lambda x, /: e")
    testT("lambda x, /,: e", "lambda x, /: e")
    testT("lambda x, *y: e")
    testT("lambda x, *y, z: e")
    testT("lambda x, *y, z, **a: e")
    testT("lambda x, *y, **z: e")
    testT("lambda x, **y: e")

    testT("lambda *x: e")
    testT("lambda *x,: e", "lambda *x: e")
    testT("lambda *x, y: e")
    testT("lambda *x, y, : e", "lambda *x, y: e")
    testT("lambda *x, y = 1: e")
    testT("lambda *x, y, z: e")
    testT("lambda *x, y = 1, z = 2: e")
    testT("lambda *x, y = 1, z: e")
    testT("lambda *x, y, z = 2: e")
    testT("lambda *x, y, **z: e")
    testT("lambda *x, **z: e")
    testT("lambda *, x: e")
    testT("lambda *, x, : e", "lambda *, x: e")
    testT("lambda *, x, **y: e")
    testT("lambda **x: e")
    testT("lambda **x, : e", "lambda **x: e")
  }

  "listOrListComprehension rule tests" in {
    testT("[]")
    testT("[x]")
    testT("[x, y]")
    testT("[x, y, z]")
    testT("[x for x in y]")
    testT("[x for x in y if z]")
    testT("[x for x in y if z if a]")
    testT("[x for x in y if z if a if b]")
    testT("[x for x in y if z if a if b]")
    testT("[x for x in y for y in z]")
    testT("[x for x in y if z for y in a]")
  }

  "tupleOrGeneratorExpOrGroup rule tests" in {
    testT("()")
    testT("(x,)")
    testT("(x,y)")
    testT("(x,y,)", "(x,y)")
    testT("(x :=y )", "x := y")
    testT("(*x)", "*x")
    testT("(yield x)", "yield x")
    testT("(x for x in y)")
    testT("(x for x in y if z)")
    testT("(x for x in y for y in z)")
  }

  "setOrDictOrSetCompOrDictComp rule tests" in {
    testT("{}")
    testT("{x}")
    testT("{x,}", "{x}")
    testT("{x, y}")
    testT("{x, y,}", "{x, y}")
    testT("{x, y, z}")
    testT("{x for x in y}")
    testT("{x for x in y if z}")
    testT("{x for x in y for y in z}")
    testT("{x:1}")
    testT("{x:1,}", "{x:1}")
    testT("{x:1, y:2}")
    testT("{x:1, y:2,}", "{x:1, y:2}")
    testT("{**x}")
    testT("{**x, **y}")
    testT("{**x, y:1}")
    testT("{**x, y:1, **z}")
    testT("{**x, y:1, **z, a:2}")
    testT("{x:1, **y, z:2, **a}")
    testT("{x:1, **y}")
    testT("{x:1, **y, z:2}")
    testT("{x:1, **y, z:2, **a}")
    testT("{x:1, **y, z:2, **a}")
    testT("{x:y for (x,y) in z}")
    testT("{x:y for x,y in z}", "{x:y for (x,y) in z}")
  }

  "string literal tests" in {
    testT("\"abc\"")
    testT("r\"abc\"")
    testT("u\"abc\"")
    testT("b\"abc\"")
    testT("rb\"abc\"")

    testT("'abc'")
    testT("r'abc'")
    testT("u'abc'")
    testT("b'abc'")
    testT("rb'abc'")

    testT("\"\"\"abc\"\"\"")
    testT("r\"\"\"abc\"\"\"")
    testT("u\"\"\"abc\"\"\"")
    testT("b\"\"\"abc\"\"\"")
    testT("rb\"\"\"abc\"\"\"")

    testT("'''abc'''")
    testT("r'''abc'''")
    testT("u'''abc'''")
    testT("b'''abc'''")
    testT("rb'''abc'''")

    testT("'abc' 'def' \"ghi\"")
  }

  "format string tests" in {
    testT("f\"{x}\"")
    testT("f\"{x,}\"", "f\"{(x,)}\"")
    testT("f\"{*x}\"")
    testT("f\"{yield x}\"")
    testT("f\"{x}{y}\"")
    testT("f\"pre{x}post\"")
    testT("f\"pre{x}mid{y}post\"")
    testT("f\"{{x}}\"")
    testT("f\"\\{x}\"")
    testT("f\"{x=}\"")
    testT("f\"{x!s}\"")
    testT("f\"{x!r}\"")
    testT("f\"{x!a}\"")
    testT("f\"{x=!s}\"")
    testT("f\"{x:1}\"")
    testT("f\"{x:{y}}\"")
    testT("f\"{x:{y}{z}}\"")
    testT("f\"{x=:1}\"")
    testT("f\"{x=!s:1}\"")
    testT("rf\"{x=!s:1}\"")

    testT("f\"a\"")
    testT("f'{a}'")
    testT("f'a'")
    testT("f\"\"\"{a}\"\"\"")
    testT("f\"\"\"a\"\"\"")
    testT("f'''{a}'''")
    testT("f'''a'''")
  }

  "format string tests with context" in {
    testS("""func(f"x: {y}")""".stripMargin)
  }

  // Check that an escaped string terminal character does not break
  // tokenization.
  "string literal terminal escape tests" in {
    testT("\"\\\"\"")
    testT("'\\''")
    testT("\"\"\"\\\"\"\"\"")
    testT("'''\\''''")
  }

  "string literal escape tests" in {
    testT("\"\\\\\"")
    testT("'\\\\'")
    testT("\"\"\"\\\\\"\"\"")
    testT("'''\\\\'''")
    testT("'\\ufoo'")
  }

  "integer literal tests" in {
    testT("000")
    testT("01")
    testT("1")
    testT("0b1")
    testT("0B1")
    testT("0b1_1")
    testT("0o1")
    testT("0O1")
    testT("0o1_1")
    testT("0x1")
    testT("0X1")
    testT("0x1_1")
  }

  "float literal tests" in {
    testT("1.")
    testT("1.1")
    testT("1.1e1")
    testT(".1")
    testT(".1e1")
    testT("1e1")
    testT("1e+1")
    testT("1e-1")
  }

  "imaginary literal tests" in {
    testT("1j")
    testT("1_1j")
    testT("1.j")
    testT("1.1j")
    testT("1.1e1j")
    testT(".1j")
    testT(".1e1j")
    testT("1e1j")
    testT("1e+1j")
    testT("1e-1j")
  }

  "empty input test" in {
    testT("")
  }

  "python2 print statement tests" in {
    testT("print x", "print(x)")
    testT("print x; print y", s"print(x)\nprint(y)")
    testT("print x\nprint y", s"print(x)\nprint(y)")
    testT("valid1;print x;valid2", s"valid1\nprint(x)\nvalid2")
    testT("valid1\nprint x;valid2", s"valid1\nprint(x)\nvalid2")
    testT("valid1\nprint x\nvalid2", s"valid1\nprint(x)\nvalid2")
    testT("valid1;print x\nvalid2", s"valid1\nprint(x)\nvalid2")
  }

  // These print statements are valid in python2 and python3
  // but have different semantics in the respective versions.
  // We favor the python2 interpretation.
  "ambigious print statement tests" in {
    testT("print (x), y", "print(x, y)")
    testT("print (x, y), z", "print(x, y, z)")
    testT("print (x, y), z, a", "print(x, y, z, a)")
  }

  "python2 exec statement tests" in {
    testT("exec x", "exec(x)")
    testT("exec x in y", "exec(x, y)")
    testT("exec x in y, z", "exec(x, y, z)")
    testT("exec x; pass", s"exec(x)\npass")
    testT("exec x;\npass", s"exec(x)\npass")
  }

  "pattern matching subject tests" in {
    testS("""match x:
        |  case _:
        |    pass""".stripMargin)
    testS(
      """match x,:
        |  case _:
        |    pass""".stripMargin,
      """match (x,):
        |  case _:
        |    pass""".stripMargin
    )
    testS(
      """match x,y,z:
        |  case _:
        |    pass""".stripMargin,
      """match (x,y,z):
        |  case _:
        |    pass""".stripMargin
    )
    testS(
      """match x,y,z,:
        |  case _:
        |    pass""".stripMargin,
      """match (x,y,z):
        |  case _:
        |    pass""".stripMargin
    )
    testS(
      """match *x,:
        |  case _:
        |    pass""".stripMargin,
      """match (*x,):
        |  case _:
        |    pass""".stripMargin
    )
    testS(
      """match *x,y:
        |  case _:
        |    pass""".stripMargin,
      """match (*x,y):
        |  case _:
        |    pass""".stripMargin
    )
    testS(
      """match x,*y:
        |  case _:
        |    pass""".stripMargin,
      """match (x,*y):
        |  case _:
        |    pass""".stripMargin
    )
    testS("""match a := b:
        |  case _:
        |    pass""".stripMargin)
  }

  "pattern matching case tests - literal" in {
    testS("""match x:
        |  case 1:
        |    pass""".stripMargin)
    testS("""match x:
        |  case -1:
        |    pass""".stripMargin)
    testS("""match x:
        |  case 1.0 + 1j:
        |    pass""".stripMargin)
    testS("""match x:
        |  case 1.0 - 1j:
        |    pass""".stripMargin)
    testS("""match x:
        |  case 'abc':
        |    pass""".stripMargin)
    testS("""match x:
        |  case 'abc' 'def':
        |    pass""".stripMargin)
    testS("""match x:
        |  case None:
        |    pass""".stripMargin)
    testS("""match x:
        |  case True:
        |    pass""".stripMargin)
    testS("""match x:
        |  case False:
        |    pass""".stripMargin)
  }

  "pattern matching case tests - capture" in {
    testS("""match x:
        |  case y:
        |    pass""".stripMargin)
  }

  "pattern matching case tests - wildcard" in {
    testS("""match x:
        |  case _:
        |    pass""".stripMargin)
  }

  "pattern matching case tests - value" in {
    testS("""match x:
        |  case a.b:
        |    pass""".stripMargin)
  }

  "pattern matching case tests - group" in {
    testS(
      """match x:
        |  case (a):
        |    pass""".stripMargin,
      """match x:
        |  case a:
        |    pass""".stripMargin
    )
  }

  "pattern matching case tests - sequence" in {
    testS(
      """match x:
        |  case a,:
        |    pass""".stripMargin,
      """match x:
        |  case [a]:
        |    pass""".stripMargin
    )
    testS(
      """match x:
        |  case a, b:
        |    pass""".stripMargin,
      """match x:
        |  case [a, b]:
        |    pass""".stripMargin
    )
    testS("""match x:
        |  case []:
        |    pass""".stripMargin)
    testS("""match x:
        |  case [a]:
        |    pass""".stripMargin)
    testS(
      """match x:
        |  case [a,]:
        |    pass""".stripMargin,
      """match x:
        |  case [a]:
        |    pass""".stripMargin
    )
    testS("""match x:
        |  case [a, b, c]:
        |    pass""".stripMargin)
    testS(
      """match x:
        |  case [a, b, c,]:
        |    pass""".stripMargin,
      """match x:
        |  case [a, b, c]:
        |    pass""".stripMargin
    )
    testS(
      """match x:
        |  case ():
        |    pass""".stripMargin,
      """match x:
        |  case []:
        |    pass""".stripMargin
    )
    testS(
      """match x:
        |  case (a,):
        |    pass""".stripMargin,
      """match x:
        |  case [a]:
        |    pass""".stripMargin
    )
    testS(
      """match x:
        |  case (a, b, c):
        |    pass""".stripMargin,
      """match x:
        |  case [a, b, c]:
        |    pass""".stripMargin
    )
    testS(
      """match x:
        |  case (a, b, c,):
        |    pass""".stripMargin,
      """match x:
        |  case [a, b, c]:
        |    pass""".stripMargin
    )
  }

  "pattern matching case tests - mapping" in {
    testS("""match x:
        |  case {}:
        |    pass""".stripMargin)
    testS("""match x:
        |  case {**y}:
        |    pass""".stripMargin)
    testS("""match x:
        |  case {1: a}:
        |    pass""".stripMargin)
    testS("""match x:
        |  case {n.m: a}:
        |    pass""".stripMargin)
    testS(
      """match x:
        |  case {1: a, 2: b, **r,}:
        |    pass""".stripMargin,
      """match x:
        |  case {1: a, 2: b, **r}:
        |    pass""".stripMargin
    )
    testS(
      """match x:
        |  case {1: a, **r, 2: b}:
        |    pass""".stripMargin,
      """match x:
        |  case {1: a, 2: b, **r}:
        |    pass""".stripMargin
    )
  }

  "pattern matching case tests - class" in {
    testS("""match x:
        |  case Foo():
        |    pass""".stripMargin)
    testS("""match x:
        |  case lib.Foo():
        |    pass""".stripMargin)
    testS("""match x:
        |  case Foo(1, 2):
        |    pass""".stripMargin)
    testS(
      """match x:
        |  case Foo(1, 2,):
        |    pass""".stripMargin,
      """match x:
        |  case Foo(1, 2):
        |    pass""".stripMargin
    )
    testS("""match x:
        |  case Foo(a = 1, b = 2):
        |    pass""".stripMargin)
    testS(
      """match x:
        |  case Foo(a = 1, b = 2,):
        |    pass""".stripMargin,
      """match x:
        |  case Foo(a = 1, b = 2):
        |    pass""".stripMargin
    )
    testS("""match x:
        |  case Foo(1, 2, a = 3, b = 4):
        |    pass""".stripMargin)
  }

  "pattern matching case tests - guard" in {
    testS("""match x:
        |  case y if y == x:
        |    pass""".stripMargin)
    testS("""match x:
        |  case y if a := 1:
        |    pass""".stripMargin)
  }

  "pattern matching case tests - as" in {
    testS("""match x:
        |  case _ as z:
        |    pass""".stripMargin)
  }

  "pattern matching case tests - general" in {
    testS("""match x:
        |  case 1 | 2 | 3:
        |    pass""".stripMargin)
    testS(
      """match x:
        |  case (1 | 2 | 3) as x:
        |    pass""".stripMargin,
      """match x:
        |  case 1 | 2 | 3 as x:
        |    pass""".stripMargin
    )
    testS("""match x:
        |  case 1 | 2 | 3 as x:
        |    pass""".stripMargin)
    testS("""match x:
        |  case [a, b, Foo(1, n = 2)] as x:
        |    pass""".stripMargin)
    testS("""match x:
        |  case 1:
        |    pass
        |  case 2:
        |    print()""".stripMargin)
    testS("""match x:
        |  case 1:
        |    while y != 2:
        |      pas
        |  case 2:
        |    print()""".stripMargin)
    testS("""match x:
        |  case 1:
        |    match y:
        |      case 1:
        |        pass""".stripMargin)
    testS("""match x:
        |  case 1:
        |    pass
        |pass""".stripMargin)
  }

  "test non keyword 'match' usages" in {
    testT("match[1] = 0")
    testT("match * 2")
  }

  "non-latin identifier name tests" in {
    testT("print(Σ_1)")
    testT("print(ø2)")
    testT("print(_x)")
    testT("print(Ǝ1)")
    testT("print(ƹ0)")
    testT("print(ש0)")
    testT("print(ߕ)")
  }
}
