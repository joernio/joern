package io.joern.rubysrc2cpg.deprecated.passes.ast

import io.joern.rubysrc2cpg.deprecated.astcreation.AstCreator
import io.joern.rubysrc2cpg.deprecated.passes.Defines
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{Identifier, Literal, NewIdentifier}
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, Operators}
import io.shiftleft.semanticcpg.language.*

class SimpleAstCreationPassTest extends RubyCode2CpgFixture(useDeprecatedFrontend = true) {

  "AST generation for simple fragments" should {

    "have correct structure for a single command call" in {
      val cpg = code("""puts 123""")

      val List(assign, puts) = cpg.call.l
      val List(arg)          = puts.argument.isLiteral.l

      puts.code shouldBe "puts 123"
      puts.lineNumber shouldBe Some(1)

      arg.code shouldBe "123"
      arg.lineNumber shouldBe Some(1)
      arg.columnNumber shouldBe Some(5)

      assign.name shouldBe "<operator>.assignment" // call node for builtin typeRef assignment
    }

    "have correct structure for an unsigned, decimal integer literal" in {
      val cpg           = code("123")
      val List(literal) = cpg.literal.l
      literal.typeFullName shouldBe "__builtin.Integer"
      literal.code shouldBe "123"
      literal.lineNumber shouldBe Some(1)
      literal.columnNumber shouldBe Some(0)
    }

    "have correct structure for a +integer, decimal literal" in {
      val cpg           = code("+1")
      val List(literal) = cpg.literal.l
      literal.typeFullName shouldBe "__builtin.Integer"
      literal.code shouldBe "+1"
      literal.lineNumber shouldBe Some(1)
      literal.columnNumber shouldBe Some(0)
    }

    "have correct structure for a -integer, decimal literal" in {
      val cpg           = code("-1")
      val List(literal) = cpg.literal.l
      literal.typeFullName shouldBe "__builtin.Integer"
      literal.code shouldBe "-1"
      literal.lineNumber shouldBe Some(1)
      literal.columnNumber shouldBe Some(0)
    }

    "have correct structure for an unsigned, decimal float literal" in {
      val cpg           = code("3.14")
      val List(literal) = cpg.literal.l
      literal.typeFullName shouldBe "__builtin.Float"
      literal.code shouldBe "3.14"
      literal.lineNumber shouldBe Some(1)
      literal.columnNumber shouldBe Some(0)
    }

    "have correct structure for a +float, decimal literal" in {
      val cpg           = code("+3.14")
      val List(literal) = cpg.literal.l
      literal.typeFullName shouldBe "__builtin.Float"
      literal.code shouldBe "+3.14"
      literal.lineNumber shouldBe Some(1)
      literal.columnNumber shouldBe Some(0)
    }

    "have correct structure for a -float, decimal literal" in {
      val cpg           = code("-3.14")
      val List(literal) = cpg.literal.l
      literal.typeFullName shouldBe "__builtin.Float"
      literal.code shouldBe "-3.14"
      literal.lineNumber shouldBe Some(1)
      literal.columnNumber shouldBe Some(0)
    }

    "have correct structure for an unsigned, decimal float literal with unsigned exponent" in {
      val cpg           = code("3e10")
      val List(literal) = cpg.literal.l
      literal.typeFullName shouldBe "__builtin.Float"
      literal.code shouldBe "3e10"
      literal.lineNumber shouldBe Some(1)
      literal.columnNumber shouldBe Some(0)
    }

    "have correct structure for an unsigned, decimal float literal with -exponent" in {
      val cpg           = code("12e-10")
      val List(literal) = cpg.literal.l
      literal.typeFullName shouldBe "__builtin.Float"
      literal.code shouldBe "12e-10"
      literal.lineNumber shouldBe Some(1)
      literal.columnNumber shouldBe Some(0)
    }

    "have correct structure for an unsigned, binary integer literal" in {
      val cpg           = code("0b01")
      val List(literal) = cpg.literal.l
      literal.typeFullName shouldBe "__builtin.Integer"
      literal.code shouldBe "0b01"
      literal.lineNumber shouldBe Some(1)
      literal.columnNumber shouldBe Some(0)
    }

    "have correct structure for a -integer, binary literal" in {
      val cpg           = code("-0b01")
      val List(literal) = cpg.literal.l
      literal.typeFullName shouldBe "__builtin.Integer"
      literal.code shouldBe "-0b01"
      literal.lineNumber shouldBe Some(1)
      literal.columnNumber shouldBe Some(0)
    }

    "have correct structure for a +integer, binary literal" in {
      val cpg           = code("+0b01")
      val List(literal) = cpg.literal.l
      literal.typeFullName shouldBe "__builtin.Integer"
      literal.code shouldBe "+0b01"
      literal.lineNumber shouldBe Some(1)
      literal.columnNumber shouldBe Some(0)
    }

    "have correct structure for an unsigned, hexadecimal integer literal" in {
      val cpg           = code("0xabc")
      val List(literal) = cpg.literal.l
      literal.typeFullName shouldBe "__builtin.Integer"
      literal.code shouldBe "0xabc"
      literal.lineNumber shouldBe Some(1)
      literal.columnNumber shouldBe Some(0)
    }

    "have correct structure for a -integer, hexadecimal literal" in {
      val cpg           = code("-0xa")
      val List(literal) = cpg.literal.l
      literal.typeFullName shouldBe "__builtin.Integer"
      literal.code shouldBe "-0xa"
      literal.lineNumber shouldBe Some(1)
      literal.columnNumber shouldBe Some(0)
    }

    "have correct structure for a +integer, hexadecimal literal" in {
      val cpg           = code("+0xa")
      val List(literal) = cpg.literal.l
      literal.typeFullName shouldBe "__builtin.Integer"
      literal.code shouldBe "+0xa"
      literal.lineNumber shouldBe Some(1)
      literal.columnNumber shouldBe Some(0)
    }

    "have correct structure for `nil` literal" in {
      val cpg           = code("puts nil")
      val List(literal) = cpg.literal.l
      literal.typeFullName shouldBe Defines.NilClass
      literal.code shouldBe "nil"
      literal.lineNumber shouldBe Some(1)
      literal.columnNumber shouldBe Some(5)
    }

    "have correct structure for `true` literal" in {
      val cpg           = code("puts true")
      val List(literal) = cpg.literal.l
      literal.typeFullName shouldBe Defines.TrueClass
      literal.code shouldBe "true"
      literal.lineNumber shouldBe Some(1)
      literal.columnNumber shouldBe Some(5)
    }

    "have correct structure for `false` literal" in {
      val cpg           = code("puts false")
      val List(literal) = cpg.literal.l
      literal.typeFullName shouldBe Defines.FalseClass
      literal.code shouldBe "false"
      literal.lineNumber shouldBe Some(1)
      literal.columnNumber shouldBe Some(5)
    }

    "have correct structure for `self` identifier" in {
      val cpg           = code("puts self")
      val List(self, _) = cpg.identifier.l
      self.typeFullName shouldBe Defines.Object
      self.code shouldBe "self"
      self.lineNumber shouldBe Some(1)
      self.columnNumber shouldBe Some(5)
    }

    "have correct structure for `__FILE__` identifier" in {
      val cpg           = code("puts __FILE__")
      val List(file, _) = cpg.identifier.l
      file.typeFullName shouldBe "__builtin.String"
      file.code shouldBe "__FILE__"
      file.lineNumber shouldBe Some(1)
      file.columnNumber shouldBe Some(5)
    }

    "have correct structure for `__LINE__` identifier" in {
      val cpg           = code("puts __LINE__")
      val List(line, _) = cpg.identifier.l
      line.typeFullName shouldBe "__builtin.Integer"
      line.code shouldBe "__LINE__"
      line.lineNumber shouldBe Some(1)
      line.columnNumber shouldBe Some(5)
    }

    "have correct structure for `__ENCODING__` identifier" in {
      val cpg               = code("puts __ENCODING__")
      val List(encoding, _) = cpg.identifier.l
      encoding.typeFullName shouldBe Defines.Encoding
      encoding.code shouldBe "__ENCODING__"
      encoding.lineNumber shouldBe Some(1)
      encoding.columnNumber shouldBe Some(5)
    }

    "have correct structure for a single-line double-quoted string literal" in {
      val cpg           = code("\"hello\"")
      val List(literal) = cpg.literal.l
      literal.typeFullName shouldBe "__builtin.String"
      literal.code shouldBe "\"hello\""
      literal.lineNumber shouldBe Some(1)
      literal.columnNumber shouldBe Some(0)
    }

    "have correct structure for a single-line single-quoted string literal" in {
      val cpg           = code("'hello'")
      val List(literal) = cpg.literal.l
      literal.typeFullName shouldBe "__builtin.String"
      literal.code shouldBe "'hello'"
      literal.lineNumber shouldBe Some(1)
      literal.columnNumber shouldBe Some(0)
    }

    "have correct structure for a single-line quoted non-expanded string literal" in {
      val cpg           = code("%q(hello)")
      val List(literal) = cpg.literal.l
      literal.typeFullName shouldBe "__builtin.String"
      literal.code shouldBe "%q(hello)"
      literal.lineNumber shouldBe Some(1)
    }

    "have correct structure for a multi-line quoted non-expanded string literal" in {
      val cpg = code("""%q<
          |xyz
          |123
          |>""".stripMargin)
      val List(literal) = cpg.literal.l
      literal.typeFullName shouldBe "__builtin.String"
      literal.code shouldBe
        """%q<
          |xyz
          |123
          |>""".stripMargin
      literal.lineNumber shouldBe Some(1)
    }

    "have correct structure for an identifier symbol literal" in {
      val cpg           = code(":someSymbolName")
      val List(literal) = cpg.literal.l
      literal.typeFullName shouldBe Defines.Symbol
      literal.code shouldBe ":someSymbolName"
      literal.lineNumber shouldBe Some(1)
      literal.columnNumber shouldBe Some(0)
    }

    "have correct structure for a single-quoted-string symbol literal" in {
      val cpg           = code(":'someSymbolName'")
      val List(literal) = cpg.literal.l
      literal.typeFullName shouldBe Defines.Symbol
      literal.code shouldBe ":'someSymbolName'"
      literal.lineNumber shouldBe Some(1)
      literal.columnNumber shouldBe Some(0)
    }

    "have correct structure for an identifier symbol literal used in an `undef` statement" in {
      val cpg           = code("undef :symbolName")
      val List(literal) = cpg.literal.l
      literal.typeFullName shouldBe Defines.Symbol
      literal.code shouldBe ":symbolName"
      literal.lineNumber shouldBe Some(1)
      literal.columnNumber shouldBe Some(6)
    }

    "have correct structure for a single-line regular expression literal" in {
      val cpg           = code("/(eu|us)/")
      val List(literal) = cpg.literal.l
      literal.typeFullName shouldBe Defines.Regexp
      literal.code shouldBe "/(eu|us)/"
      literal.lineNumber shouldBe Some(1)
      literal.columnNumber shouldBe Some(0)
    }

    "have correct structure for a single-line quoted (%r) regular expression literal" in {
      val cpg               = code("%r{eu|us}")
      val List(literalNode) = cpg.literal.l
      literalNode.typeFullName shouldBe Defines.Regexp
      literalNode.code shouldBe "%r{eu|us}"
      literalNode.lineNumber shouldBe Some(1)
      literalNode.columnNumber shouldBe Some(0)
    }

    "have correct structure for an empty regular expression literal used as the second argument to a call" in {
      val cpg           = code("puts(x, //)")
      val List(literal) = cpg.literal.l
      literal.typeFullName shouldBe Defines.Regexp
      literal.code shouldBe "//"
      literal.lineNumber shouldBe Some(1)
      literal.columnNumber shouldBe Some(8)
    }

    "have correct structure for a single-line regular expression literal passed as argument to a command" in {
      val cpg = code("puts /x/")

      val List(_, callNode) = cpg.call.l
      callNode.code shouldBe "puts /x/"
      callNode.name shouldBe "puts"
      callNode.lineNumber shouldBe Some(1)

      val List(literalArg) = callNode.argument.isLiteral.l
      literalArg.argumentIndex shouldBe 1
      literalArg.typeFullName shouldBe Defines.Regexp
      literalArg.code shouldBe "/x/"
      literalArg.lineNumber shouldBe Some(1)
    }

    "have correct structure for a single left had side call" in {
      val cpg            = code("array[n] = 10")
      val List(callNode) = cpg.call.name(Operators.indexAccess).l
      callNode.code shouldBe "array[n]"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(5)
    }

    "have correct structure for a binary expression" in {
      val cpg            = code("x+y")
      val List(callNode) = cpg.call.name(Operators.addition).l
      callNode.code shouldBe "x+y"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(0)
    }

    "have correct structure for a not expression" in {
      val cpg            = code("not y")
      val List(callNode) = cpg.call.name(Operators.not).l
      callNode.code shouldBe "not y"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(0)
    }

    "have correct structure for a power expression" in {
      val cpg            = code("x**y")
      val List(callNode) = cpg.call.name(Operators.exponentiation).l
      callNode.code shouldBe "x**y"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(0)
    }

    "have correct structure for a inclusive range expression" in {
      val cpg            = code("1..10")
      val List(callNode) = cpg.call.name(Operators.range).l
      callNode.code shouldBe "1..10"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(0)
    }

    "have correct structure for a non-inclusive range expression" in {
      val cpg            = code("1...10")
      val List(callNode) = cpg.call.name(Operators.range).l
      callNode.code shouldBe "1...10"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(0)
    }

    "have correct structure for a relational expression" in {
      val cpg            = code("x<y")
      val List(callNode) = cpg.call.name(Operators.lessThan).l
      callNode.code shouldBe "x<y"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(0)
    }

    "have correct structure for a unary exclamation expression" in {
      val cpg            = code("!y")
      val List(callNode) = cpg.call.name(Operators.not).l
      callNode.code shouldBe "!y"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(0)
    }

    "have correct structure for a unary tilde expression" in {
      val cpg            = code("~y")
      val List(callNode) = cpg.call.name(Operators.not).l
      callNode.code shouldBe "~y"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(0)
    }

    "have correct structure for a unary plus expression" in {
      val cpg            = code("+y")
      val List(callNode) = cpg.call.name(Operators.plus).l
      callNode.code shouldBe "+y"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(0)
    }

    "have correct structure for a unary minus expression" in {
      val cpg            = code("-y")
      val List(callNode) = cpg.call.name(Operators.minus).l
      callNode.code shouldBe "-y"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(0)
    }

    "have correct structure for a call node" in {
      val cpg               = code("puts \"something\"")
      val List(_, callNode) = cpg.call.l
      callNode.code shouldBe "puts \"something\""
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(0)
    }

    "have correct structure for a logical and expression" in {
      val cpg            = code("x & y")
      val List(callNode) = cpg.call.name(Operators.logicalAnd).l
      callNode.code shouldBe "x & y"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(0)
    }

    "have correct structure for a logical or with bar expression" in {
      val cpg            = code("x | y")
      val List(callNode) = cpg.call.name(Operators.logicalOr).l
      callNode.code shouldBe "x | y"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(0)
    }

    "have correct structure for a logical or with carat expression" in {
      val cpg            = code("x ^ y")
      val List(callNode) = cpg.call.name(Operators.logicalOr).l
      callNode.code shouldBe "x ^ y"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(0)
    }

    "have correct structure for a assignment expression" in {
      val cpg            = code("x = y")
      val List(callNode) = cpg.call.name(Operators.assignment).l
      callNode.code shouldBe "x = y"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(2)
    }

    "have correct structure for a multiple assignment expression" in {
      val cpg      = code("x, y, z = a, b, c")
      val callNode = cpg.call.name(Operators.assignment).l
      callNode.size shouldBe 3
      callNode.argument
        .where(_.argumentIndex(1))
        .code
        .l shouldBe List("x", "y", "z")
      callNode.argument
        .where(_.argumentIndex(2))
        .code
        .l shouldBe List("a", "b", "c")
      callNode.lineNumber.l shouldBe List(1, 1, 1)
    }

    "have correct structure for a multiple assignment expression with calls in RHS" in {
      val cpg      = code("x, y = [ foo(), bar() ]")
      val callNode = cpg.call.name(Operators.assignment).l
      callNode.size shouldBe 2
      callNode.argument
        .where(_.argumentIndex(1))
        .code
        .l shouldBe List("x", "y")
      callNode.argument
        .where(_.argumentIndex(2))
        .code
        .l shouldBe List("foo()", "bar()")
      callNode.lineNumber.l shouldBe List(1, 1)
    }

    "have correct structure for a single assignment expression with array in RHS" in {
      val cpg                      = code("x = [a, b, c]")
      val List(assignmentCallNode) = cpg.call.name(Operators.assignment).l
      assignmentCallNode.size shouldBe 1
      val List(arrayCallNode) = cpg.call.name(Operators.arrayInitializer).l
      arrayCallNode.size shouldBe 1
      arrayCallNode.argument
        .where(_.argumentIndex(1))
        .code
        .l shouldBe List("a")
      arrayCallNode.argument
        .where(_.argumentIndex(2))
        .code
        .l shouldBe List("b")
      arrayCallNode.argument
        .where(_.argumentIndex(3))
        .code
        .l shouldBe List("c")
    }

    "have correct structure for a equals expression" in {
      val cpg            = code("x == y")
      val List(callNode) = cpg.call.name(Operators.equals).l
      callNode.code shouldBe "x == y"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(0)
    }

    "have correct structure for a division expression" in {
      val cpg            = code("x / y")
      val List(callNode) = cpg.call.name(Operators.division).l
      callNode.code shouldBe "x / y"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(0)
    }

    "have correct structure for a modulo expression" in {
      val cpg            = code("x % y")
      val List(callNode) = cpg.call.name(Operators.modulo).l
      callNode.code shouldBe "x % y"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(0)
    }

    "have correct structure for a shift right expression" in {
      val cpg            = code("x >> y")
      val List(callNode) = cpg.call.name(Operators.logicalShiftRight).l
      callNode.code shouldBe "x >> y"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(0)
    }

    "have correct structure for a shift left expression" in {
      val cpg            = code("x << y")
      val List(callNode) = cpg.call.name(Operators.shiftLeft).l
      callNode.code shouldBe "x << y"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(0)
    }

    "have correct structure for a compare expression" in {
      val cpg            = code("x <=> y")
      val List(callNode) = cpg.call.name(Operators.compare).l
      callNode.code shouldBe "x <=> y"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(0)
    }

    "have correct structure for a indexing expression" in {
      val cpg            = code("def some_method(index)\n some_map[index]\nend")
      val List(callNode) = cpg.call.name(Operators.indexAccess).l
      callNode.code shouldBe "some_map[index]"
      callNode.lineNumber shouldBe Some(2)
      callNode.columnNumber shouldBe Some(9)
    }

    "have correct structure for overloaded index operator method" in {
      val cpg = code("""
          |class MyClass
          |def [](key)
          |  @member_hash[key]
          |end
          |end
          |""".stripMargin)

      val List(methodNode) = cpg.method.name("\\[]").l
      methodNode.fullName shouldBe "Test0.rb::program.MyClass.[]"
      methodNode.code shouldBe "def [](key)\n  @member_hash[key]\nend"
      methodNode.lineNumber shouldBe Some(3)
      methodNode.lineNumberEnd shouldBe Some(5)
      methodNode.columnNumber shouldBe Some(4)
    }

    "have correct structure for overloaded equality operator method" in {
      val cpg = code("""
          |class MyClass
          |def ==(other)
          |  @my_member==other
          |end
          |end
          |""".stripMargin)

      val List(methodNode) = cpg.method.name("==").l
      methodNode.fullName shouldBe "Test0.rb::program.MyClass.=="
      methodNode.code shouldBe "def ==(other)\n  @my_member==other\nend"
      methodNode.lineNumber shouldBe Some(3)
      methodNode.lineNumberEnd shouldBe Some(5)
      methodNode.columnNumber shouldBe Some(4)
    }

    "have correct structure for class method" in {
      val cpg = code("""
          |class MyClass
          |def some_method(param)
          |end
          |end
          |""".stripMargin)

      val List(methodNode) = cpg.method.name("some_method").l
      methodNode.fullName shouldBe "Test0.rb::program.MyClass.some_method"
      methodNode.code shouldBe "def some_method(param)\nend"
      methodNode.lineNumber shouldBe Some(3)
      methodNode.lineNumberEnd shouldBe Some(4)
      methodNode.columnNumber shouldBe Some(4)
    }

    "have correct structure for scope resolution operator call" in {
      val cpg = code("""
          |def foo(param)
          |::SomeConstant = param
          |end
          |""".stripMargin)

      val List(identifierNode) = cpg.identifier.name("SomeConstant").l
      identifierNode.code shouldBe "SomeConstant"
      identifierNode.lineNumber shouldBe Some(3)
      identifierNode.columnNumber shouldBe Some(0)
    }

    "have correct structure for a addition expression with space before addition" in {
      val cpg            = code("x + y")
      val List(callNode) = cpg.call.name(Operators.addition).l
      callNode.code shouldBe "x + y"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(0)
    }

    "have correct structure for a addition expression with space before subtraction" in {
      val cpg            = code("x - y")
      val List(callNode) = cpg.call.name(Operators.subtraction).l
      callNode.code shouldBe "x - y"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(0)
    }

    "have correct structure for object's method access (chainedInvocationPrimary)" in {
      val cpg            = code("object.some_method(arg1,arg2)")
      val List(callNode) = cpg.call.name("some_method").l
      callNode.code shouldBe "object.some_method(arg1,arg2)"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(6)

      val List(identifierNode1) = cpg.identifier.name("arg1").l
      identifierNode1.code shouldBe "arg1"
      identifierNode1.lineNumber shouldBe Some(1)
      identifierNode1.columnNumber shouldBe Some(19)

      val List(identifierNode2) = cpg.identifier.name("arg2").l
      identifierNode2.code shouldBe "arg2"
      identifierNode2.lineNumber shouldBe Some(1)
      identifierNode2.columnNumber shouldBe Some(24)
    }

    "have correct structure for object's method.member access (chainedInvocationPrimary)" ignore {
      val cpg                  = code("object.some_member")
      val List(identifierNode) = cpg.identifier.name("some_member").l
      identifierNode.code shouldBe "some_member"
      identifierNode.lineNumber shouldBe Some(1)
      identifierNode.columnNumber shouldBe Some(0)
    }

    "have correct structure for negation before block (invocationExpressionOrCommand)" in {
      val cpg = code("!foo arg do\nputs arg\nend")

      val List(callNode1) = cpg.call.name(Operators.not).l
      callNode1.code shouldBe "!foo arg do\nputs arg\nend"
      callNode1.lineNumber shouldBe Some(1)
      callNode1.columnNumber shouldBe Some(0)

      val List(callNode2) = cpg.call.name("foo").l
      callNode2.code shouldBe "foo arg do\nputs arg\nend"
      callNode2.lineNumber shouldBe Some(1)
      callNode2.columnNumber shouldBe Some(1)

      val List(callNode3) = cpg.call.name("puts").l
      callNode3.code shouldBe "puts arg"
      callNode3.lineNumber shouldBe Some(2)
      callNode3.columnNumber shouldBe Some(0)

      val List(argArgumentOfPuts, argArgumentOfFoo) = cpg.identifier.name("arg").l
      argArgumentOfFoo.code shouldBe "arg"
      argArgumentOfFoo.lineNumber shouldBe Some(2)
      argArgumentOfFoo.columnNumber shouldBe Some(5)

      argArgumentOfPuts.code shouldBe "arg"
      argArgumentOfPuts.lineNumber shouldBe Some(1)
    }

    "have correct structure for a hash initialisation" in {
      val cpg       = code("hashMap = {\"k1\" => 1, \"k2\" => 2}")
      val callNodes = cpg.call.name("<operator>.keyValueAssociation").l
      callNodes.size shouldBe 2
      callNodes.head.code shouldBe "\"k1\" => 1"
      callNodes.head.lineNumber shouldBe Some(1)
      callNodes.head.columnNumber shouldBe Some(16)
    }

    "have correct structure for defined? command" in {
      val cpg = code("defined? x")

      val List(callNode) = cpg.call.name("<operator>.defined").l
      callNode.code shouldBe "defined? x"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(0)

      val List(identifierNode) = cpg.identifier.name("x").l
      identifierNode.code shouldBe "x"
      identifierNode.lineNumber shouldBe Some(1)
      identifierNode.columnNumber shouldBe Some(9)
    }

    "have correct structure for defined? call" in {
      val cpg = code("defined?(x)")

      val List(callNode) = cpg.call.name("<operator>.defined").l
      callNode.code shouldBe "defined?(x)"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(0)

      val List(identifierNode) = cpg.identifier.name("x").l
      identifierNode.code shouldBe "x"
      identifierNode.lineNumber shouldBe Some(1)
      identifierNode.columnNumber shouldBe Some(9)
    }

    "have correct structure for chainedInvocationWithoutArgumentsPrimary" in {
      val cpg = code("object::foo do\nputs \"right here\"\nend")

      val List(callNode1) = cpg.call.name("foo").l
      callNode1.code shouldBe "puts \"right here\""
      callNode1.lineNumber shouldBe Some(1)
      callNode1.columnNumber shouldBe Some(3)

      val List(callNode2) = cpg.call.name("puts").l
      callNode2.code shouldBe "puts \"right here\""
      callNode2.lineNumber shouldBe Some(2)
      callNode2.columnNumber shouldBe Some(0)
    }

    "have correct structure for require with an expression" in {
      val cpg = code("Dir[Rails.root.join('a', 'b', '**', '*.rb')].each { |f| require f }")

      val List(callNode) = cpg.call.name("require").l
      callNode.code shouldBe "require f"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(56)
    }

    "have correct structure for undef" in {
      val cpg = code("undef method1,method2")

      val List(callNode) = cpg.call.name("<operator>.undef").l
      callNode.code shouldBe "undef method1,method2"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(0)
    }

    "have correct structure for ternary if expression" in {
      val cpg               = code("a ? b : c")
      val List(controlNode) = cpg.controlStructure.l

      controlNode.controlStructureType shouldBe ControlStructureTypes.IF
      controlNode.code shouldBe "a ? b : c"
      controlNode.lineNumber shouldBe Some(1)
      controlNode.columnNumber shouldBe Some(0)

      val List(a) = controlNode.condition.isIdentifier.l
      a.code shouldBe "a"
      a.name shouldBe "a"
      a.lineNumber shouldBe Some(1)
      a.columnNumber shouldBe Some(0)

      val List(_, b, c) = controlNode.astChildren.isIdentifier.l
      b.code shouldBe "b"
      b.name shouldBe "b"
      b.lineNumber shouldBe Some(1)
      b.columnNumber shouldBe Some(4)

      c.code shouldBe "c"
      c.name shouldBe "c"
      c.lineNumber shouldBe Some(1)
      c.columnNumber shouldBe Some(8)
    }

    "have correct structure for if statement" in {
      val cpg = code("""if x == 0 then
          |  puts 1
          |end
          |""".stripMargin)

      val List(ifNode) = cpg.controlStructure.l
      ifNode.controlStructureType shouldBe ControlStructureTypes.IF
      ifNode.lineNumber shouldBe Some(1)

      val List(ifCondition, ifBlock) = ifNode.astChildren.l
      ifCondition.code shouldBe "x == 0"
      ifCondition.lineNumber shouldBe Some(1)

      val List(puts) = ifBlock.astChildren.l
      puts.code shouldBe "puts 1"
      puts.lineNumber shouldBe Some(2)
    }

    "have correct structure for if-else statement" in {
      val cpg = code("""if x == 0 then
          |  puts 1
          |else
          |  puts 2
          |end
          |""".stripMargin)

      val List(ifNode) = cpg.controlStructure.l
      ifNode.controlStructureType shouldBe ControlStructureTypes.IF
      ifNode.lineNumber shouldBe Some(1)

      val List(ifCondition, ifBlock, elseBlock) = ifNode.astChildren.l
      ifCondition.code shouldBe "x == 0"
      ifCondition.lineNumber shouldBe Some(1)

      val List(puts1) = ifBlock.astChildren.l
      puts1.code shouldBe "puts 1"
      puts1.lineNumber shouldBe Some(2)

      val List(puts2) = elseBlock.astChildren.l
      puts2.code shouldBe "puts 2"
      puts2.lineNumber shouldBe Some(4)
    }

    "have correct structure for class definition with body having only identifiers" in {
      val cpg = code("class MyClass\nidentifier1\nidentifier2\nend")

      val List(identifierNode1) = cpg.identifier.name("identifier1").l
      identifierNode1.code shouldBe "identifier1"
      identifierNode1.lineNumber shouldBe Some(2)
      identifierNode1.columnNumber shouldBe Some(0)

      val List(identifierNode2) = cpg.identifier.name("identifier2").l
      identifierNode2.code shouldBe "identifier2"
      identifierNode2.lineNumber shouldBe Some(3)
      identifierNode2.columnNumber shouldBe Some(0)
    }

    // NOTE: The representation for `super` may change, in order to accommodate its meaning.
    //       But until then, modelling it as a call seems the appropriate thing to do.
    "have correct structure for `super` expression call without block" in {
      val cpg = code("super(1)")

      val List(callNode) = cpg.call.l
      callNode.code shouldBe "super(1)"
      callNode.name shouldBe "<operator>.super"
      callNode.lineNumber shouldBe Some(1)

      val List(literalArg) = callNode.argument.isLiteral.l
      literalArg.argumentIndex shouldBe 1
      literalArg.code shouldBe "1"
      literalArg.lineNumber shouldBe Some(1)
    }

    "have correct structure for `super` command call without block" in {
      val cpg = code("super 1")

      val List(callNode) = cpg.call.l
      callNode.code shouldBe "super 1"
      callNode.name shouldBe "<operator>.super"
      callNode.lineNumber shouldBe Some(1)

      val List(literalArg) = callNode.argument.isLiteral.l
      literalArg.argumentIndex shouldBe 1
      literalArg.code shouldBe "1"
      literalArg.lineNumber shouldBe Some(1)
    }

    "have generated call nodes for regex interpolation" in {
      val cpg               = code("/x#{Regexp.quote(foo)}b#{x+'z'}a/")
      val List(literalNode) = cpg.literal.l
      cpg.call.size shouldBe 2
      literalNode.code shouldBe "'z'"
    }

    "have correct structure for keyword? named method usage usage" in {
      val cpg = code("x = 1.nil?")

      val List(callNode) = cpg.call.nameExact("nil?").l
      callNode.code shouldBe "1.nil?"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(5)

      val List(arg) = callNode.argument.isLiteral.l
      arg.code shouldBe "1"
    }

    "have correct structure for keyword usage inside association" in {
      val cpg = code("foo if: x.nil?")

      val List(callNode) = cpg.call.nameExact("nil?").l
      callNode.code shouldBe "x.nil?"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(9)

      val List(arg) = callNode.argument.isIdentifier.l
      arg.code shouldBe "x"

      val List(assocCallNode) = cpg.call.nameExact("<operator>.activeRecordAssociation").l
      assocCallNode.code shouldBe "if: x.nil?"
      assocCallNode.lineNumber shouldBe Some(1)
      assocCallNode.columnNumber shouldBe Some(6)

      assocCallNode.argument.size shouldBe 2
      assocCallNode.argument.argumentIndex(1).head.code shouldBe "if"
      assocCallNode.argument.argumentIndex(2).head.code shouldBe "x.nil?"
    }

    "have correct structure for proc definiton with procParameters and empty block" in {
      val cpg =
        code("-> (x,y) {}")
      cpg.parameter.size shouldBe 2
    }

    "have correct structure for proc definiton with procParameters and non-empty block" in {
      val cpg =
        code("""-> (x,y) {
            |if (x)
            | y
            |else
            | b
            |end
            |}""".stripMargin)
      cpg.parameter.size shouldBe 2
      val List(paramOne, paramTwo) = cpg.parameter.l
      paramOne.name shouldBe "x"
      paramTwo.name shouldBe "y"
      cpg.ifBlock.size shouldBe 1
    }

    "have correct structure for proc definition with no parameters and empty block" in {
      val cpg = code("-> {}")
      cpg.parameter.size shouldBe 0
    }

    "have correct structure for proc definition with additional context" in {
      val cpg = code(
        "scope :get_all_doctors, -> { (select('id, first_name').where('role = :user_role', user_role: User.roles[:doctor])) }"
      )
      cpg.parameter.size shouldBe 6
      cpg.call.name("proc_2").size shouldBe 1
      cpg.call.name("scope").size shouldBe 1
      cpg.call.name("where").size shouldBe 1
      cpg.call.name("select").size shouldBe 1
      cpg.call.name("roles").size shouldBe 1
      cpg.call.name("<operator>.activeRecordAssociation").size shouldBe 1
      cpg.call.name("<operator>.indexAccess").size shouldBe 1
    }

    "have correct structure when method called with safe navigation without parameters" in {
      val cpg = code("foo&.bar")
      cpg.call.size shouldBe 1
    }

    "have correct structure when method called with safe navigation with parameters with parantheses" in {
      val cpg = code("foo&.bar(1)")

      val List(callNode)  = cpg.call.l
      val List(actualArg) = callNode.argument.argumentIndex(1).l
      actualArg.code shouldBe "1"
      cpg.argument.size shouldBe 2
      cpg.call.size shouldBe 1
    }

    "have correct structure when method called with safe navigation with parameters without parantheses" in {
      val cpg = code("foo&.bar 1,2")

      val List(callNode)   = cpg.call.l
      val List(actualArg1) = callNode.argument.argumentIndex(1).l
      actualArg1.code shouldBe "1"
      val List(actualArg2) = callNode.argument.argumentIndex(2).l
      actualArg2.code shouldBe "2"
      cpg.argument.size shouldBe 3
      cpg.call.size shouldBe 1
    }

    "have correct structure when method call present in next line, with the second line starting with `.`" in {
      val cpg = code("foo\n   .bar(1)")

      val List(callNode) = cpg.call.l
      cpg.call.size shouldBe 1
      callNode.code shouldBe ("foo\n   .bar(1)")
      callNode.name shouldBe "bar"
      callNode.lineNumber shouldBe Some(2)
      val List(actualArg) = callNode.argument.argumentIndex(1).l
      actualArg.code shouldBe "1"
    }

    "have correct structure when method call present in next line, with the first line ending with `.`" in {
      val cpg = code("foo.\n   bar(1)")

      val List(callNode) = cpg.call.l
      cpg.call.size shouldBe 1
      callNode.code shouldBe ("foo.\n   bar(1)")
      callNode.name shouldBe "bar"
      callNode.lineNumber shouldBe Some(1)
      val List(actualArg) = callNode.argument.argumentIndex(1).l
      actualArg.code shouldBe "1"
    }

    "have correct structure for proc parameter with name" in {
      val cpg                   = code("def foo(&block) end")
      val List(actualParameter) = cpg.method("foo").parameter.l
      actualParameter.name shouldBe "block"
    }

    "have correct structure for proc parameter with no name" in {
      val cpg                   = code("def foo(&) end")
      val List(actualParameter) = cpg.method("foo").parameter.l
      actualParameter.name shouldBe "param_0"
    }

    "have correct structure when regular expression literal passed after `when`" in {
      val cpg = code("""
          |case foo
          | when /^ch/
          |   bar
          |end
          |""".stripMargin)

      val List(literalArg) = cpg.literal.l
      literalArg.typeFullName shouldBe Defines.Regexp
      literalArg.code shouldBe "/^ch/"
      literalArg.lineNumber shouldBe Some(3)
    }

    "have correct structure when have interpolated double-quoted string literal" in {
      val cpg = code("""
          |v = :"w x #{y} z"
          |""".stripMargin)

      cpg.call.size shouldBe 4
      cpg.call.name("<operator>.formatString").head.code shouldBe """:"w x #{y} z""""
      cpg.call.name("<operator>.formatValue").head.code shouldBe "#{y}"

      cpg.literal.size shouldBe 2
      cpg.literal.code("w x ").size shouldBe 1
      cpg.literal.code(" z").size shouldBe 1

      cpg.identifier.name("y").size shouldBe 1
      cpg.identifier.name("v").size shouldBe 1
    }

    "have correct structure when have non-interpolated double-quoted string literal" in {
      val cpg = code("""
          |x = :"y z"
          |""".stripMargin)

      cpg.call.size shouldBe 1
      val List(literal) = cpg.literal.l
      literal.code shouldBe ":\"y z\""
      literal.typeFullName shouldBe Defines.Symbol
    }

    "have correct structure when have symbol " in {
      val cpg = code(s"""
          |x = :"${10}"
          |""".stripMargin)

      cpg.call.size shouldBe 1
      val List(literal) = cpg.literal.l
      literal.typeFullName shouldBe Defines.Symbol
      literal.code shouldBe ":\"10\""
    }
  }

  "have correct structure when no RHS for a mandatory parameter is provided" in {
    val cpg = code("""
        |def foo(bar:)
        |end
        |""".stripMargin)

    val List(parameterNode) = cpg.method("foo").parameter.l
    parameterNode.name shouldBe "bar"
    parameterNode.lineNumber shouldBe Some(2)
  }

  "have correct structure when RHS for a mandatory parameter is provided" in {
    val cpg = code("""
        |def foo(bar: world)
        |end
        |""".stripMargin)

    val List(parameterNode) = cpg.method("foo").parameter.l
    parameterNode.name shouldBe "bar"
    parameterNode.lineNumber shouldBe Some(2)
  }

  // Change below test cases to focus on the argument of call `foo`
  "have correct structure when a association is passed as an argument with parantheses" in {
    val cpg = code("""foo(bar:)""".stripMargin)

    cpg.argument.size shouldBe 2
    cpg.argument.l(0).code shouldBe "bar:"
    cpg.call.size shouldBe 2
    val List(callNode, operatorNode) = cpg.call.l
    callNode.name shouldBe "foo"
    operatorNode.name shouldBe "<operator>.activeRecordAssociation"
  }

  "have correct structure when a association is passed as an argument without parantheses" in {
    val cpg = code("""foo bar:""".stripMargin)

    cpg.argument.size shouldBe 2
    cpg.argument.l.head.code shouldBe "bar:"

    cpg.call.size shouldBe 2
    val List(callNode, operatorNode) = cpg.call.l
    callNode.name shouldBe "foo"
    operatorNode.name shouldBe "<operator>.activeRecordAssociation"
  }

  "have correct structure with ternary operator with multiple line" in {
    val cpg = code("""x = a ?
        | b
        |: c""".stripMargin)

    val List(controlNode) = cpg.controlStructure.l
    controlNode.controlStructureType shouldBe ControlStructureTypes.IF
    controlNode.code shouldBe "a ?\n b\n: c"
    controlNode.lineNumber shouldBe Some(1)
    controlNode.columnNumber shouldBe Some(4)

    val List(a) = controlNode.condition.isIdentifier.l
    a.code shouldBe "a"
    a.name shouldBe "a"
    a.lineNumber shouldBe Some(1)
    a.columnNumber shouldBe Some(4)

    val List(_, b, c) = controlNode.astChildren.isIdentifier.l
    b.code shouldBe "b"
    b.name shouldBe "b"
    b.lineNumber shouldBe Some(2)
    b.columnNumber shouldBe Some(1)

    c.code shouldBe "c"
    c.name shouldBe "c"
    c.lineNumber shouldBe Some(3)
    c.columnNumber shouldBe Some(2)
  }

  "have correct structure for blank indexing arguments" in {
    val cpg = code("""
        |bar = Set[]
        |""".stripMargin)

    val List(callNode) = cpg.call.name("<operator>.indexAccess").l
    callNode.lineNumber shouldBe Some(2)
    callNode.columnNumber shouldBe Some(9)
  }

  "method defined inside a class using << operator" in {
    val cpg = code("""
        class MyClass
        |
        |  class << self
        |    def print
        |      puts "log #{self}"
        |    end
        |  end
        |  class << self
        |  end
        |end
        |
        |MyClass.print""".stripMargin)

    val List(callNode) = cpg.call.name("print").l
    callNode.lineNumber shouldBe Some(13)
    callNode.columnNumber shouldBe Some(7)
    callNode.name shouldBe "print"
  }

  "have correct structure for body statements inside a do block" in {
    val cpg = code("""
        |def foo
        |1/0
        |rescue ZeroDivisionError => e
        |end""".stripMargin)

    val List(methodNode) = cpg.method.code(".*foo.*").l
    methodNode.name shouldBe "foo"
    methodNode.lineNumber shouldBe Some(2)

    val List(assignmentOperator, divisionOperator) = cpg.method.name(".*operator.*").l
    divisionOperator.name shouldBe "<operator>.division"
    assignmentOperator.name shouldBe "<operator>.assignment"
  }

  "have correct structure when regex literal is used on RHS of association" in {
    val cpg = code("""
        |books = [
        |   {
        |       id: /.*/
        |   }
        |]
        |""".stripMargin)

    val List(assocOperator) = cpg.call(".*activeRecordAssociation.*").l
    assocOperator.code shouldBe "id: /.*/"
    assocOperator.astChildren.code.l(1) shouldBe "/.*/"
    assocOperator.lineNumber shouldBe Some(4)
  }

  "have double-quoted string literals containing \\u character" in {
    val cpg = code("""
      |val fileName = "AB\u0003\u0004\u0014\u0000\u0000\u0000\b\u0000\u0000\u0000!\u0000file"
      |""".stripMargin)

    cpg.identifier.size shouldBe 1
    cpg.identifier.name.head shouldBe "fileName"
    cpg.literal.head.code
      .stripPrefix("\"")
      .stripSuffix("\"")
      .trim shouldBe """AB\u0003\u0004\u0014\u0000\u0000\u0000\b\u0000\u0000\u0000!\u0000file"""

  }

  "have correct structure for a endless method" in {
    val cpg = code("""
        |def foo(a,b) = a*b
        |""".stripMargin)

    val List(methodNode) = cpg.method.name("foo").l
    methodNode.lineNumber shouldBe Some(2)
    methodNode.columnNumber shouldBe Some(4)
  }

  "have correct structure for symbol literal defined using \\:" in {
    val cpg = code("""
        |foo = {:bar=>zoo}
        |""".stripMargin)

    val List(keyValueAssocOperator) = cpg.call(".*keyValueAssociation.*").l
    keyValueAssocOperator.code shouldBe ":bar=>zoo"
    keyValueAssocOperator.astChildren.l.head.code shouldBe ":bar"
    keyValueAssocOperator.astChildren.l(1).code shouldBe "zoo"
  }

  "having a binary expression includes + and @" in {
    val cpg = code("""
        |class MyClass
        |  def initialize(a)
        |    @a = a
        |  end
        |
        |  def calculate_x(b)
        |    x = b+@a
        |    return x
        |  end
        |end
        |""".stripMargin)
    cpg.identifier("a").dedup.size shouldBe 1
    cpg.identifier("b").dedup.size shouldBe 1
    cpg.identifier("x").name.dedup.size shouldBe 1
    cpg.method("calculate_x").size shouldBe 1
  }

  "have correct structure for empty %w array" in {
    val cpg = code("""
        |a = %w[]
        |""".stripMargin)

    val List(assignmentCallNode) = cpg.call.name(Operators.assignment).l
    assignmentCallNode.size shouldBe 1
    val List(arrayCallNode) = cpg.call.name(Operators.arrayInitializer).l
    arrayCallNode.size shouldBe 1
    arrayCallNode.argument.size shouldBe 0
  }

  "have correct structure for %w array with %w()" in {
    val cpg = code("""
        |a = %w(b c d)
        |""".stripMargin)

    val List(assignmentCallNode) = cpg.call.name(Operators.assignment).l
    assignmentCallNode.size shouldBe 1
    val List(arrayCallNode) = cpg.call.name(Operators.arrayInitializer).l
    arrayCallNode.size shouldBe 1
    arrayCallNode.argument
      .where(_.argumentIndex(1))
      .code
      .l shouldBe List("b")
    arrayCallNode.argument
      .where(_.argumentIndex(2))
      .code
      .l shouldBe List("c")
    arrayCallNode.argument
      .where(_.argumentIndex(3))
      .code
      .l shouldBe List("d")
  }

  "have correct structure for %w array with %w() with entries separated by whitespace" in {
    val cpg = code("""
        |a = %w(
        | bob
        | cod
        | dod
        |)
        |""".stripMargin)

    val List(assignmentCallNode) = cpg.call.name(Operators.assignment).l
    assignmentCallNode.size shouldBe 1
    val List(arrayCallNode) = cpg.call.name(Operators.arrayInitializer).l
    arrayCallNode.size shouldBe 1
    arrayCallNode.argument
      .where(_.argumentIndex(1))
      .code
      .l shouldBe List("bob")
    arrayCallNode.argument
      .where(_.argumentIndex(2))
      .code
      .l shouldBe List("cod")
    arrayCallNode.argument
      .where(_.argumentIndex(3))
      .code
      .l shouldBe List("dod")
  }

  "have correct structure for %w array with %w- -" in {
    val cpg = code("""
        |a = %w-b c-
        |""".stripMargin)

    val List(assignmentCallNode) = cpg.call.name(Operators.assignment).l
    assignmentCallNode.size shouldBe 1
    val List(arrayCallNode) = cpg.call.name(Operators.arrayInitializer).l
    arrayCallNode.size shouldBe 1
    arrayCallNode.argument
      .where(_.argumentIndex(1))
      .code
      .l shouldBe List("b")
    arrayCallNode.argument
      .where(_.argumentIndex(2))
      .code
      .l shouldBe List("c")
  }

  "have correct structure for %i() array with two elements" in {
    val cpg = code("x = %i(yy zz)")

    val List(arrayInit)                        = cpg.call.name(Operators.arrayInitializer).l
    val List(yyNode: Literal, zzNode: Literal) = arrayInit.argument.isLiteral.l

    yyNode.code shouldBe "yy"
    yyNode.argumentIndex shouldBe 1
    yyNode.typeFullName shouldBe Defines.Symbol

    zzNode.code shouldBe "zz"
    zzNode.argumentIndex shouldBe 2
    zzNode.typeFullName shouldBe Defines.Symbol
  }

  "have correct structure parenthesised arguments in a return jump" in {
    val cpg = code("""return(value) unless item""".stripMargin)

    cpg.identifier.size shouldBe 2
    cpg.identifier.name("value").size shouldBe 1
    cpg.identifier.name("item").size shouldBe 1

    val List(methodReturn) = cpg.ret.l
    methodReturn.code shouldBe "return(value)"
    methodReturn.lineNumber shouldBe Some(1)
    methodReturn.columnNumber shouldBe Some(0)
  }

  "have correct structure for a hash containing splatting elements" in {
    val cpg = code("""
        |bar={:x=>1}
        |foo = {
        |**bar
        |}
        |""".stripMargin)

    val List(keyValueAssocOperator) = cpg.call(".*keyValueAssociation.*").l
    keyValueAssocOperator.code shouldBe ":x=>1"
    keyValueAssocOperator.astChildren.l(1).code shouldBe "1"

    val List(pseudoIdentifier, actualIdentifier) = cpg.identifier("bar").l
    pseudoIdentifier.lineNumber shouldBe Some(2)
    pseudoIdentifier.columnNumber shouldBe Some(0)
  }

  "have correct structure for regex match global variables" in {
    val cpg = code("""
        |content_filename =~ /filename="(.*)"/
        |value = $1
        |""".stripMargin)

    cpg.call.size shouldBe 2
    cpg.call.code(".*filename.*").head.methodFullName shouldBe "<operator>.patternMatch"

    cpg.identifier.code("value").size shouldBe 1
    cpg.identifier.name("\\$1").size shouldBe 1
    cpg.identifier.name("\\$1").head.typeFullName shouldBe Defines.String

    cpg.literal.code("/filename=\"(.*)\"/").head.typeFullName shouldBe Defines.Regexp
  }

  "have correct structure of unless keyword and regex statement" in {
    val cpg = code("""def contains_numbers?(string)
                     |  # Define a regular expression pattern to match any digit
                     |  regex_pattern = /\d/
                     |
                     |  # Check if the string contains any numbers using the 'unless' keyword
                     |  unless string.match(regex_pattern).nil?
                     |    return true
                     |  end
                     |
                     |  return false
                     |end""".stripMargin)

    cpg.identifier.code("regex_pattern").name.dedup.size shouldBe 1
    cpg.method("contains_numbers\\?").name.size shouldBe 1
    cpg.call("<operator>.assignment").name.size shouldBe 2
    cpg.call("match").name.size shouldBe 1
  }

  "have correct structure for association identifier" in {
    val cpg = code("""
        |foo(a:b)
        |""".stripMargin)

    cpg.call.size shouldBe 2
    cpg.call.name("<operator>.activeRecordAssociation").size shouldBe 1

    cpg.identifier.size shouldBe 2
    cpg.identifier.name("a").size shouldBe 1
    cpg.identifier.name("b").size shouldBe 1
  }

  "have correct structure for multiline %i" in {
    val cpg = code("""
        |w = %i(x y
        |     z)
        |""".stripMargin)

    val List(arrayInit)                                      = cpg.call.name(Operators.arrayInitializer).l
    val List(xNode: Literal, yNode: Literal, zNode: Literal) = arrayInit.argument.isLiteral.l

    xNode.code shouldBe "x"
    xNode.argumentIndex shouldBe 1
    xNode.typeFullName shouldBe Defines.Symbol

    yNode.code shouldBe "y"
    yNode.argumentIndex shouldBe 2
    yNode.typeFullName shouldBe Defines.Symbol

    zNode.code shouldBe "z"
    zNode.argumentIndex shouldBe 3
    zNode.typeFullName shouldBe Defines.Symbol
  }

  "have correct structure for packing LHS in multiple assignment" in {
    val cpg = code("""
        |some_lhs,*pack_lhs = some_rhs,pack_rhs1, pack_rhs2
        |""".stripMargin)

    cpg.call.name("<operator>.assignment").size shouldBe 2
    val callNode1 = cpg.call.code("some_lhs = some_rhs").l.head
    callNode1.lineNumber shouldBe Some(2)
    callNode1.columnNumber shouldBe Some(19)
    val args1 = callNode1.argument.l
    args1.size shouldBe 2
    args1.head.code shouldBe "some_lhs"
    args1.tail.code.l.head shouldBe "some_rhs"

    val callNode2 = cpg.call.code("pack_lhs = pack_rhs1, pack_rhs2").l.head
    callNode2.lineNumber shouldBe Some(2)
    callNode2.columnNumber shouldBe Some(19)
    val args2 = callNode2.argument.l
    args2.size shouldBe 2
    args2.head.code shouldBe "pack_lhs"
    args2.tail.code.l.head shouldBe "pack_rhs1, pack_rhs2"
  }
}
