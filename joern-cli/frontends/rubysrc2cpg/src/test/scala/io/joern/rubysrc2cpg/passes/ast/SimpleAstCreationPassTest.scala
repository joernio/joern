package io.joern.rubysrc2cpg.passes.ast

import io.joern.rubysrc2cpg.passes.Defines
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language._

class SimpleAstCreationPassTest extends RubyCode2CpgFixture {

  "AST generation for simple fragments" should {

    "have correct structure for a single command call" in {
      val cpg = code("""puts 123""")

      val List(commandCall) = cpg.call.l
      val List(arg)         = commandCall.argument.isLiteral.l

      commandCall.code shouldBe "puts 123"
      commandCall.lineNumber shouldBe Some(1)

      arg.code shouldBe "123"
      arg.lineNumber shouldBe Some(1)
      arg.columnNumber shouldBe Some(5)
    }

    "have correct structure for an unsigned, decimal integer literal" in {
      val cpg           = code("123")
      val List(literal) = cpg.literal.l
      literal.typeFullName shouldBe Defines.Integer
      literal.code shouldBe "123"
      literal.lineNumber shouldBe Some(1)
      literal.columnNumber shouldBe Some(0)
    }

    "have correct structure for a +integer, decimal literal" in {
      val cpg           = code("+1")
      val List(literal) = cpg.literal.l
      literal.typeFullName shouldBe Defines.Integer
      literal.code shouldBe "+1"
      literal.lineNumber shouldBe Some(1)
      literal.columnNumber shouldBe Some(0)
    }

    "have correct structure for a -integer, decimal literal" in {
      val cpg           = code("-1")
      val List(literal) = cpg.literal.l
      literal.typeFullName shouldBe Defines.Integer
      literal.code shouldBe "-1"
      literal.lineNumber shouldBe Some(1)
      literal.columnNumber shouldBe Some(0)
    }

    "have correct structure for an unsigned, decimal float literal" in {
      val cpg           = code("3.14")
      val List(literal) = cpg.literal.l
      literal.typeFullName shouldBe Defines.Float
      literal.code shouldBe "3.14"
      literal.lineNumber shouldBe Some(1)
      literal.columnNumber shouldBe Some(0)
    }

    "have correct structure for a +float, decimal literal" in {
      val cpg           = code("+3.14")
      val List(literal) = cpg.literal.l
      literal.typeFullName shouldBe Defines.Float
      literal.code shouldBe "+3.14"
      literal.lineNumber shouldBe Some(1)
      literal.columnNumber shouldBe Some(0)
    }

    "have correct structure for a -float, decimal literal" in {
      val cpg           = code("-3.14")
      val List(literal) = cpg.literal.l
      literal.typeFullName shouldBe Defines.Float
      literal.code shouldBe "-3.14"
      literal.lineNumber shouldBe Some(1)
      literal.columnNumber shouldBe Some(0)
    }

    "have correct structure for an unsigned, decimal float literal with unsigned exponent" in {
      val cpg           = code("3e10")
      val List(literal) = cpg.literal.l
      literal.typeFullName shouldBe Defines.Float
      literal.code shouldBe "3e10"
      literal.lineNumber shouldBe Some(1)
      literal.columnNumber shouldBe Some(0)
    }

    "have correct structure for an unsigned, decimal float literal with -exponent" in {
      val cpg           = code("12e-10")
      val List(literal) = cpg.literal.l
      literal.typeFullName shouldBe Defines.Float
      literal.code shouldBe "12e-10"
      literal.lineNumber shouldBe Some(1)
      literal.columnNumber shouldBe Some(0)
    }

    "have correct structure for an unsigned, binary integer literal" in {
      val cpg           = code("0b01")
      val List(literal) = cpg.literal.l
      literal.typeFullName shouldBe Defines.Integer
      literal.code shouldBe "0b01"
      literal.lineNumber shouldBe Some(1)
      literal.columnNumber shouldBe Some(0)
    }

    "have correct structure for a -integer, binary literal" in {
      val cpg           = code("-0b01")
      val List(literal) = cpg.literal.l
      literal.typeFullName shouldBe Defines.Integer
      literal.code shouldBe "-0b01"
      literal.lineNumber shouldBe Some(1)
      literal.columnNumber shouldBe Some(0)
    }

    "have correct structure for a +integer, binary literal" in {
      val cpg           = code("+0b01")
      val List(literal) = cpg.literal.l
      literal.typeFullName shouldBe Defines.Integer
      literal.code shouldBe "+0b01"
      literal.lineNumber shouldBe Some(1)
      literal.columnNumber shouldBe Some(0)
    }

    "have correct structure for an unsigned, hexadecimal integer literal" in {
      val cpg           = code("0xabc")
      val List(literal) = cpg.literal.l
      literal.typeFullName shouldBe Defines.Integer
      literal.code shouldBe "0xabc"
      literal.lineNumber shouldBe Some(1)
      literal.columnNumber shouldBe Some(0)
    }

    "have correct structure for a -integer, hexadecimal literal" in {
      val cpg           = code("-0xa")
      val List(literal) = cpg.literal.l
      literal.typeFullName shouldBe Defines.Integer
      literal.code shouldBe "-0xa"
      literal.lineNumber shouldBe Some(1)
      literal.columnNumber shouldBe Some(0)
    }

    "have correct structure for a +integer, hexadecimal literal" in {
      val cpg           = code("+0xa")
      val List(literal) = cpg.literal.l
      literal.typeFullName shouldBe Defines.Integer
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
      val cpg        = code("puts self")
      val List(self) = cpg.identifier.l
      self.typeFullName shouldBe Defines.Object
      self.code shouldBe "self"
      self.lineNumber shouldBe Some(1)
      self.columnNumber shouldBe Some(5)
    }

    "have correct structure for `__FILE__` identifier" in {
      val cpg        = code("puts __FILE__")
      val List(file) = cpg.identifier.l
      file.typeFullName shouldBe Defines.String
      file.code shouldBe "__FILE__"
      file.lineNumber shouldBe Some(1)
      file.columnNumber shouldBe Some(5)
    }

    "have correct structure for `__LINE__` identifier" in {
      val cpg        = code("puts __LINE__")
      val List(line) = cpg.identifier.l
      line.typeFullName shouldBe Defines.Integer
      line.code shouldBe "__LINE__"
      line.lineNumber shouldBe Some(1)
      line.columnNumber shouldBe Some(5)
    }

    "have correct structure for `__ENCODING__` identifier" in {
      val cpg            = code("puts __ENCODING__")
      val List(encoding) = cpg.identifier.l
      encoding.typeFullName shouldBe Defines.Encoding
      encoding.code shouldBe "__ENCODING__"
      encoding.lineNumber shouldBe Some(1)
      encoding.columnNumber shouldBe Some(5)
    }

    "have correct structure for a single-line double-quoted string literal" in {
      val cpg           = code("\"hello\"")
      val List(literal) = cpg.literal.l
      literal.typeFullName shouldBe Defines.String
      literal.code shouldBe "\"hello\""
      literal.lineNumber shouldBe Some(1)
      literal.columnNumber shouldBe Some(0)
    }

    "have correct structure for a single-line single-quoted string literal" in {
      val cpg           = code("'hello'")
      val List(literal) = cpg.literal.l
      literal.typeFullName shouldBe Defines.String
      literal.code shouldBe "'hello'"
      literal.lineNumber shouldBe Some(1)
      literal.columnNumber shouldBe Some(0)
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

    "have correct structure for a single-line regular expression literal" in {
      val cpg           = code("/(eu|us)/")
      val List(literal) = cpg.literal.l
      literal.typeFullName shouldBe Defines.Regexp
      literal.code shouldBe "/(eu|us)/"
      literal.lineNumber shouldBe Some(1)
      literal.columnNumber shouldBe Some(0)
    }

    "have correct structure for an empty regular expression literal used as the second argument to a call" in {
      val cpg           = code("puts(x, //)")
      val List(literal) = cpg.literal.l
      literal.typeFullName shouldBe Defines.Regexp
      literal.code shouldBe "//"
      literal.lineNumber shouldBe Some(1)
      literal.columnNumber shouldBe Some(8)
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
      callNode.columnNumber shouldBe Some(1)
    }

    "have correct structure for a non-inclusive range expression" in {
      val cpg            = code("1...10")
      val List(callNode) = cpg.call.name(Operators.range).l
      callNode.code shouldBe "1...10"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(1)
    }

    "have correct structure for a relational expression" in {
      val cpg            = code("x<y")
      val List(callNode) = cpg.call.name(Operators.lessThan).l
      callNode.code shouldBe "x<y"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(1)
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
      val cpg            = code("puts \"something\"")
      val List(callNode) = cpg.call.l
      callNode.code shouldBe "puts \"something\""
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(0)
    }

    "have correct structure for a logical and expression" in {
      val cpg            = code("x & y")
      val List(callNode) = cpg.call.name(Operators.logicalAnd).l
      callNode.code shouldBe "x & y"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(2)
    }

    "have correct structure for a logical or with bar expression" in {
      val cpg            = code("x | y")
      val List(callNode) = cpg.call.name(Operators.logicalOr).l
      callNode.code shouldBe "x | y"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(2)
    }

    "have correct structure for a logical or with carat expression" in {
      val cpg            = code("x ^ y")
      val List(callNode) = cpg.call.name(Operators.logicalOr).l
      callNode.code shouldBe "x ^ y"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(2)
    }

    "have correct structure for a assignment expression" in {
      val cpg            = code("x = y")
      val List(callNode) = cpg.call.name(Operators.assignment).l
      callNode.code shouldBe "="
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(2)
    }

    "have correct structure for a equals expression" in {
      val cpg            = code("x == y")
      val List(callNode) = cpg.call.name(Operators.equals).l
      callNode.code shouldBe "x == y"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(2)
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
      callNode.columnNumber shouldBe Some(2)
    }

    "have correct structure for a shift left expression" in {
      val cpg            = code("x << y")
      val List(callNode) = cpg.call.name(Operators.shiftLeft).l
      callNode.code shouldBe "x << y"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(2)
    }

    "have correct structure for a compare expression" in {
      val cpg            = code("x <=> y")
      val List(callNode) = cpg.call.name(Operators.compare).l
      callNode.code shouldBe "x <=> y"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(2)
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

      val List(callNode) = cpg.call.name("<operator>.scopeResolution").l
      callNode.code shouldBe "::SomeConstant"
      callNode.lineNumber shouldBe Some(3)
      callNode.columnNumber shouldBe Some(0)

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

      val List(identifierNode) = cpg.identifier.name("arg").l
      identifierNode.code shouldBe "arg"
      identifierNode.lineNumber shouldBe Some(2)
      identifierNode.columnNumber shouldBe Some(5)
    }

    "have correct structure for a hash initialisation" in {
      val cpg       = code("hashMap = {\"k1\" => 1, \"k2\" => 2}")
      val callNodes = cpg.call.name("<operator>.keyValueAssociation").l
      callNodes.size shouldBe 2
      callNodes.head.code shouldBe "\"k1\" => 1"
      callNodes.head.lineNumber shouldBe Some(1)
      callNodes.head.columnNumber shouldBe Some(16)
    }

    "have correct structure for defined expression" in {
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

    "have correct structure for chainedInvocationWithoutArgumentsPrimary" in {
      val cpg = code("object::foo do\nputs \"right here\"\nend")

      val List(callNode1) = cpg.call.name("foo").l
      callNode1.code shouldBe "object::foo do\nputs \"right here\"\nend"
      callNode1.lineNumber shouldBe Some(1)
      callNode1.columnNumber shouldBe Some(6)

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

    "have correct structure in the body of module definition" in {
      val cpg = code("module MyModule\ndef some_method\nend\nprivate\nend")

      val List(callNode) = cpg.method.name("some_method").l
      callNode.code shouldBe "def some_method\nend"
      callNode.lineNumber shouldBe Some(2)
      callNode.columnNumber shouldBe Some(4)

      cpg.identifier.name("private").l.size shouldBe 0
    }

    "have correct structure for undef" in {
      val cpg = code("undef method1,method2")

      val List(callNode) = cpg.call.name("<operator>.undef").l
      callNode.code shouldBe "undef method1,method2"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(0)
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
  }
}
