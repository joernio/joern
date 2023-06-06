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

    "have correct structure for an unsigned, decimal integer literal" ignore {
      val cpg           = code("123")
      val List(literal) = cpg.literal.l
      literal.typeFullName shouldBe Defines.Integer
      literal.code shouldBe "123"
      literal.lineNumber shouldBe Some(1)
      literal.columnNumber shouldBe Some(1)
    }

    "have correct structure for a +integer, decimal literal" ignore {
      val cpg           = code("+1")
      val List(literal) = cpg.literal.l
      literal.typeFullName shouldBe Defines.Integer
      literal.code shouldBe "+1"
      literal.lineNumber shouldBe Some(1)
      literal.columnNumber shouldBe Some(1)
    }

    "have correct structure for a -integer, decimal literal" ignore {
      val cpg           = code("-1")
      val List(literal) = cpg.literal.l
      literal.typeFullName shouldBe Defines.Integer
      literal.code shouldBe "-1"
      literal.lineNumber shouldBe Some(1)
      literal.columnNumber shouldBe Some(1)
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

    "have correct code for a single left had side call" in {
      val cpg            = code("array[n] = 10")
      val List(callNode) = cpg.call.name(Operators.indexAccess).l
      callNode.code shouldBe "array[n]"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(5)
    }

    "have correct code for a binary expression" in {
      val cpg            = code("x+y")
      val List(callNode) = cpg.call.name(Operators.addition).l
      callNode.code shouldBe "x+y"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(1)
    }

    "have correct code for a not expression" in {
      val cpg            = code("not y")
      val List(callNode) = cpg.call.name(Operators.not).l
      callNode.code shouldBe "not y"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(0)
    }

    "have correct code for a power expression" in {
      val cpg            = code("x**y")
      val List(callNode) = cpg.call.name(Operators.exponentiation).l
      callNode.code shouldBe "x**y"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(1)
    }

    "have correct code for a inclusive range expression" in {
      val cpg            = code("1..10")
      val List(callNode) = cpg.call.name(Operators.range).l
      callNode.code shouldBe "1..10"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(1)
    }

    "have correct code for a non-inclusive range expression" in {
      val cpg            = code("1...10")
      val List(callNode) = cpg.call.name(Operators.range).l
      callNode.code shouldBe "1...10"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(1)
    }

    "have correct code for a relational expression" in {
      val cpg            = code("x<y")
      val List(callNode) = cpg.call.name(Operators.lessThan).l
      callNode.code shouldBe "x<y"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(1)
    }

    "have correct code for a unary exclamation expression" in {
      val cpg            = code("!y")
      val List(callNode) = cpg.call.name(Operators.not).l
      callNode.code shouldBe "!y"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(0)
    }

    "have correct code for a unary tilde expression" in {
      val cpg            = code("~y")
      val List(callNode) = cpg.call.name(Operators.not).l
      callNode.code shouldBe "~y"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(0)
    }

    "have correct code for a unary plus expression" in {
      val cpg            = code("+y")
      val List(callNode) = cpg.call.name(Operators.plus).l
      callNode.code shouldBe "+y"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(0)
    }

    "have correct code for a unary minus expression" in {
      val cpg            = code("-y")
      val List(callNode) = cpg.call.name(Operators.minus).l
      callNode.code shouldBe "-y"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(0)
    }

    "have correct code for a call node" in {
      val cpg            = code("puts \"something\"")
      val List(callNode) = cpg.call.l
      callNode.code shouldBe "puts \"something\""
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(0)
    }

    "have correct code for a logical and expression" in {
      val cpg            = code("x & y")
      val List(callNode) = cpg.call.name(Operators.logicalAnd).l
      callNode.code shouldBe "x & y"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(2)
    }

    "have correct code for a logical or with bar expression" in {
      val cpg            = code("x | y")
      val List(callNode) = cpg.call.name(Operators.logicalOr).l
      callNode.code shouldBe "x | y"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(2)
    }

    "have correct code for a logical or with carat expression" in {
      val cpg            = code("x ^ y")
      val List(callNode) = cpg.call.name(Operators.logicalOr).l
      callNode.code shouldBe "x ^ y"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(2)
    }

    "have correct code for a assignment expression" in {
      val cpg            = code("x = y")
      val List(callNode) = cpg.call.name(Operators.assignment).l
      callNode.code shouldBe "="
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(2)
    }

    "have correct code for a equals expression" in {
      val cpg            = code("x == y")
      val List(callNode) = cpg.call.name(Operators.equals).l
      callNode.code shouldBe "x == y"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(2)
    }

    "have correct code for a division expression" in {
      val cpg            = code("x / y")
      val List(callNode) = cpg.call.name(Operators.division).l
      callNode.code shouldBe "x / y"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(2)
    }

    "have correct code for a modulo expression" in {
      val cpg            = code("x % y")
      val List(callNode) = cpg.call.name(Operators.modulo).l
      callNode.code shouldBe "x % y"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(2)
    }

    "have correct code for a shift right expression" in {
      val cpg            = code("x >> y")
      val List(callNode) = cpg.call.name(Operators.logicalShiftRight).l
      callNode.code shouldBe "x >> y"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(2)
    }

    "have correct code for a shift left expression" in {
      val cpg            = code("x << y")
      val List(callNode) = cpg.call.name(Operators.shiftLeft).l
      callNode.code shouldBe "x << y"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(2)
    }

    "have correct code for a compare expression" in {
      val cpg            = code("x <=> y")
      val List(callNode) = cpg.call.name(Operators.compare).l
      callNode.code shouldBe "x <=> y"
      callNode.lineNumber shouldBe Some(1)
      callNode.columnNumber shouldBe Some(2)
    }
  }
}
