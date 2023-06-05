package io.joern.rubysrc2cpg.passes.ast

import io.joern.rubysrc2cpg.passes.Defines
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
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
  }
}
