package io.joern.rubysrc2cpg.parser

class StringTests extends RubyParserAbstractTest {

  "Empty string literals" should {
    val singleQuotedExample = "''"
    val doubleQuotedExample = "\"\""

    "be parsed as primary expressions" in {
      prettyPrint(_.primary(), singleQuotedExample) shouldEqual
        """LiteralPrimary
          | Literal
          |  ''
          |""".stripMargin

      prettyPrint(_.primary(), doubleQuotedExample) shouldEqual
        """LiteralPrimary
          | Literal
          |  "
          |  "
          |""".stripMargin
    }
  }

  "One-hole string interpolation containing text and a numeric literal" should {
    val code = """"text=#{1}""""

    "be parsed as a primary expression" in {
      prettyPrint(_.primary(), code) shouldEqual
        """StringInterpolationPrimary
          | StringInterpolation
          |  "
          |  text=
          |  Interpolation
          |   #{
          |   CompoundStatement
          |    Statements
          |     ExpressionOrCommandStatement
          |      ExpressionExpressionOrCommand
          |       PrimaryExpression
          |        LiteralPrimary
          |         Literal
          |          NumericLiteral
          |           UnsignedNumericLiteral
          |            1
          |   }
          |  "
          |""".stripMargin
    }
  }

  "Two-hole string interpolation containing numeric literals" should {
    val code = """"#{1}#{2}""""

    "be parsed as a primary expression" in {
      prettyPrint(_.primary(), code) shouldBe
        """StringInterpolationPrimary
          | StringInterpolation
          |  "
          |  Interpolation
          |   #{
          |   CompoundStatement
          |    Statements
          |     ExpressionOrCommandStatement
          |      ExpressionExpressionOrCommand
          |       PrimaryExpression
          |        LiteralPrimary
          |         Literal
          |          NumericLiteral
          |           UnsignedNumericLiteral
          |            1
          |   }
          |  Interpolation
          |   #{
          |   CompoundStatement
          |    Statements
          |     ExpressionOrCommandStatement
          |      ExpressionExpressionOrCommand
          |       PrimaryExpression
          |        LiteralPrimary
          |         Literal
          |          NumericLiteral
          |           UnsignedNumericLiteral
          |            2
          |   }
          |  "
          |""".stripMargin
    }
  }
}
