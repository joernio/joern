package io.joern.rubysrc2cpg.parser

class StringTests extends RubyParserAbstractTest {

  "A single-quoted string literal" when {

    "empty" should {
      val code = "''"

      "be parsed as a primary expression" in {
        printAst(_.primary(), code) shouldEqual
          """LiteralPrimary
            | SingleQuotedStringLiteral
            |  ''""".stripMargin
      }
    }
  }

  "A double-quoted string literal" when {

    "empty" should {
      val code = "\"\""

      "be parsed as a primary expression" in {
        printAst(_.primary(), code) shouldEqual
          """LiteralPrimary
            | DoubleQuotedStringLiteral
            |  "
            |  """".stripMargin
      }
    }

    "containing text and a numeric literal interpolation" should {
      val code = """"text=#{1}""""

      "be parsed as primary expression" in {
        printAst(_.primary(), code) shouldEqual
          """StringInterpolationPrimary
            | StringInterpolation
            |  "
            |  text=
            |  InterpolatedStringSequence
            |   #{
            |   CompoundStatement
            |    Statements
            |     ExpressionOrCommandStatement
            |      ExpressionExpressionOrCommand
            |       PrimaryExpression
            |        LiteralPrimary
            |         NumericLiteralLiteral
            |          NumericLiteral
            |           UnsignedNumericLiteral
            |            1
            |   }
            |  """".stripMargin
      }
    }

    "containing two numeric literal interpolations" should {
      val code = """"#{1}#{2}""""

      "be parsed as primary expression" in {
        printAst(_.primary(), code) shouldEqual
          """StringInterpolationPrimary
            | StringInterpolation
            |  "
            |  InterpolatedStringSequence
            |   #{
            |   CompoundStatement
            |    Statements
            |     ExpressionOrCommandStatement
            |      ExpressionExpressionOrCommand
            |       PrimaryExpression
            |        LiteralPrimary
            |         NumericLiteralLiteral
            |          NumericLiteral
            |           UnsignedNumericLiteral
            |            1
            |   }
            |  InterpolatedStringSequence
            |   #{
            |   CompoundStatement
            |    Statements
            |     ExpressionOrCommandStatement
            |      ExpressionExpressionOrCommand
            |       PrimaryExpression
            |        LiteralPrimary
            |         NumericLiteralLiteral
            |          NumericLiteral
            |           UnsignedNumericLiteral
            |            2
            |   }
            |  """".stripMargin
      }
    }
  }
}
