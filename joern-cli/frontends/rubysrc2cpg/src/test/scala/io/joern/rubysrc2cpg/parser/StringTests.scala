package io.joern.rubysrc2cpg.parser

class StringTests extends RubyParserAbstractTest {

  "A single-quoted string literal" when {

    "empty" should {
      val code = "''"

      "be parsed as a primary expression" in {
        printAst(_.primary(), code) shouldEqual
          """LiteralPrimary
            | StringLiteralLiteral
            |  SingleQuotedStringLiteral
            |   ''""".stripMargin
      }
    }

    "separated by whitespace" should {
      val code = "'x' 'y'"

      "be parsed as a literal expression" in {
        printAst(_.literal(), code) shouldEqual
          """StringLiteralLiteral
            | ConcatenationStringLiteral
            |  SingleQuotedStringLiteral
            |   'x'
            |  SingleQuotedStringLiteral
            |   'y'""".stripMargin
      }
    }

    "separated by '\\\n' " should {
      val code = """'x' \
          | 'y'""".stripMargin

      "be parsed as a literal expression" in {
        printAst(_.literal(), code) shouldEqual
          """StringLiteralLiteral
            | ConcatenationStringLiteral
            |  SingleQuotedStringLiteral
            |   'x'
            |   \
            |  SingleQuotedStringLiteral
            |   'y'""".stripMargin
      }
    }
  }

  "A double-quoted string literal" when {

    "empty" should {
      val code = "\"\""

      "be parsed as a primary expression" in {
        printAst(_.primary(), code) shouldEqual
          """LiteralPrimary
            | StringLiteralLiteral
            |  DoubleQuotedStringLiteral
            |   "
            |   """".stripMargin
      }

      "separated by whitespace" should {
        val code = "\"x\" \"y\""

        "be parsed as a literal expression" in {
          printAst(_.literal(), code) shouldEqual
            """StringLiteralLiteral
              | ConcatenationStringLiteral
              |  DoubleQuotedStringLiteral
              |   "
              |   x
              |   "
              |  DoubleQuotedStringLiteral
              |   "
              |   y
              |   """".stripMargin
        }
      }

      "separated by '\\\n' " should {
        val code =
          """"x" \
            | "y" """.stripMargin

        "be parsed as a literal expression" in {
          printAst(_.literal(), code) shouldEqual
            """StringLiteralLiteral
              | ConcatenationStringLiteral
              |  DoubleQuotedStringLiteral
              |   "
              |   x
              |   "
              |   \
              |  DoubleQuotedStringLiteral
              |   "
              |   y
              |   """".stripMargin
        }
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

    "separated by '\\\n' and containing a numeric literal interpolation" should {
      val code = """"#{10} is a" \
                   |"number"""".stripMargin

      "be parsed as a literal expression" in {
        printAst(_.literal(), code) shouldEqual
          """StringLiteralLiteral
          | DoubleQuotedStringLiteral
          |  "
          |  #{
          |  10
          |  }
          |   is a
          |  "
          |   \
          |  "
          |  number
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
