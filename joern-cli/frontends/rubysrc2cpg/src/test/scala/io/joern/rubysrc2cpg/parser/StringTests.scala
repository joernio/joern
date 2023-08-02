package io.joern.rubysrc2cpg.parser

class StringTests extends RubyParserAbstractTest {

  "A single-quoted string literal" when {

    "empty" should {
      val code = "''"

      "be parsed as a primary expression" in {
        printAst(_.primary(), code) shouldEqual
          """StringExpressionPrimary
            | SimpleStringExpression
            |  SingleQuotedStringLiteral
            |   ''""".stripMargin
      }
    }

    "separated by whitespace" should {
      val code = "'x' 'y'"

      "be parsed as a primary expression" in {
        printAst(_.primary(), code) shouldEqual
          """StringExpressionPrimary
            | ConcatenatedStringExpression
            |  SimpleStringExpression
            |   SingleQuotedStringLiteral
            |    'x'
            |  SimpleStringExpression
            |   SingleQuotedStringLiteral
            |    'y'""".stripMargin
      }
    }

    "separated by '\\\\n' " should {
      val code = """'x' \
          | 'y'""".stripMargin

      "be parsed as a primary expression" in {
        printAst(_.primary(), code) shouldEqual
          """StringExpressionPrimary
            | ConcatenatedStringExpression
            |  SimpleStringExpression
            |   SingleQuotedStringLiteral
            |    'x'
            |   \
            |  SimpleStringExpression
            |   SingleQuotedStringLiteral
            |    'y'""".stripMargin
      }
    }

    "separated by '\\\\n' twice" should {
      val code =
        """'x' \
          | 'y' \
          | 'z'""".stripMargin

      "be parsed as a primary expression" in {
        printAst(_.primary(), code) shouldEqual
          """StringExpressionPrimary
            | ConcatenatedStringExpression
            |  SimpleStringExpression
            |   SingleQuotedStringLiteral
            |    'x'
            |   \
            |  ConcatenatedStringExpression
            |   SimpleStringExpression
            |    SingleQuotedStringLiteral
            |     'y'
            |    \
            |   SimpleStringExpression
            |    SingleQuotedStringLiteral
            |     'z'""".stripMargin
      }
    }
  }

  "A non-expanded `%q` string literal" should {

    "be parsed as a primary expression" when {

      "it is empty and uses the `(`-`)` delimiters" in {
        val code = "%q()"
        printAst(_.primary(), code) shouldEqual
          """StringExpressionPrimary
            | SimpleStringExpression
            |  NonExpandedQuotedStringLiteral
            |   %q(
            |   )""".stripMargin
      }

      "it is empty and uses the `[`-`]` delimiters" in {
        val code = "%q[]"
        printAst(_.primary(), code) shouldEqual
          """StringExpressionPrimary
            | SimpleStringExpression
            |  NonExpandedQuotedStringLiteral
            |   %q[
            |   ]""".stripMargin
      }

      "it is empty and uses the `{`-`}` delimiters" in {
        val code = "%q{}"
        printAst(_.primary(), code) shouldEqual
          """StringExpressionPrimary
            | SimpleStringExpression
            |  NonExpandedQuotedStringLiteral
            |   %q{
            |   }""".stripMargin
      }

      "it is empty and uses the `<`-`>` delimiters" in {
        val code = "%q<>"
        printAst(_.primary(), code) shouldEqual
          """StringExpressionPrimary
            | SimpleStringExpression
            |  NonExpandedQuotedStringLiteral
            |   %q<
            |   >""".stripMargin
      }

      "it is empty and uses the `#` delimiters" in {
        val code = "%q##"
        printAst(_.primary(), code) shouldEqual
          """StringExpressionPrimary
            | SimpleStringExpression
            |  NonExpandedQuotedStringLiteral
            |   %q#
            |   #""".stripMargin
      }

      "it contains a single non-escaped character and uses the `(`-`)` delimiters" in {
        val code = "%q(x)"
        printAst(_.primary(), code) shouldEqual
          """StringExpressionPrimary
            | SimpleStringExpression
            |  NonExpandedQuotedStringLiteral
            |   %q(
            |   x
            |   )""".stripMargin
      }

      "it contains a single non-escaped character and uses the `[`-`]` delimiters" in {
        val code = "%q[x]"
        printAst(_.primary(), code) shouldEqual
          """StringExpressionPrimary
            | SimpleStringExpression
            |  NonExpandedQuotedStringLiteral
            |   %q[
            |   x
            |   ]""".stripMargin
      }

      "it contains a single non-escaped character and uses the `#` delimiters" in {
        val code = "%q#x#"
        printAst(_.primary(), code) shouldEqual
          """StringExpressionPrimary
            | SimpleStringExpression
            |  NonExpandedQuotedStringLiteral
            |   %q#
            |   x
            |   #""".stripMargin
      }

      "it contains a single escaped character and uses the `(`-`)` delimiters" in {
        val code = "%q(\\()"
        printAst(_.primary(), code) shouldEqual
          """StringExpressionPrimary
            | SimpleStringExpression
            |  NonExpandedQuotedStringLiteral
            |   %q(
            |   \(
            |   )""".stripMargin
      }

      "it contains a single escaped character and uses the `[`-`]` delimiters" in {
        val code = "%q[\\]]"
        printAst(_.primary(), code) shouldEqual
          """StringExpressionPrimary
            | SimpleStringExpression
            |  NonExpandedQuotedStringLiteral
            |   %q[
            |   \]
            |   ]""".stripMargin
      }

      "it contains a single escaped character and uses the `#` delimiters" in {
        val code = "%q#\\##"
        printAst(_.primary(), code) shouldEqual
          """StringExpressionPrimary
            | SimpleStringExpression
            |  NonExpandedQuotedStringLiteral
            |   %q#
            |   \#
            |   #""".stripMargin
      }

      "it contains a word and uses the `(`-`)` delimiters" in {
        val code = "%q(foo)"
        printAst(_.primary(), code) shouldEqual
          """StringExpressionPrimary
            | SimpleStringExpression
            |  NonExpandedQuotedStringLiteral
            |   %q(
            |   f
            |   o
            |   o
            |   )""".stripMargin
      }

      "it contains an empty nested string using the `(`-`)` delimiters" in {
        val code = "%q( () )"
        printAst(_.primary(), code) shouldEqual
          """StringExpressionPrimary
            | SimpleStringExpression
            |  NonExpandedQuotedStringLiteral
            |   %q(
            |   (
            |   )
            |   )""".stripMargin
      }

      "it contains an escaped single-character nested string using the `(`-`)` delimiters" in {
        val code = "%q( (\\)) )"
        printAst(_.primary(), code) shouldEqual
          """StringExpressionPrimary
            | SimpleStringExpression
            |  NonExpandedQuotedStringLiteral
            |   %q(
            |   (
            |   \)
            |   )
            |   )""".stripMargin
      }

      "it contains an escaped single-character nested string using the `<`-`>` delimiters" in {
        val code = "%q< <\\>> >"
        printAst(_.primary(), code) shouldEqual
          """StringExpressionPrimary
            | SimpleStringExpression
            |  NonExpandedQuotedStringLiteral
            |   %q<
            |   <
            |   \>
            |   >
            |   >""".stripMargin
      }
    }
  }

  "A double-quoted string literal" when {

    "empty" should {
      val code = "\"\""

      "be parsed as a primary expression" in {
        printAst(_.primary(), code) shouldEqual
          """StringExpressionPrimary
            | SimpleStringExpression
            |  DoubleQuotedStringLiteral
            |   "
            |   """".stripMargin
      }

      "separated by whitespace" should {
        val code = "\"x\" \"y\""

        "be parsed as a primary expression" in {
          printAst(_.primary(), code) shouldEqual
            """StringExpressionPrimary
              | ConcatenatedStringExpression
              |  SimpleStringExpression
              |   DoubleQuotedStringLiteral
              |    "
              |    x
              |    "
              |  SimpleStringExpression
              |   DoubleQuotedStringLiteral
              |    "
              |    y
              |    """".stripMargin
        }
      }

      "separated by '\\\\n'" should {
        val code =
          """"x" \
            | "y" """.stripMargin

        "be parsed as a primary expression" in {
          printAst(_.primary(), code) shouldEqual
            """StringExpressionPrimary
              | ConcatenatedStringExpression
              |  SimpleStringExpression
              |   DoubleQuotedStringLiteral
              |    "
              |    x
              |    "
              |   \
              |  SimpleStringExpression
              |   DoubleQuotedStringLiteral
              |    "
              |    y
              |    """".stripMargin
        }
      }
    }

    "containing text and a numeric literal interpolation" should {
      val code = """"text=#{1}""""

      "be parsed as primary expression" in {
        printAst(_.primary(), code) shouldEqual
          """StringExpressionPrimary
            | InterpolatedStringExpression
            |  StringInterpolation
            |   "
            |   text=
            |   InterpolatedStringSequence
            |    #{
            |    CompoundStatement
            |     Statements
            |      ExpressionOrCommandStatement
            |       ExpressionExpressionOrCommand
            |        PrimaryExpression
            |         LiteralPrimary
            |          NumericLiteralLiteral
            |           NumericLiteral
            |            UnsignedNumericLiteral
            |             1
            |    }
            |   """".stripMargin
      }
    }

    "containing two numeric literal interpolations" should {
      val code = """"#{1}#{2}""""

      "be parsed as primary expression" in {
        printAst(_.primary(), code) shouldEqual
          """StringExpressionPrimary
            | InterpolatedStringExpression
            |  StringInterpolation
            |   "
            |   InterpolatedStringSequence
            |    #{
            |    CompoundStatement
            |     Statements
            |      ExpressionOrCommandStatement
            |       ExpressionExpressionOrCommand
            |        PrimaryExpression
            |         LiteralPrimary
            |          NumericLiteralLiteral
            |           NumericLiteral
            |            UnsignedNumericLiteral
            |             1
            |    }
            |   InterpolatedStringSequence
            |    #{
            |    CompoundStatement
            |     Statements
            |      ExpressionOrCommandStatement
            |       ExpressionExpressionOrCommand
            |        PrimaryExpression
            |         LiteralPrimary
            |          NumericLiteralLiteral
            |           NumericLiteral
            |            UnsignedNumericLiteral
            |             2
            |    }
            |   """".stripMargin
      }
    }

    "separated by '\\\\n'" should {
      val code = """"x" \
          | "y" """.stripMargin

      "be parsed as a primary expression" in {
        printAst(_.primary(), code) shouldEqual
          """StringExpressionPrimary
              | ConcatenatedStringExpression
              |  SimpleStringExpression
              |   DoubleQuotedStringLiteral
              |    "
              |    x
              |    "
              |   \
              |  SimpleStringExpression
              |   DoubleQuotedStringLiteral
              |    "
              |    y
              |    """".stripMargin
      }

      "separated by '\\\\n' and containing a numeric interpolation" should {
        val code = """"#{10}" \
                     | "is a number."""".stripMargin

        "be parsed as a primary expression" in {
          printAst(_.primary(), code) shouldEqual
            """StringExpressionPrimary
              | ConcatenatedStringExpression
              |  InterpolatedStringExpression
              |   StringInterpolation
              |    "
              |    InterpolatedStringSequence
              |     #{
              |     CompoundStatement
              |      Statements
              |       ExpressionOrCommandStatement
              |        ExpressionExpressionOrCommand
              |         PrimaryExpression
              |          LiteralPrimary
              |           NumericLiteralLiteral
              |            NumericLiteral
              |             UnsignedNumericLiteral
              |              10
              |     }
              |    "
              |   \
              |  SimpleStringExpression
              |   DoubleQuotedStringLiteral
              |    "
              |    is a number.
              |    """".stripMargin
        }
      }
    }
  }
}
