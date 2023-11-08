package io.joern.rubysrc2cpg.deprecated.parser

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
            |  ConcatenatedStringExpression
            |   SimpleStringExpression
            |    SingleQuotedStringLiteral
            |     'y'
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
          """QuotedStringExpressionPrimary
            | NonExpandedQuotedStringLiteral
            |  %q(
            |  )""".stripMargin
      }

      "it is empty and uses the `[`-`]` delimiters" in {
        val code = "%q[]"
        printAst(_.primary(), code) shouldEqual
          """QuotedStringExpressionPrimary
            | NonExpandedQuotedStringLiteral
            |  %q[
            |  ]""".stripMargin
      }

      "it is empty and uses the `{`-`}` delimiters" in {
        val code = "%q{}"
        printAst(_.primary(), code) shouldEqual
          """QuotedStringExpressionPrimary
            | NonExpandedQuotedStringLiteral
            |  %q{
            |  }""".stripMargin
      }

      "it is empty and uses the `<`-`>` delimiters" in {
        val code = "%q<>"
        printAst(_.primary(), code) shouldEqual
          """QuotedStringExpressionPrimary
            | NonExpandedQuotedStringLiteral
            |  %q<
            |  >""".stripMargin
      }

      "it is empty and uses the `#` delimiters" in {
        val code = "%q##"
        printAst(_.primary(), code) shouldEqual
          """QuotedStringExpressionPrimary
            | NonExpandedQuotedStringLiteral
            |  %q#
            |  #""".stripMargin
      }

      "it contains a single non-escaped character and uses the `(`-`)` delimiters" in {
        val code = "%q(x)"
        printAst(_.primary(), code) shouldEqual
          """QuotedStringExpressionPrimary
            | NonExpandedQuotedStringLiteral
            |  %q(
            |  x
            |  )""".stripMargin
      }

      "it contains a single non-escaped character and uses the `[`-`]` delimiters" in {
        val code = "%q[x]"
        printAst(_.primary(), code) shouldEqual
          """QuotedStringExpressionPrimary
            | NonExpandedQuotedStringLiteral
            |  %q[
            |  x
            |  ]""".stripMargin
      }

      "it contains a single non-escaped character and uses the `#` delimiters" in {
        val code = "%q#x#"
        printAst(_.primary(), code) shouldEqual
          """QuotedStringExpressionPrimary
            | NonExpandedQuotedStringLiteral
            |  %q#
            |  x
            |  #""".stripMargin
      }

      "it contains a single escaped character and uses the `(`-`)` delimiters" in {
        val code = "%q(\\()"
        printAst(_.primary(), code) shouldEqual
          """QuotedStringExpressionPrimary
            | NonExpandedQuotedStringLiteral
            |  %q(
            |  \(
            |  )""".stripMargin
      }

      "it contains a single escaped character and uses the `[`-`]` delimiters" in {
        val code = "%q[\\]]"
        printAst(_.primary(), code) shouldEqual
          """QuotedStringExpressionPrimary
            | NonExpandedQuotedStringLiteral
            |  %q[
            |  \]
            |  ]""".stripMargin
      }

      "it contains a single escaped character and uses the `#` delimiters" in {
        val code = "%q#\\##"
        printAst(_.primary(), code) shouldEqual
          """QuotedStringExpressionPrimary
            | NonExpandedQuotedStringLiteral
            |  %q#
            |  \#
            |  #""".stripMargin
      }

      "it contains a word and uses the `(`-`)` delimiters" in {
        val code = "%q(foo)"
        printAst(_.primary(), code) shouldEqual
          """QuotedStringExpressionPrimary
            | NonExpandedQuotedStringLiteral
            |  %q(
            |  foo
            |  )""".stripMargin
      }

      "it contains an empty nested string using the `(`-`)` delimiters" in {
        val code = "%q( () )"
        printAst(_.primary(), code) shouldEqual
          """QuotedStringExpressionPrimary
            | NonExpandedQuotedStringLiteral
            |  %q(
            |   () 
            |  )""".stripMargin
      }

      "it contains an escaped single-character nested string using the `(`-`)` delimiters" in {
        val code = "%q( (\\)) )"
        printAst(_.primary(), code) shouldEqual
          """QuotedStringExpressionPrimary
            | NonExpandedQuotedStringLiteral
            |  %q(
            |   (\)) 
            |  )""".stripMargin
      }

      "it contains an escaped single-character nested string using the `<`-`>` delimiters" in {
        val code = "%q< <\\>> >"
        printAst(_.primary(), code) shouldEqual
          """QuotedStringExpressionPrimary
            | NonExpandedQuotedStringLiteral
            |  %q<
            |   <\>> 
            |  >""".stripMargin
      }
    }
  }

  "An expanded `%Q` string literal" when {

    "empty" should {
      val code = "%Q()"

      "be parsed as a primary expression" in {
        printAst(_.primary(), code) shouldEqual
          """QuotedStringExpressionPrimary
            | ExpandedQuotedStringLiteral
            |  %Q(
            |  )""".stripMargin
      }
    }

    "containing text and a numeric literal interpolation" should {
      val code = "%Q{text=#{1}}"

      "be parsed as primary expression" in {
        printAst(_.primary(), code) shouldEqual
          """QuotedStringExpressionPrimary
            | ExpandedQuotedStringLiteral
            |  %Q{
            |  text=
            |  DelimitedStringInterpolation
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
            |  }""".stripMargin
      }
    }

    "containing two consecutive numeric literal interpolations" should {
      val code = "%Q[#{1}#{2}]"

      "be parsed as primary expression" in {
        printAst(_.primary(), code) shouldEqual
          """QuotedStringExpressionPrimary
            | ExpandedQuotedStringLiteral
            |  %Q[
            |  DelimitedStringInterpolation
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
            |  DelimitedStringInterpolation
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
            |  ]""".stripMargin
      }
    }

  }

  "An expanded `%(` string literal" when {

    "empty" should {
      val code = "%()"

      "be parsed as a primary expression" in {
        printAst(_.primary(), code) shouldEqual
          """QuotedStringExpressionPrimary
            | ExpandedQuotedStringLiteral
            |  %(
            |  )""".stripMargin
      }
    }

    "containing text and a numeric literal interpolation" should {
      val code = "%(text=#{1})"

      "be parsed as primary expression" in {
        printAst(_.primary(), code) shouldEqual
          """QuotedStringExpressionPrimary
            | ExpandedQuotedStringLiteral
            |  %(
            |  text=
            |  DelimitedStringInterpolation
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
            |  )""".stripMargin
      }
    }

    "containing two consecutive numeric literal interpolations" should {
      val code = "%(#{1}#{2})"

      "be parsed as primary expression" in {
        printAst(_.primary(), code) shouldEqual
          """QuotedStringExpressionPrimary
            | ExpandedQuotedStringLiteral
            |  %(
            |  DelimitedStringInterpolation
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
            |  DelimitedStringInterpolation
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
            |  )""".stripMargin
      }
    }

    "used as the argument to a `puts` command" should {

      "be parsed as a statement" in {
        val code = "puts %()"
        printAst(_.statement(), code) shouldEqual
          """ExpressionOrCommandStatement
            | InvocationExpressionOrCommand
            |  SingleCommandOnlyInvocationWithoutParentheses
            |   SimpleMethodCommand
            |    MethodIdentifier
            |     puts
            |    ArgumentsWithoutParentheses
            |     Arguments
            |      ExpressionArgument
            |       PrimaryExpression
            |        QuotedStringExpressionPrimary
            |         ExpandedQuotedStringLiteral
            |          %(
            |          )""".stripMargin
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
              |  SimpleStringExpression
              |   DoubleQuotedStringLiteral
              |    "
              |    is a number.
              |    """".stripMargin
        }
      }
    }
  }

  "An expanded `%x` external command literal" when {

    "empty" should {
      val code = "%x//"

      "be parsed as a primary expression" in {
        printAst(_.primary(), code) shouldEqual
          """QuotedStringExpressionPrimary
            | ExpandedExternalCommandLiteral
            |  %x/
            |  /""".stripMargin
      }
    }

    "containing text and a string literal interpolation" should {
      val code = "%x{l#{'s'}}"

      "be parsed as primary expression" in {
        printAst(_.primary(), code) shouldEqual
          """QuotedStringExpressionPrimary
            | ExpandedExternalCommandLiteral
            |  %x{
            |  l
            |  DelimitedStringInterpolation
            |   #{
            |   CompoundStatement
            |    Statements
            |     ExpressionOrCommandStatement
            |      ExpressionExpressionOrCommand
            |       PrimaryExpression
            |        StringExpressionPrimary
            |         SimpleStringExpression
            |          SingleQuotedStringLiteral
            |           's'
            |   }
            |  }""".stripMargin
      }
    }
  }

  "A HERE_DOCs expression" when {

    "used to generate a single string" should {
      val code =
        """<<-SQL
          |SELECT * FROM food
          |WHERE healthy = true
          |SQL
          |""".stripMargin

      "be parsed as a primary expression" in {
        printAst(_.primary(), code) shouldEqual
          """LiteralPrimary
            | HereDocLiteral
            |  <<-SQL
            |SELECT * FROM food
            |WHERE healthy = true
            |SQL""".stripMargin
      }

    }

    "used to generate a single string parameter for a function call" should {
      val code =
        """foo(<<-SQL)
          |SELECT * FROM food
          |WHERE healthy = true
          |SQL
          |""".stripMargin

      // TODO: The rest of the HERE_DOC should probably be parsed somehow
      "be parsed as a primary expression" in {
        printAst(_.primary(), code) shouldEqual
          """InvocationWithParenthesesPrimary
            | MethodIdentifier
            |  foo
            | ArgsOnlyArgumentsWithParentheses
            |  (
            |  Arguments
            |   HereDocArgument
            |    <<-SQL
            |  )""".stripMargin
      }
    }
  }
}
