package io.joern.rubysrc2cpg.deprecated.parser

class RegexTests extends RubyParserAbstractTest {

  "An empty regex literal" when {

    "by itself" should {
      val code = "//"

      "be parsed as a primary expression" in {
        printAst(_.primary(), code) shouldEqual
          """LiteralPrimary
            | RegularExpressionLiteral
            |  /
            |  /""".stripMargin
      }
    }

    "on the RHS of an assignment" should {
      val code = "x = //"

      "be parsed as a single assignment to a regex literal" in {
        printAst(_.expressionOrCommand(), code) shouldEqual
          """ExpressionExpressionOrCommand
            | SingleAssignmentExpression
            |  VariableIdentifierOnlySingleLeftHandSide
            |   VariableIdentifier
            |    x
            |  =
            |  MultipleRightHandSide
            |   ExpressionOrCommands
            |    ExpressionExpressionOrCommand
            |     PrimaryExpression
            |      LiteralPrimary
            |       RegularExpressionLiteral
            |        /
            |        /""".stripMargin
      }
    }

    "as the argument to a `puts` command" should {
      val code = "puts //"

      "be parsed as such" in {
        printAst(_.expressionOrCommand(), code) shouldEqual
          """InvocationExpressionOrCommand
            | SingleCommandOnlyInvocationWithoutParentheses
            |  SimpleMethodCommand
            |   MethodIdentifier
            |    puts
            |   ArgumentsWithoutParentheses
            |    Arguments
            |     ExpressionArgument
            |      PrimaryExpression
            |       LiteralPrimary
            |        RegularExpressionLiteral
            |         /
            |         /""".stripMargin
      }
    }

    "as the sole argument to a parenthesized invocation" should {
      val code = "puts(//)"

      "be parsed as such" in {
        printAst(_.expressionOrCommand(), code) shouldEqual
          """ExpressionExpressionOrCommand
            | PrimaryExpression
            |  InvocationWithParenthesesPrimary
            |   MethodIdentifier
            |    puts
            |   ArgsOnlyArgumentsWithParentheses
            |    (
            |    Arguments
            |     ExpressionArgument
            |      PrimaryExpression
            |       LiteralPrimary
            |        RegularExpressionLiteral
            |         /
            |         /
            |    )""".stripMargin
      }
    }

    "as the second argument to a parenthesized invocation" should {
      val code = "puts(1, //)"

      "be parsed as such" in {
        printAst(_.expressionOrCommand(), code) shouldEqual
          """ExpressionExpressionOrCommand
            | PrimaryExpression
            |  InvocationWithParenthesesPrimary
            |   MethodIdentifier
            |    puts
            |   ArgsOnlyArgumentsWithParentheses
            |    (
            |    Arguments
            |     ExpressionArgument
            |      PrimaryExpression
            |       LiteralPrimary
            |        NumericLiteralLiteral
            |         NumericLiteral
            |          UnsignedNumericLiteral
            |           1
            |     ,
            |     ExpressionArgument
            |      PrimaryExpression
            |       LiteralPrimary
            |        RegularExpressionLiteral
            |         /
            |         /
            |    )""".stripMargin
      }
    }

    "used in a `when` clause" should {
      val code =
        """case foo
            |      when /^ch_/
            |        bar
            |end""".stripMargin

      "be parsed as such" in {
        printAst(_.primary(), code) shouldEqual
          """CaseExpressionPrimary
              | CaseExpression
              |  case
              |  ExpressionExpressionOrCommand
              |   PrimaryExpression
              |    VariableReferencePrimary
              |     VariableIdentifierVariableReference
              |      VariableIdentifier
              |       foo
              |  WhenClause
              |   when
              |   WhenArgument
              |    Expressions
              |     PrimaryExpression
              |      LiteralPrimary
              |       RegularExpressionLiteral
              |        /
              |        ^ch_
              |        /
              |   ThenClause
              |    CompoundStatement
              |     Statements
              |      ExpressionOrCommandStatement
              |       ExpressionExpressionOrCommand
              |        PrimaryExpression
              |         VariableReferencePrimary
              |          VariableIdentifierVariableReference
              |           VariableIdentifier
              |            bar
              |  end""".stripMargin
      }

      "used in a `unless` clause" should {
        val code =
          """unless /\A([^@\s]+)@((?:[-a-z0-9]+\.)+[a-z]{2,})\z/i.match?(value)
            |end""".stripMargin

        "be parsed as such" in {
          printAst(_.primary(), code) shouldEqual
            """UnlessExpressionPrimary
              | UnlessExpression
              |  unless
              |  ExpressionExpressionOrCommand
              |   PrimaryExpression
              |    ChainedInvocationPrimary
              |     LiteralPrimary
              |      RegularExpressionLiteral
              |       /
              |       \A([^@\s]+)@((?:[-a-z0-9]+\.)+[a-z]{2,})\z
              |       /i
              |     .
              |     MethodName
              |      MethodIdentifier
              |       MethodOnlyIdentifier
              |        match
              |        ?
              |     ArgsOnlyArgumentsWithParentheses
              |      (
              |      Arguments
              |       ExpressionArgument
              |        PrimaryExpression
              |         VariableReferencePrimary
              |          VariableIdentifierVariableReference
              |           VariableIdentifier
              |            value
              |      )
              |  ThenClause
              |   CompoundStatement
              |  end""".stripMargin
        }
      }
    }
  }

  "A non-interpolated regex literal" when {

    "by itself" should {
      val code = "/(eu|us)/"

      "be parsed as a primary expression" in {
        printAst(_.primary(), code) shouldEqual
          """LiteralPrimary
            | RegularExpressionLiteral
            |  /
            |  (eu|us)
            |  /""".stripMargin
      }
    }

    "on the RHS of an assignment" should {
      val code = "x = /(eu|us)/"

      "be parsed as a single assignment to a regex literal" in {
        printAst(_.expressionOrCommand(), code) shouldEqual
          """ExpressionExpressionOrCommand
            | SingleAssignmentExpression
            |  VariableIdentifierOnlySingleLeftHandSide
            |   VariableIdentifier
            |    x
            |  =
            |  MultipleRightHandSide
            |   ExpressionOrCommands
            |    ExpressionExpressionOrCommand
            |     PrimaryExpression
            |      LiteralPrimary
            |       RegularExpressionLiteral
            |        /
            |        (eu|us)
            |        /""".stripMargin
      }
    }

    "as the argument to a `puts` command" should {
      val code = "puts /(eu|us)/"

      "be parsed as such" in {
        printAst(_.expressionOrCommand(), code) shouldEqual
          """InvocationExpressionOrCommand
            | SingleCommandOnlyInvocationWithoutParentheses
            |  SimpleMethodCommand
            |   MethodIdentifier
            |    puts
            |   ArgumentsWithoutParentheses
            |    Arguments
            |     ExpressionArgument
            |      PrimaryExpression
            |       LiteralPrimary
            |        RegularExpressionLiteral
            |         /
            |         (eu|us)
            |         /""".stripMargin
      }
    }

    "as the argument to an parenthesized invocation" should {
      val code = "puts(/(eu|us)/)"

      "be parsed as such" in {
        printAst(_.expressionOrCommand(), code) shouldEqual
          """ExpressionExpressionOrCommand
            | PrimaryExpression
            |  InvocationWithParenthesesPrimary
            |   MethodIdentifier
            |    puts
            |   ArgsOnlyArgumentsWithParentheses
            |    (
            |    Arguments
            |     ExpressionArgument
            |      PrimaryExpression
            |       LiteralPrimary
            |        RegularExpressionLiteral
            |         /
            |         (eu|us)
            |         /
            |    )""".stripMargin
      }
    }
  }

  "A quoted non-interpolated (`%r`) regex literal" when {

    "by itself and using the `{`-`}` delimiters" should {

      "be parsed as a primary expression" in {
        val code = "%r{a-z}"
        printAst(_.primary(), code) shouldEqual
          """QuotedRegexInterpolationPrimary
            | QuotedRegexInterpolation
            |  %r{
            |  a-z
            |  }""".stripMargin
      }
    }

    "by itself and using the `<`-`>` delimiters" should {

      "be parsed as a primary expression" in {
        val code = "%r<eu|us>"
        printAst(_.primary(), code) shouldEqual
          """QuotedRegexInterpolationPrimary
            | QuotedRegexInterpolation
            |  %r<
            |  eu|us
            |  >""".stripMargin
      }
    }

    "by itself, empty and using the `[`-`]` delimiters" should {

      "be parsed as a primary expression" in {
        val code = "%r[]"
        printAst(_.primary(), code) shouldEqual
          """QuotedRegexInterpolationPrimary
            | QuotedRegexInterpolation
            |  %r[
            |  ]""".stripMargin
      }
    }

  }

  "A (numeric literal)-interpolated regex literal" when {

    "by itself" should {
      val code = "/x#{1}y/"

      "be parsed as a primary expression" in {
        printAst(_.primary(), code) shouldEqual
          """RegexInterpolationPrimary
              | RegexInterpolation
              |  /
              |  x
              |  InterpolatedRegexSequence
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
              |  y
              |  /""".stripMargin
      }
    }

    "on the RHS of an assignment" should {
      val code = "x = /x#{1}y/"

      "be parsed as a single assignment to a regex literal" in {
        printAst(_.expressionOrCommand(), code) shouldEqual
          """ExpressionExpressionOrCommand
            | SingleAssignmentExpression
            |  VariableIdentifierOnlySingleLeftHandSide
            |   VariableIdentifier
            |    x
            |  =
            |  MultipleRightHandSide
            |   ExpressionOrCommands
            |    ExpressionExpressionOrCommand
            |     PrimaryExpression
            |      RegexInterpolationPrimary
            |       RegexInterpolation
            |        /
            |        x
            |        InterpolatedRegexSequence
            |         #{
            |         CompoundStatement
            |          Statements
            |           ExpressionOrCommandStatement
            |            ExpressionExpressionOrCommand
            |             PrimaryExpression
            |              LiteralPrimary
            |               NumericLiteralLiteral
            |                NumericLiteral
            |                 UnsignedNumericLiteral
            |                  1
            |         }
            |        y
            |        /""".stripMargin
      }
    }

    "as the argument to a `puts` command" should {
      val code = "puts /x#{1}y/"

      "be parsed as such" in {
        printAst(_.expressionOrCommand(), code) shouldEqual
          """InvocationExpressionOrCommand
            | SingleCommandOnlyInvocationWithoutParentheses
            |  SimpleMethodCommand
            |   MethodIdentifier
            |    puts
            |   ArgumentsWithoutParentheses
            |    Arguments
            |     ExpressionArgument
            |      PrimaryExpression
            |       RegexInterpolationPrimary
            |        RegexInterpolation
            |         /
            |         x
            |         InterpolatedRegexSequence
            |          #{
            |          CompoundStatement
            |           Statements
            |            ExpressionOrCommandStatement
            |             ExpressionExpressionOrCommand
            |              PrimaryExpression
            |               LiteralPrimary
            |                NumericLiteralLiteral
            |                 NumericLiteral
            |                  UnsignedNumericLiteral
            |                   1
            |          }
            |         y
            |         /""".stripMargin
      }
    }

    "as the argument to an parenthesized invocation" should {
      val code = "puts(/x#{1}y/)"

      "be parsed as such" in {
        printAst(_.expressionOrCommand(), code) shouldEqual
          """ExpressionExpressionOrCommand
            | PrimaryExpression
            |  InvocationWithParenthesesPrimary
            |   MethodIdentifier
            |    puts
            |   ArgsOnlyArgumentsWithParentheses
            |    (
            |    Arguments
            |     ExpressionArgument
            |      PrimaryExpression
            |       RegexInterpolationPrimary
            |        RegexInterpolation
            |         /
            |         x
            |         InterpolatedRegexSequence
            |          #{
            |          CompoundStatement
            |           Statements
            |            ExpressionOrCommandStatement
            |             ExpressionExpressionOrCommand
            |              PrimaryExpression
            |               LiteralPrimary
            |                NumericLiteralLiteral
            |                 NumericLiteral
            |                  UnsignedNumericLiteral
            |                   1
            |          }
            |         y
            |         /
            |    )""".stripMargin
      }
    }
  }

  "An interpolated quoted (`%r`) regex" when {

    "by itself, containing a numeric literal interpolation and text" should {

      "be parsed as a primary expression" in {
        val code = """%r{x#{0}|y}"""
        printAst(_.primary(), code) shouldEqual
          """QuotedRegexInterpolationPrimary
            | QuotedRegexInterpolation
            |  %r{
            |  x
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
            |            0
            |   }
            |  |y
            |  }""".stripMargin

      }
    }
  }
}
