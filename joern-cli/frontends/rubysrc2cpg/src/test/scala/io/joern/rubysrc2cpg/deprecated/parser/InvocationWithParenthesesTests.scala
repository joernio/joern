package io.joern.rubysrc2cpg.deprecated.parser

class InvocationWithParenthesesTests extends RubyParserAbstractTest {

  "A method invocation with parentheses" should {

    "be parsed as a primary expression" when {

      "it contains no arguments" in {
        val code = "foo()"

        printAst(_.primary(), code) shouldEqual
          """InvocationWithParenthesesPrimary
            | MethodIdentifier
            |  foo
            | BlankArgsArgumentsWithParentheses
            |  (
            |  )""".stripMargin
      }

      "it contains no arguments but has newline in between" in {
        val code =
          """foo(
            |)
            |""".stripMargin

        printAst(_.primary(), code) shouldEqual
          s"""InvocationWithParenthesesPrimary
            | MethodIdentifier
            |  foo
            | BlankArgsArgumentsWithParentheses
            |  (
            |  )""".stripMargin
      }

      "it contains a single numeric literal positional argument" in {
        val code = "foo(1)"

        printAst(_.primary(), code) shouldEqual
          """InvocationWithParenthesesPrimary
            | MethodIdentifier
            |  foo
            | ArgsOnlyArgumentsWithParentheses
            |  (
            |  Arguments
            |   ExpressionArgument
            |    PrimaryExpression
            |     LiteralPrimary
            |      NumericLiteralLiteral
            |       NumericLiteral
            |        UnsignedNumericLiteral
            |         1
            |  )""".stripMargin
      }

      "it contains a single numeric literal keyword argument" in {
        val code = "foo(region: 1)"

        printAst(_.primary(), code) shouldEqual
          s"""InvocationWithParenthesesPrimary
             | MethodIdentifier
             |  foo
             | ArgsOnlyArgumentsWithParentheses
             |  (
             |  Arguments
             |   AssociationArgument
             |    Association
             |     PrimaryExpression
             |      VariableReferencePrimary
             |       VariableIdentifierVariableReference
             |        VariableIdentifier
             |         region
             |     :
             |     PrimaryExpression
             |      LiteralPrimary
             |       NumericLiteralLiteral
             |        NumericLiteral
             |         UnsignedNumericLiteral
             |          1
             |  )""".stripMargin
      }

      "it contains an identifier keyword argument" in {
        val code = "foo(region:region)"

        printAst(_.primary(), code) shouldEqual
          s"""InvocationWithParenthesesPrimary
             | MethodIdentifier
             |  foo
             | ArgsOnlyArgumentsWithParentheses
             |  (
             |  Arguments
             |   AssociationArgument
             |    Association
             |     PrimaryExpression
             |      VariableReferencePrimary
             |       VariableIdentifierVariableReference
             |        VariableIdentifier
             |         region
             |     :
             |     PrimaryExpression
             |      VariableReferencePrimary
             |       VariableIdentifierVariableReference
             |        VariableIdentifier
             |         region
             |  )""".stripMargin
      }

      "it contains a non-empty regex literal keyword argument" in {
        val code = "foo(id: /.*/)"
        printAst(_.primary(), code) shouldEqual
          """InvocationWithParenthesesPrimary
            | MethodIdentifier
            |  foo
            | ArgsOnlyArgumentsWithParentheses
            |  (
            |  Arguments
            |   AssociationArgument
            |    Association
            |     PrimaryExpression
            |      VariableReferencePrimary
            |       VariableIdentifierVariableReference
            |        VariableIdentifier
            |         id
            |     :
            |     PrimaryExpression
            |      LiteralPrimary
            |       RegularExpressionLiteral
            |        /
            |        .*
            |        /
            |  )""".stripMargin
      }

      "it contains a single symbol literal positional argument" in {
        val code = "foo(:region)"

        printAst(_.primary(), code) shouldEqual
          """InvocationWithParenthesesPrimary
            | MethodIdentifier
            |  foo
            | ArgsOnlyArgumentsWithParentheses
            |  (
            |  Arguments
            |   ExpressionArgument
            |    PrimaryExpression
            |     LiteralPrimary
            |      SymbolLiteral
            |       Symbol
            |        :region
            |  )""".stripMargin
      }

      "it contains a single symbol literal positional argument and trailing comma" in {
        val code = "foo(:region,)"

        printAst(_.primary(), code) shouldEqual
          """InvocationWithParenthesesPrimary
            | MethodIdentifier
            |  foo
            | ArgsOnlyArgumentsWithParentheses
            |  (
            |  Arguments
            |   ExpressionArgument
            |    PrimaryExpression
            |     LiteralPrimary
            |      SymbolLiteral
            |       Symbol
            |        :region
            |  ,
            |  )""".stripMargin
      }

      "it contains a splatting expression before a keyword argument" in {
        val code = "foo(*x, y: 1)"
        printAst(_.primary(), code) shouldEqual
          """InvocationWithParenthesesPrimary
            | MethodIdentifier
            |  foo
            | ArgsOnlyArgumentsWithParentheses
            |  (
            |  Arguments
            |   SplattingArgumentArgument
            |    SplattingArgument
            |     *
            |     ExpressionExpressionOrCommand
            |      PrimaryExpression
            |       VariableReferencePrimary
            |        VariableIdentifierVariableReference
            |         VariableIdentifier
            |          x
            |   ,
            |   AssociationArgument
            |    Association
            |     PrimaryExpression
            |      VariableReferencePrimary
            |       VariableIdentifierVariableReference
            |        VariableIdentifier
            |         y
            |     :
            |     PrimaryExpression
            |      LiteralPrimary
            |       NumericLiteralLiteral
            |        NumericLiteral
            |         UnsignedNumericLiteral
            |          1
            |  )""".stripMargin
      }

      "it contains a keyword-named keyword argument" in {
        val code = "foo(if: true)"
        printAst(_.primary(), code) shouldEqual
          """InvocationWithParenthesesPrimary
            | MethodIdentifier
            |  foo
            | ArgsOnlyArgumentsWithParentheses
            |  (
            |  Arguments
            |   AssociationArgument
            |    Association
            |     Keyword
            |      if
            |     :
            |     PrimaryExpression
            |      VariableReferencePrimary
            |       PseudoVariableIdentifierVariableReference
            |        TruePseudoVariableIdentifier
            |         true
            |  )""".stripMargin
      }

      "it contains a safe navigation operator with no parameters" in {
        val code = "foo&.bar()"
        printAst(_.primary(), code) shouldEqual
          """ChainedInvocationPrimary
            | VariableReferencePrimary
            |  VariableIdentifierVariableReference
            |   VariableIdentifier
            |    foo
            | &.
            | MethodName
            |  MethodIdentifier
            |   bar
            | BlankArgsArgumentsWithParentheses
            |  (
            |  )""".stripMargin
      }

      "it contains a safe navigation operator with non-zero parameters" in {
        val code = "foo&.bar(1, 2)"
        printAst(_.primary(), code) shouldEqual
          """ChainedInvocationPrimary
            | VariableReferencePrimary
            |  VariableIdentifierVariableReference
            |   VariableIdentifier
            |    foo
            | &.
            | MethodName
            |  MethodIdentifier
            |   bar
            | ArgsOnlyArgumentsWithParentheses
            |  (
            |  Arguments
            |   ExpressionArgument
            |    PrimaryExpression
            |     LiteralPrimary
            |      NumericLiteralLiteral
            |       NumericLiteral
            |        UnsignedNumericLiteral
            |         1
            |   ,
            |   ExpressionArgument
            |    PrimaryExpression
            |     LiteralPrimary
            |      NumericLiteralLiteral
            |       NumericLiteral
            |        UnsignedNumericLiteral
            |         2
            |  )""".stripMargin
      }

      "it spans two lines, with the second line starting with `.`" in {
        val code = "foo\n   .bar"
        printAst(_.primary(), code) shouldEqual
          """ChainedInvocationPrimary
            | VariableReferencePrimary
            |  VariableIdentifierVariableReference
            |   VariableIdentifier
            |    foo
            | .
            | MethodName
            |  MethodIdentifier
            |   bar""".stripMargin
      }

      "it spans two lines, with the first line ending with `.`" in {
        val code = "foo.\n   bar"
        printAst(_.primary(), code) shouldEqual
          """ChainedInvocationPrimary
            | VariableReferencePrimary
            |  VariableIdentifierVariableReference
            |   VariableIdentifier
            |    foo
            | .
            | MethodName
            |  MethodIdentifier
            |   bar""".stripMargin
      }
    }
  }
}
