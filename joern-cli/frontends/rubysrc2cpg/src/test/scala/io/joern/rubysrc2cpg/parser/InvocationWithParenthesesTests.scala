package io.joern.rubysrc2cpg.parser

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
            |  WsOrNl
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
             |     WsOrNl
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
            |     WsOrNl
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
            |   WsOrNl
            |   AssociationArgument
            |    Association
            |     PrimaryExpression
            |      VariableReferencePrimary
            |       VariableIdentifierVariableReference
            |        VariableIdentifier
            |         y
            |     :
            |     WsOrNl
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
            |     WsOrNl
            |     PrimaryExpression
            |      VariableReferencePrimary
            |       PseudoVariableIdentifierVariableReference
            |        TruePseudoVariableIdentifier
            |         true
            |  )""".stripMargin
      }

    }
  }
}
