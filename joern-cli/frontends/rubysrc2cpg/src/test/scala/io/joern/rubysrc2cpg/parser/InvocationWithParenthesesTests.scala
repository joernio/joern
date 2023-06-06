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
            |  BlockExprAssocTypeArguments
            |   Expressions
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
            |  BlockExprAssocTypeArguments
            |   Associations
            |    Association
            |     PrimaryExpression
            |      VariableReferencePrimary
            |       VariableReference
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

      "it contains a single symbol literal positional argument" in {
        val code = "foo(:region)"

        printAst(_.primary(), code) shouldEqual
          """InvocationWithParenthesesPrimary
            | MethodIdentifier
            |  foo
            | ArgsOnlyArgumentsWithParentheses
            |  (
            |  BlockExprAssocTypeArguments
            |   Expressions
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
            |  BlockExprAssocTypeArguments
            |   Expressions
            |    PrimaryExpression
            |     LiteralPrimary
            |      SymbolLiteral
            |       Symbol
            |        :region
            |  ,
            |  )""".stripMargin
      }
    }
  }
}
