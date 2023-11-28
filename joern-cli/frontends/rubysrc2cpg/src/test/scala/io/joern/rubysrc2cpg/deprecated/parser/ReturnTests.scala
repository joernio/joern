package io.joern.rubysrc2cpg.deprecated.parser

class ReturnTests extends RubyParserAbstractTest {

  "A standalone return statement" should {

    "be parsed as statement" when {

      "it contains no arguments" in {
        val code = "return"
        printAst(_.statement(), code) shouldEqual
          """ExpressionOrCommandStatement
            | InvocationExpressionOrCommand
            |  ReturnArgsInvocationWithoutParentheses
            |   return""".stripMargin

      }

      "it contains a scoped chain invocation" in {
        val code = "return ::X.y()"
        printAst(_.statement(), code) shouldEqual
          """ExpressionOrCommandStatement
            | InvocationExpressionOrCommand
            |  ReturnArgsInvocationWithoutParentheses
            |   return
            |   Arguments
            |    ExpressionArgument
            |     PrimaryExpression
            |      ChainedInvocationPrimary
            |       SimpleScopedConstantReferencePrimary
            |        ::
            |        X
            |       .
            |       MethodName
            |        MethodIdentifier
            |         y
            |       BlankArgsArgumentsWithParentheses
            |        (
            |        )""".stripMargin
      }

      "it contains arguments in parentheses" in {
        val code = "return(0)"
        printAst(_.statement(), code) shouldEqual
          """ExpressionOrCommandStatement
            | ExpressionExpressionOrCommand
            |  PrimaryExpression
            |   ReturnWithParenthesesPrimary
            |    return
            |    ArgsOnlyArgumentsWithParentheses
            |     (
            |     Arguments
            |      ExpressionArgument
            |       PrimaryExpression
            |        LiteralPrimary
            |         NumericLiteralLiteral
            |          NumericLiteral
            |           UnsignedNumericLiteral
            |            0
            |     )""".stripMargin
      }
    }
  }

}
