package io.joern.rubysrc2cpg.parser

class InvocationWithoutParenthesesTests extends RubyParserAbstractTest {

  "A method invocation without parentheses" should {

    "be parsed as a primary expression" when {

      "it contains a keyword?-named member" in {
        val code = "task.nil?"

        printAst(_.primary(), code) shouldBe
          """ChainedInvocationPrimary
            | VariableReferencePrimary
            |  VariableIdentifierVariableReference
            |   VariableIdentifier
            |    task
            | .
            | MethodName
            |  MethodIdentifier
            |   MethodOnlyIdentifier
            |    Keyword
            |     nil
            |    ?""".stripMargin
      }

      "it is keyword?-named" in {
        val code = "do?"

        printAst(_.primary(), code) shouldBe
          """MethodOnlyIdentifierPrimary
            | MethodOnlyIdentifier
            |  Keyword
            |   do
            |  ?""".stripMargin
      }

      "it is keyword!-named" in {
        val code = "return!"

        printAst(_.primary(), code) shouldBe
          """MethodOnlyIdentifierPrimary
            | MethodOnlyIdentifier
            |  Keyword
            |   return
            |  !""".stripMargin
      }
    }
  }

  "A command with do block" should {

    "be parsed as a statement" when {

      "it contains only one argument" in {
        val code = """it 'should print 1' do
                      |  puts 1
                      |end 
                      |""".stripMargin

        printAst(_.statement(), code) shouldBe
          """ExpressionOrCommandStatement
            | InvocationExpressionOrCommand
            |  ChainedCommandDoBlockInvocationWithoutParentheses
            |   ChainedCommandWithDoBlock
            |    ArgsAndDoBlockAndMethodIdCommandWithDoBlock
            |     MethodIdentifier
            |      it
            |     ArgumentsWithoutParentheses
            |      Arguments
            |       ExpressionArgument
            |        PrimaryExpression
            |         StringExpressionPrimary
            |          SimpleStringExpression
            |           SingleQuotedStringLiteral
            |            'should print 1'
            |     DoBlock
            |      do
            |      Separators
            |       Separator
            |      WsOrNl
            |      CompoundStatement
            |       Statements
            |        ExpressionOrCommandStatement
            |         InvocationExpressionOrCommand
            |          SingleCommandOnlyInvocationWithoutParentheses
            |           SimpleMethodCommand
            |            MethodIdentifier
            |             puts
            |            ArgumentsWithoutParentheses
            |             Arguments
            |              ExpressionArgument
            |               PrimaryExpression
            |                LiteralPrimary
            |                 NumericLiteralLiteral
            |                  NumericLiteral
            |                   UnsignedNumericLiteral
            |                    1
            |       Separators
            |        Separator
            |      end""".stripMargin

      }
    }
  }

}
