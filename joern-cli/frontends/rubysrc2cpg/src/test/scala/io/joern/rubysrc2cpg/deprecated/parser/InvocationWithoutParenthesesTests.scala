package io.joern.rubysrc2cpg.deprecated.parser

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
            |      BodyStatement
            |       CompoundStatement
            |        Statements
            |         ExpressionOrCommandStatement
            |          InvocationExpressionOrCommand
            |           SingleCommandOnlyInvocationWithoutParentheses
            |            SimpleMethodCommand
            |             MethodIdentifier
            |              puts
            |             ArgumentsWithoutParentheses
            |              Arguments
            |               ExpressionArgument
            |                PrimaryExpression
            |                 LiteralPrimary
            |                  NumericLiteralLiteral
            |                   NumericLiteral
            |                    UnsignedNumericLiteral
            |                     1
            |      end""".stripMargin

      }

      "it contains a safe navigation operator with no parameters" in {
        val code = "foo&.bar"
        printAst(_.primary(), code) shouldEqual
          """ChainedInvocationPrimary
            | VariableReferencePrimary
            |  VariableIdentifierVariableReference
            |   VariableIdentifier
            |    foo
            | &.
            | MethodName
            |  MethodIdentifier
            |   bar""".stripMargin
      }

      "it contains a safe navigation operator with non-zero parameters" in {
        val code = "foo&.bar 1,2"
        printAst(_.command(), code) shouldEqual
          """MemberAccessCommand
            | VariableReferencePrimary
            |  VariableIdentifierVariableReference
            |   VariableIdentifier
            |    foo
            | &.
            | MethodName
            |  MethodIdentifier
            |   bar
            | ArgumentsWithoutParentheses
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
            |         2""".stripMargin
      }

    }
  }

  "invocation with association arguments" should {
    "have correct structure for association arguments" in {
      val code = """foo bar:"""
      printAst(_.program(), code) shouldBe
        """Program
          | CompoundStatement
          |  Statements
          |   ExpressionOrCommandStatement
          |    InvocationExpressionOrCommand
          |     SingleCommandOnlyInvocationWithoutParentheses
          |      SimpleMethodCommand
          |       MethodIdentifier
          |        foo
          |       ArgumentsWithoutParentheses
          |        Arguments
          |         AssociationArgument
          |          Association
          |           PrimaryExpression
          |            VariableReferencePrimary
          |             VariableIdentifierVariableReference
          |              VariableIdentifier
          |               bar
          |           :
          | EOF""".stripMargin
    }
  }

}
