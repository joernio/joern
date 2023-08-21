package io.joern.rubysrc2cpg.parser

class CaseConditionTests extends RubyParserAbstractTest {

  "A case expression" should {

    "be parsed as a primary expression" when {

      "it contains just one `when` branch" in {
        val code =
          """case something
            | when 1
            |   puts 2
            |end
            |""".stripMargin

        printAst(_.primary(), code) shouldBe
          """CaseExpressionPrimary
            | CaseExpression
            |  case
            |  ExpressionExpressionOrCommand
            |   PrimaryExpression
            |    VariableReferencePrimary
            |     VariableIdentifierVariableReference
            |      VariableIdentifier
            |       something
            |  Separators
            |  WhenClause
            |   when
            |   WhenArgument
            |    Expressions
            |     PrimaryExpression
            |      LiteralPrimary
            |       NumericLiteralLiteral
            |        NumericLiteral
            |         UnsignedNumericLiteral
            |          1
            |   ThenClause
            |    Separators
            |    CompoundStatement
            |     Statements
            |      ExpressionOrCommandStatement
            |       InvocationExpressionOrCommand
            |        SingleCommandOnlyInvocationWithoutParentheses
            |         SimpleMethodCommand
            |          MethodIdentifier
            |           puts
            |          ArgumentsWithoutParentheses
            |           Arguments
            |            ExpressionArgument
            |             PrimaryExpression
            |              LiteralPrimary
            |               NumericLiteralLiteral
            |                NumericLiteral
            |                 UnsignedNumericLiteral
            |                  2
            |     Separators
            |  end""".stripMargin
      }

      "it contains both an empty `when` and `else` branch" in {
        val code =
          """case something
            | when 1
            | else
            | end
            |""".stripMargin

        printAst(_.primary(), code) shouldBe
          """CaseExpressionPrimary
            | CaseExpression
            |  case
            |  ExpressionExpressionOrCommand
            |   PrimaryExpression
            |    VariableReferencePrimary
            |     VariableIdentifierVariableReference
            |      VariableIdentifier
            |       something
            |  Separators
            |  WhenClause
            |   when
            |   WhenArgument
            |    Expressions
            |     PrimaryExpression
            |      LiteralPrimary
            |       NumericLiteralLiteral
            |        NumericLiteral
            |         UnsignedNumericLiteral
            |          1
            |   ThenClause
            |    Separators
            |    CompoundStatement
            |  ElseClause
            |   else
            |   CompoundStatement
            |  end""".stripMargin
      }

      "it uses `then` as separator for `when`" in {
        val code =
          """case something
            | when 1 then
            | end
            |""".stripMargin

        printAst(_.primary(), code) shouldBe
          """CaseExpressionPrimary
            | CaseExpression
            |  case
            |  ExpressionExpressionOrCommand
            |   PrimaryExpression
            |    VariableReferencePrimary
            |     VariableIdentifierVariableReference
            |      VariableIdentifier
            |       something
            |  Separators
            |  WhenClause
            |   when
            |   WhenArgument
            |    Expressions
            |     PrimaryExpression
            |      LiteralPrimary
            |       NumericLiteralLiteral
            |        NumericLiteral
            |         UnsignedNumericLiteral
            |          1
            |   ThenClause
            |    then
            |    CompoundStatement
            |  end""".stripMargin
      }

      "it contains two single-line `when-then` branches" in {
        val code =
          """case x
            | when 1 then 2
            | when 2 then 3
            | end
            |""".stripMargin

        printAst(_.primary(), code) shouldBe
          """CaseExpressionPrimary
            | CaseExpression
            |  case
            |  ExpressionExpressionOrCommand
            |   PrimaryExpression
            |    VariableReferencePrimary
            |     VariableIdentifierVariableReference
            |      VariableIdentifier
            |       x
            |  Separators
            |  WhenClause
            |   when
            |   WhenArgument
            |    Expressions
            |     PrimaryExpression
            |      LiteralPrimary
            |       NumericLiteralLiteral
            |        NumericLiteral
            |         UnsignedNumericLiteral
            |          1
            |   ThenClause
            |    then
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
            |     Separators
            |  WhenClause
            |   when
            |   WhenArgument
            |    Expressions
            |     PrimaryExpression
            |      LiteralPrimary
            |       NumericLiteralLiteral
            |        NumericLiteral
            |         UnsignedNumericLiteral
            |          2
            |   ThenClause
            |    then
            |    CompoundStatement
            |     Statements
            |      ExpressionOrCommandStatement
            |       ExpressionExpressionOrCommand
            |        PrimaryExpression
            |         LiteralPrimary
            |          NumericLiteralLiteral
            |           NumericLiteral
            |            UnsignedNumericLiteral
            |             3
            |     Separators
            |  end""".stripMargin
      }
    }
  }

}
