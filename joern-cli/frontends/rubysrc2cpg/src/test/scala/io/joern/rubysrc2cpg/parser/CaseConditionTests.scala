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
            |  WsOrNl
            |  ExpressionExpressionOrCommand
            |   PrimaryExpression
            |    VariableReferencePrimary
            |     VariableReference
            |      VariableIdentifier
            |       something
            |  Separators
            |   Separator
            |  WhenClause
            |   when
            |   WsOrNl
            |   WhenArgument
            |    Expressions
            |     PrimaryExpression
            |      LiteralPrimary
            |       NumericLiteralLiteral
            |        NumericLiteral
            |         UnsignedNumericLiteral
            |          1
            |   ThenClause
            |    Separator
            |    WsOrNl
            |    CompoundStatement
            |     Statements
            |      ExpressionOrCommandStatement
            |       InvocationExpressionOrCommand
            |        SingleCommandOnlyInvocationWithoutParentheses
            |         SimpleMethodCommand
            |          MethodIdentifier
            |           puts
            |          ArgumentsWithoutParentheses
            |           BlockExprAssocTypeArguments
            |            Expressions
            |             PrimaryExpression
            |              LiteralPrimary
            |               NumericLiteralLiteral
            |                NumericLiteral
            |                 UnsignedNumericLiteral
            |                  2
            |     Separators
            |      Separator
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
            |  WsOrNl
            |  ExpressionExpressionOrCommand
            |   PrimaryExpression
            |    VariableReferencePrimary
            |     VariableReference
            |      VariableIdentifier
            |       something
            |  Separators
            |   Separator
            |  WhenClause
            |   when
            |   WsOrNl
            |   WhenArgument
            |    Expressions
            |     PrimaryExpression
            |      LiteralPrimary
            |       NumericLiteralLiteral
            |        NumericLiteral
            |         UnsignedNumericLiteral
            |          1
            |   ThenClause
            |    Separator
            |    WsOrNl
            |    CompoundStatement
            |  ElseClause
            |   else
            |   WsOrNl
            |   WsOrNl
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
            |  WsOrNl
            |  ExpressionExpressionOrCommand
            |   PrimaryExpression
            |    VariableReferencePrimary
            |     VariableReference
            |      VariableIdentifier
            |       something
            |  Separators
            |   Separator
            |  WhenClause
            |   when
            |   WsOrNl
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
            |    WsOrNl
            |    WsOrNl
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
            |  WsOrNl
            |  ExpressionExpressionOrCommand
            |   PrimaryExpression
            |    VariableReferencePrimary
            |     VariableReference
            |      VariableIdentifier
            |       x
            |  Separators
            |   Separator
            |  WhenClause
            |   when
            |   WsOrNl
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
            |    WsOrNl
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
            |      Separator
            |  WhenClause
            |   when
            |   WsOrNl
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
            |    WsOrNl
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
            |      Separator
            |  end""".stripMargin
      }
    }
  }

}
