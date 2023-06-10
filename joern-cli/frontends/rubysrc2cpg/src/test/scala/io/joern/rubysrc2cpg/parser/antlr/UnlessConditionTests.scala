package io.joern.rubysrc2cpg.parser.antlr

class UnlessConditionTests extends RubyParserAbstractTest {

  "An unless statement" should {
    "be parsed as a primary expression" when {

      "it uses a newline instead of the keyword then" in {
        val code =
          """unless foo
            | bar
            |end
            |""".stripMargin

        printAst(_.primary(), code) shouldEqual
          """UnlessExpressionPrimary
            | UnlessExpression
            |  unless
            |  WsOrNl
            |  ExpressionExpressionOrCommand
            |   PrimaryExpression
            |    VariableReferencePrimary
            |     VariableReference
            |      VariableIdentifier
            |       foo
            |  ThenClause
            |   Separator
            |   WsOrNl
            |   CompoundStatement
            |    Statements
            |     ExpressionOrCommandStatement
            |      ExpressionExpressionOrCommand
            |       PrimaryExpression
            |        VariableReferencePrimary
            |         VariableReference
            |          VariableIdentifier
            |           bar
            |    Separators
            |     Separator
            |  end""".stripMargin
      }

      "it uses a semicolon instead of the keyword then" in {
        val code =
          """unless foo; bar
            |end
            |""".stripMargin

        printAst(_.primary(), code) shouldEqual
          """UnlessExpressionPrimary
            | UnlessExpression
            |  unless
            |  WsOrNl
            |  ExpressionExpressionOrCommand
            |   PrimaryExpression
            |    VariableReferencePrimary
            |     VariableReference
            |      VariableIdentifier
            |       foo
            |  ThenClause
            |   Separator
            |    ;
            |   WsOrNl
            |   CompoundStatement
            |    Statements
            |     ExpressionOrCommandStatement
            |      ExpressionExpressionOrCommand
            |       PrimaryExpression
            |        VariableReferencePrimary
            |         VariableReference
            |          VariableIdentifier
            |           bar
            |    Separators
            |     Separator
            |  end""".stripMargin
      }

      "it uses the keyword then" in {
        val code =
          """unless foo then
            | bar
            |end
            |""".stripMargin

        printAst(_.primary(), code) shouldEqual
          """UnlessExpressionPrimary
            | UnlessExpression
            |  unless
            |  WsOrNl
            |  ExpressionExpressionOrCommand
            |   PrimaryExpression
            |    VariableReferencePrimary
            |     VariableReference
            |      VariableIdentifier
            |       foo
            |  ThenClause
            |   then
            |   WsOrNl
            |   WsOrNl
            |   CompoundStatement
            |    Statements
            |     ExpressionOrCommandStatement
            |      ExpressionExpressionOrCommand
            |       PrimaryExpression
            |        VariableReferencePrimary
            |         VariableReference
            |          VariableIdentifier
            |           bar
            |    Separators
            |     Separator
            |  end""".stripMargin
      }
    }
  }
}
