package io.joern.rubysrc2cpg.parser

class RescueClauseTests extends RubyParserAbstractTest {

  "A rescue statement" should {

    "be parsed as a standalone statement" when {

      "in the immediate scope of a `begin` block" in {
        val code =
          """begin
            |1/0
            |rescue ZeroDivisionError => e
            |end""".stripMargin

        printAst(_.beginExpression(), code) shouldEqual
          """BeginExpression
            | begin
            | WsOrNl
            | BodyStatement
            |  CompoundStatement
            |   Statements
            |    ExpressionOrCommandStatement
            |     ExpressionExpressionOrCommand
            |      MultiplicativeExpression
            |       PrimaryExpression
            |        LiteralPrimary
            |         NumericLiteralLiteral
            |          NumericLiteral
            |           UnsignedNumericLiteral
            |            1
            |       /
            |       PrimaryExpression
            |        LiteralPrimary
            |         NumericLiteralLiteral
            |          NumericLiteral
            |           UnsignedNumericLiteral
            |            0
            |   Separators
            |    Separator
            |  RescueClause
            |   rescue
            |   ExceptionClass
            |    PrimaryExpression
            |     VariableReferencePrimary
            |      VariableIdentifierVariableReference
            |       VariableIdentifier
            |        ZeroDivisionError
            |   WsOrNl
            |   ExceptionVariableAssignment
            |    =>
            |    VariableIdentifierOnlySingleLeftHandSide
            |     VariableIdentifier
            |      e
            |   ThenClause
            |    Separator
            |    CompoundStatement
            | end""".stripMargin
      }

      "in the immediate scope of a `def` block" in {
        val code =
          """def foo;
            |1/0
            |rescue ZeroDivisionError => e
            |end""".stripMargin

        printAst(_.methodDefinition(), code) shouldEqual
          """MethodDefinition
            | def
            | WsOrNl
            | SimpleMethodNamePart
            |  DefinedMethodName
            |   MethodName
            |    MethodIdentifier
            |     foo
            | MethodParameterPart
            | Separator
            |  ;
            | WsOrNl
            | BodyStatement
            |  CompoundStatement
            |   Statements
            |    ExpressionOrCommandStatement
            |     ExpressionExpressionOrCommand
            |      MultiplicativeExpression
            |       PrimaryExpression
            |        LiteralPrimary
            |         NumericLiteralLiteral
            |          NumericLiteral
            |           UnsignedNumericLiteral
            |            1
            |       /
            |       PrimaryExpression
            |        LiteralPrimary
            |         NumericLiteralLiteral
            |          NumericLiteral
            |           UnsignedNumericLiteral
            |            0
            |   Separators
            |    Separator
            |  RescueClause
            |   rescue
            |   ExceptionClass
            |    PrimaryExpression
            |     VariableReferencePrimary
            |      VariableIdentifierVariableReference
            |       VariableIdentifier
            |        ZeroDivisionError
            |   WsOrNl
            |   ExceptionVariableAssignment
            |    =>
            |    VariableIdentifierOnlySingleLeftHandSide
            |     VariableIdentifier
            |      e
            |   ThenClause
            |    Separator
            |    CompoundStatement
            | end""".stripMargin
      }

      "in the immediate scope of a `do` block" in {
        val code =
          """foo x do |y|
            |y/0
            |rescue ZeroDivisionError => e
            |end""".stripMargin

        printAst(_.statement(), code) shouldEqual
          """ExpressionOrCommandStatement
            | InvocationExpressionOrCommand
            |  ChainedCommandDoBlockInvocationWithoutParentheses
            |   ChainedCommandWithDoBlock
            |    ArgsAndDoBlockAndMethodIdCommandWithDoBlock
            |     MethodIdentifier
            |      foo
            |     ArgumentsWithoutParentheses
            |      Arguments
            |       ExpressionArgument
            |        PrimaryExpression
            |         VariableReferencePrimary
            |          VariableIdentifierVariableReference
            |           VariableIdentifier
            |            x
            |     DoBlock
            |      do
            |      WsOrNl
            |      BlockParameter
            |       |
            |       BlockParameters
            |        VariableIdentifierOnlySingleLeftHandSide
            |         VariableIdentifier
            |          y
            |       |
            |      Separators
            |       Separator
            |      BodyStatement
            |       CompoundStatement
            |        Statements
            |         ExpressionOrCommandStatement
            |          ExpressionExpressionOrCommand
            |           MultiplicativeExpression
            |            PrimaryExpression
            |             VariableReferencePrimary
            |              VariableIdentifierVariableReference
            |               VariableIdentifier
            |                y
            |            /
            |            PrimaryExpression
            |             LiteralPrimary
            |              NumericLiteralLiteral
            |               NumericLiteral
            |                UnsignedNumericLiteral
            |                 0
            |        Separators
            |         Separator
            |       RescueClause
            |        rescue
            |        ExceptionClass
            |         PrimaryExpression
            |          VariableReferencePrimary
            |           VariableIdentifierVariableReference
            |            VariableIdentifier
            |             ZeroDivisionError
            |        WsOrNl
            |        ExceptionVariableAssignment
            |         =>
            |         VariableIdentifierOnlySingleLeftHandSide
            |          VariableIdentifier
            |           e
            |        ThenClause
            |         Separator
            |         CompoundStatement
            |      end""".stripMargin
      }
    }

  }

}
