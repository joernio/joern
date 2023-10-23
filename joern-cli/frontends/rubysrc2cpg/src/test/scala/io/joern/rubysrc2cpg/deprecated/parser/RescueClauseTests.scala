package io.joern.rubysrc2cpg.deprecated.parser

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
            |  RescueClause
            |   rescue
            |   ExceptionClass
            |    PrimaryExpression
            |     VariableReferencePrimary
            |      VariableIdentifierVariableReference
            |       VariableIdentifier
            |        ZeroDivisionError
            |   ExceptionVariableAssignment
            |    =>
            |    VariableIdentifierOnlySingleLeftHandSide
            |     VariableIdentifier
            |      e
            |   ThenClause
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
            | SimpleMethodNamePart
            |  DefinedMethodName
            |   MethodName
            |    MethodIdentifier
            |     foo
            | MethodParameterPart
            | BodyStatement
            |  CompoundStatement
            |   ;
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
            |  RescueClause
            |   rescue
            |   ExceptionClass
            |    PrimaryExpression
            |     VariableReferencePrimary
            |      VariableIdentifierVariableReference
            |       VariableIdentifier
            |        ZeroDivisionError
            |   ExceptionVariableAssignment
            |    =>
            |    VariableIdentifierOnlySingleLeftHandSide
            |     VariableIdentifier
            |      e
            |   ThenClause
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
            |      BlockParameter
            |       |
            |       BlockParameters
            |        VariableIdentifierOnlySingleLeftHandSide
            |         VariableIdentifier
            |          y
            |       |
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
            |       RescueClause
            |        rescue
            |        ExceptionClass
            |         PrimaryExpression
            |          VariableReferencePrimary
            |           VariableIdentifierVariableReference
            |            VariableIdentifier
            |             ZeroDivisionError
            |        ExceptionVariableAssignment
            |         =>
            |         VariableIdentifierOnlySingleLeftHandSide
            |          VariableIdentifier
            |           e
            |        ThenClause
            |         CompoundStatement
            |      end""".stripMargin
      }
    }

  }

}
