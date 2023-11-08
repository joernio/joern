package io.joern.rubysrc2cpg.deprecated.parser

class AssignmentTests extends RubyParserAbstractTest {

  "single assignment" should {

    "be parsed as a statement" when {

      "it contains no whitespace before `=`" in {
        val code = "x=1"
        printAst(_.statement(), code) shouldEqual
          """ExpressionOrCommandStatement
            | ExpressionExpressionOrCommand
            |  SingleAssignmentExpression
            |   VariableIdentifierOnlySingleLeftHandSide
            |    VariableIdentifier
            |     x
            |   =
            |   MultipleRightHandSide
            |    ExpressionOrCommands
            |     ExpressionExpressionOrCommand
            |      PrimaryExpression
            |       LiteralPrimary
            |        NumericLiteralLiteral
            |         NumericLiteral
            |          UnsignedNumericLiteral
            |           1""".stripMargin
      }
    }
  }

  "multiple assignment" should {

    "be parsed as a statement" when {
      "two identifiers are assigned an array containing two calls" in {
        val code = "p, q = [foo(), bar()]"
        printAst(_.statement(), code) shouldEqual
          """ExpressionOrCommandStatement
            | ExpressionExpressionOrCommand
            |  MultipleAssignmentExpression
            |   MultipleLeftHandSideAndpackingLeftHandSideMultipleLeftHandSide
            |    MultipleLeftHandSideItem
            |     VariableIdentifierOnlySingleLeftHandSide
            |      VariableIdentifier
            |       p
            |    ,
            |    MultipleLeftHandSideItem
            |     VariableIdentifierOnlySingleLeftHandSide
            |      VariableIdentifier
            |       q
            |   =
            |   MultipleRightHandSide
            |    ExpressionOrCommands
            |     ExpressionExpressionOrCommand
            |      PrimaryExpression
            |       ArrayConstructorPrimary
            |        BracketedArrayConstructor
            |         [
            |         ExpressionsOnlyIndexingArguments
            |          Expressions
            |           PrimaryExpression
            |            InvocationWithParenthesesPrimary
            |             MethodIdentifier
            |              foo
            |             BlankArgsArgumentsWithParentheses
            |              (
            |              )
            |           ,
            |           PrimaryExpression
            |            InvocationWithParenthesesPrimary
            |             MethodIdentifier
            |              bar
            |             BlankArgsArgumentsWithParentheses
            |              (
            |              )
            |         ]""".stripMargin
      }
    }
  }

}
