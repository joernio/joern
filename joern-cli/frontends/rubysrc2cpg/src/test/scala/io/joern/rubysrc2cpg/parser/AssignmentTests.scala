package io.joern.rubysrc2cpg.parser

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

}
