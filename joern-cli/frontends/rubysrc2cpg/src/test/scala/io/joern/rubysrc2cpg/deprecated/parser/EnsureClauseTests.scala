package io.joern.rubysrc2cpg.deprecated.parser

class EnsureClauseTests extends RubyParserAbstractTest {

  "An ensure statement" should {

    "be parsed as a standalone statement" when {

      "in the immediate scope of a `def` block" in {
        val code =
          """def refund
            |  ensure
            |    redirect_to paddle_charge_path(@charge)
            |end""".stripMargin
        printAst(_.methodDefinition(), code) shouldEqual
          """MethodDefinition
            | def
            | SimpleMethodNamePart
            |  DefinedMethodName
            |   MethodName
            |    MethodIdentifier
            |     refund
            | MethodParameterPart
            | BodyStatement
            |  CompoundStatement
            |  EnsureClause
            |   ensure
            |   CompoundStatement
            |    Statements
            |     ExpressionOrCommandStatement
            |      InvocationExpressionOrCommand
            |       SingleCommandOnlyInvocationWithoutParentheses
            |        SimpleMethodCommand
            |         MethodIdentifier
            |          redirect_to
            |         ArgumentsWithoutParentheses
            |          Arguments
            |           ExpressionArgument
            |            PrimaryExpression
            |             InvocationWithParenthesesPrimary
            |              MethodIdentifier
            |               paddle_charge_path
            |              ArgsOnlyArgumentsWithParentheses
            |               (
            |               Arguments
            |                ExpressionArgument
            |                 PrimaryExpression
            |                  VariableReferencePrimary
            |                   VariableIdentifierVariableReference
            |                    VariableIdentifier
            |                     @charge
            |               )
            | end""".stripMargin
      }
    }
  }

}
