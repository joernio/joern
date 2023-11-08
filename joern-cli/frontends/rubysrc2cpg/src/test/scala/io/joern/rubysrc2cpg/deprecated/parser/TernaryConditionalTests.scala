package io.joern.rubysrc2cpg.deprecated.parser

class TernaryConditionalTests extends RubyParserAbstractTest {

  "Ternary conditional expressions" should {

    "be parsed as expressions" when {

      "they are a standalone one-line expression" in {
        val code = "x ? y : z"
        printAst(_.expression(), code) shouldEqual
          """ConditionalOperatorExpression
            | PrimaryExpression
            |  VariableReferencePrimary
            |   VariableIdentifierVariableReference
            |    VariableIdentifier
            |     x
            | ?
            | PrimaryExpression
            |  VariableReferencePrimary
            |   VariableIdentifierVariableReference
            |    VariableIdentifier
            |     y
            | :
            | PrimaryExpression
            |  VariableReferencePrimary
            |   VariableIdentifierVariableReference
            |    VariableIdentifier
            |     z""".stripMargin

      }

      "they are a standalone multi-line expression" in {
        val code =
          """x ?
            |  y
            |: z
            |""".stripMargin
        printAst(_.expression(), code) shouldEqual
          """ConditionalOperatorExpression
            | PrimaryExpression
            |  VariableReferencePrimary
            |   VariableIdentifierVariableReference
            |    VariableIdentifier
            |     x
            | ?
            | PrimaryExpression
            |  VariableReferencePrimary
            |   VariableIdentifierVariableReference
            |    VariableIdentifier
            |     y
            | :
            | PrimaryExpression
            |  VariableReferencePrimary
            |   VariableIdentifierVariableReference
            |    VariableIdentifier
            |     z""".stripMargin
      }
    }
  }

}
