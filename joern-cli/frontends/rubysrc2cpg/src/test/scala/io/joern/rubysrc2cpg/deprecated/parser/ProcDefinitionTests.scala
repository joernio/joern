package io.joern.rubysrc2cpg.deprecated.parser

class ProcDefinitionTests extends RubyParserAbstractTest {

  "A one-line proc definition" should {

    "be parsed as a primary expression" when {

      "it contains no parameters and no statements in a brace block" in {
        val code = "-> {}"
        printAst(_.primary(), code) shouldBe
          """ProcDefinitionPrimary
            | ProcDefinition
            |  ->
            |  BraceBlockBlock
            |   BraceBlock
            |    {
            |    BodyStatement
            |     CompoundStatement
            |    }""".stripMargin
      }

      "it contains no parameters and no statements in a do block" in {
        val code = "-> do ; end"
        printAst(_.primary(), code) shouldBe
          """ProcDefinitionPrimary
            | ProcDefinition
            |  ->
            |  DoBlockBlock
            |   DoBlock
            |    do
            |    BodyStatement
            |     CompoundStatement
            |      ;
            |    end""".stripMargin
      }

      "it contains no parameters and returns a literal in a do block" in {
        val code = "-> do 1 end"
        printAst(_.primary(), code) shouldBe
          """ProcDefinitionPrimary
            | ProcDefinition
            |  ->
            |  DoBlockBlock
            |   DoBlock
            |    do
            |    BodyStatement
            |     CompoundStatement
            |      Statements
            |       ExpressionOrCommandStatement
            |        ExpressionExpressionOrCommand
            |         PrimaryExpression
            |          LiteralPrimary
            |           NumericLiteralLiteral
            |            NumericLiteral
            |             UnsignedNumericLiteral
            |              1
            |    end""".stripMargin
      }

      "it contains a mandatory parameter and no statements in a brace block" in {
        val code = "-> (x) {}"
        printAst(_.primary(), code) shouldBe
          """ProcDefinitionPrimary
            | ProcDefinition
            |  ->
            |  (
            |  Parameters
            |   Parameter
            |    MandatoryParameter
            |     x
            |  )
            |  BraceBlockBlock
            |   BraceBlock
            |    {
            |    BodyStatement
            |     CompoundStatement
            |    }""".stripMargin
      }

      "it contains a mandatory parameter and no statements in a do block" in {
        val code = "-> (x) do ; end"
        printAst(_.primary(), code) shouldBe
          """ProcDefinitionPrimary
            | ProcDefinition
            |  ->
            |  (
            |  Parameters
            |   Parameter
            |    MandatoryParameter
            |     x
            |  )
            |  DoBlockBlock
            |   DoBlock
            |    do
            |    BodyStatement
            |     CompoundStatement
            |      ;
            |    end""".stripMargin
      }

      "it contains an optional numeric parameter and no statements in a brace block" in {
        val code = "->(x = 1) {}"
        printAst(_.primary(), code) shouldBe
          """ProcDefinitionPrimary
            | ProcDefinition
            |  ->
            |  (
            |  Parameters
            |   Parameter
            |    OptionalParameter
            |     x
            |     =
            |     PrimaryExpression
            |      LiteralPrimary
            |       NumericLiteralLiteral
            |        NumericLiteral
            |         UnsignedNumericLiteral
            |          1
            |  )
            |  BraceBlockBlock
            |   BraceBlock
            |    {
            |    BodyStatement
            |     CompoundStatement
            |    }""".stripMargin
      }

      "it contains a keyword parameter and no statements in a do block" in {
        val code = "-> (foo: 1) do ; end"
        printAst(_.primary(), code) shouldBe
          """ProcDefinitionPrimary
            | ProcDefinition
            |  ->
            |  (
            |  Parameters
            |   Parameter
            |    KeywordParameter
            |     foo
            |     :
            |     PrimaryExpression
            |      LiteralPrimary
            |       NumericLiteralLiteral
            |        NumericLiteral
            |         UnsignedNumericLiteral
            |          1
            |  )
            |  DoBlockBlock
            |   DoBlock
            |    do
            |    BodyStatement
            |     CompoundStatement
            |      ;
            |    end""".stripMargin
      }

      "it contains two mandatory parameters and two puts statements in a brace block" in {
        val code = "->(x, y) {puts x; puts y}"
        printAst(_.primary(), code) shouldBe
          """ProcDefinitionPrimary
            | ProcDefinition
            |  ->
            |  (
            |  Parameters
            |   Parameter
            |    MandatoryParameter
            |     x
            |   ,
            |   Parameter
            |    MandatoryParameter
            |     y
            |  )
            |  BraceBlockBlock
            |   BraceBlock
            |    {
            |    BodyStatement
            |     CompoundStatement
            |      Statements
            |       ExpressionOrCommandStatement
            |        InvocationExpressionOrCommand
            |         SingleCommandOnlyInvocationWithoutParentheses
            |          SimpleMethodCommand
            |           MethodIdentifier
            |            puts
            |           ArgumentsWithoutParentheses
            |            Arguments
            |             ExpressionArgument
            |              PrimaryExpression
            |               VariableReferencePrimary
            |                VariableIdentifierVariableReference
            |                 VariableIdentifier
            |                  x
            |       ;
            |       ExpressionOrCommandStatement
            |        InvocationExpressionOrCommand
            |         SingleCommandOnlyInvocationWithoutParentheses
            |          SimpleMethodCommand
            |           MethodIdentifier
            |            puts
            |           ArgumentsWithoutParentheses
            |            Arguments
            |             ExpressionArgument
            |              PrimaryExpression
            |               VariableReferencePrimary
            |                VariableIdentifierVariableReference
            |                 VariableIdentifier
            |                  y
            |    }""".stripMargin
      }

    }
  }

}
