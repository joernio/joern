package io.joern.rubysrc2cpg.parser

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
            |    CompoundStatement
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
            |    WsOrNl
            |    Separators
            |     Separator
            |      ;
            |    WsOrNl
            |    CompoundStatement
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
            |    CompoundStatement
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
            |    WsOrNl
            |    Separators
            |     Separator
            |      ;
            |    WsOrNl
            |    CompoundStatement
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
            |     WsOrNl
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
            |    CompoundStatement
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
            |     WsOrNl
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
            |    WsOrNl
            |    Separators
            |     Separator
            |      ;
            |    WsOrNl
            |    CompoundStatement
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
            |   WsOrNl
            |   Parameter
            |    MandatoryParameter
            |     y
            |  )
            |  BraceBlockBlock
            |   BraceBlock
            |    {
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
            |              VariableReferencePrimary
            |               VariableIdentifierVariableReference
            |                VariableIdentifier
            |                 x
            |      Separators
            |       Separator
            |        ;
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
            |              VariableReferencePrimary
            |               VariableIdentifierVariableReference
            |                VariableIdentifier
            |                 y
            |    }""".stripMargin
      }

    }
  }

}
