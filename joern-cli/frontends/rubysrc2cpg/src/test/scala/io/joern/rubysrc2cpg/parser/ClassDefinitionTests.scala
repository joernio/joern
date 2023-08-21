package io.joern.rubysrc2cpg.parser

class ClassDefinitionTests extends RubyParserAbstractTest {

  "A one-line singleton class definition" should {

    "be parsed as a primary expression" when {

      "it contains no members" in {
        val code = "class << self ; end"
        printAst(_.primary(), code) shouldBe
          """ClassDefinitionPrimary
            | ClassDefinition
            |  class
            |  <<
            |  ExpressionExpressionOrCommand
            |   PrimaryExpression
            |    VariableReferencePrimary
            |     PseudoVariableIdentifierVariableReference
            |      SelfPseudoVariableIdentifier
            |       self
            |  Separators
            |   ;
            |  BodyStatement
            |   CompoundStatement
            |  end""".stripMargin
      }
    }
  }

  "A multi-line singleton class definition" should {

    "be parsed as a primary expression" when {

      "it contains a single method definition" in {
        val code =
          """class << x
            | def show; puts self; end
            |end""".stripMargin
        printAst(_.primary(), code) shouldBe
          """ClassDefinitionPrimary
            | ClassDefinition
            |  class
            |  <<
            |  ExpressionExpressionOrCommand
            |   PrimaryExpression
            |    VariableReferencePrimary
            |     VariableIdentifierVariableReference
            |      VariableIdentifier
            |       x
            |  Separators
            |  BodyStatement
            |   CompoundStatement
            |    Statements
            |     ExpressionOrCommandStatement
            |      ExpressionExpressionOrCommand
            |       PrimaryExpression
            |        MethodDefinitionPrimary
            |         MethodDefinition
            |          def
            |          SimpleMethodNamePart
            |           DefinedMethodName
            |            MethodName
            |             MethodIdentifier
            |              show
            |          MethodParameterPart
            |          Separators
            |           ;
            |          BodyStatement
            |           CompoundStatement
            |            Statements
            |             ExpressionOrCommandStatement
            |              InvocationExpressionOrCommand
            |               SingleCommandOnlyInvocationWithoutParentheses
            |                SimpleMethodCommand
            |                 MethodIdentifier
            |                  puts
            |                 ArgumentsWithoutParentheses
            |                  Arguments
            |                   ExpressionArgument
            |                    PrimaryExpression
            |                     VariableReferencePrimary
            |                      PseudoVariableIdentifierVariableReference
            |                       SelfPseudoVariableIdentifier
            |                        self
            |            Separators
            |             ;
            |          end
            |    Separators
            |  end""".stripMargin
      }
    }
  }

}
