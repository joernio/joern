package io.joern.rubysrc2cpg.parser

class MethodDefinitionTests extends RubyParserAbstractTest {

  "A one-line empty method definition" should {

    "be parsed as a primary expression" when {

      "it contains no parameters" in {
        val code = "def foo; end"
        printAst(_.primary(), code) shouldEqual
          """MethodDefinitionPrimary
            | MethodDefinition
            |  def
            |  WsOrNl
            |  SimpleMethodNamePart
            |   DefinedMethodName
            |    MethodName
            |     MethodIdentifier
            |      foo
            |  MethodParameterPart
            |   Separator
            |    ;
            |  WsOrNl
            |  BodyStatement
            |   CompoundStatement
            |  end""".stripMargin
      }

      "it contains a mandatory parameter" in {
        val code = "def foo(x);end"
        printAst(_.primary(), code) shouldEqual
          """MethodDefinitionPrimary
            | MethodDefinition
            |  def
            |  WsOrNl
            |  SimpleMethodNamePart
            |   DefinedMethodName
            |    MethodName
            |     MethodIdentifier
            |      foo
            |  MethodParameterPart
            |   (
            |   Parameters
            |    MandatoryParameters
            |     x
            |   )
            |  BodyStatement
            |   CompoundStatement
            |    Separators
            |     Separator
            |      ;
            |  end""".stripMargin
      }

      "it contains an optional numeric parameter" in {
        val code = "def foo(x=1);end"
        printAst(_.primary(), code) shouldEqual
          """MethodDefinitionPrimary
             | MethodDefinition
             |  def
             |  WsOrNl
             |  SimpleMethodNamePart
             |   DefinedMethodName
             |    MethodName
             |     MethodIdentifier
             |      foo
             |  MethodParameterPart
             |   (
             |   Parameters
             |    OptionalParameters
             |     OptionalParameter
             |      x
             |      =
             |      PrimaryExpression
             |       LiteralPrimary
             |        NumericLiteralLiteral
             |         NumericLiteral
             |          UnsignedNumericLiteral
             |           1
             |   )
             |  BodyStatement
             |   CompoundStatement
             |    Separators
             |     Separator
             |      ;
             |  end""".stripMargin
      }
    }
  }
}
