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

      "it contains two parameters, the last of which a &-parameter" in {
        val code = "def foo(x, &y); end"
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
            |    ,
            |    ProcParameter
            |     &
            |     y
            |   )
            |  BodyStatement
            |   CompoundStatement
            |    Separators
            |     Separator
            |      ;
            |  WsOrNl
            |  end""".stripMargin
      }

      "it contains a named (array) splatting argument" in {
        val code = "def foo(*arr); end"
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
          |    ArrayParameter
          |     *
          |     arr
          |   )
          |  BodyStatement
          |   CompoundStatement
          |    Separators
          |     Separator
          |      ;
          |  WsOrNl
          |  end""".stripMargin
      }

      "it contains a named (hash) splatting argument" in {
        val code = "def foo(**hash); end"
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
          |    HashParameter
          |     **
          |     hash
          |   )
          |  BodyStatement
          |   CompoundStatement
          |    Separators
          |     Separator
          |      ;
          |  WsOrNl
          |  end""".stripMargin
      }

      "it contains both a named array and hash splatting argument" in {
        val code = "def foo(*arr, **hash); end"
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
          |    ArrayParameter
          |     *
          |     arr
          |    ,
          |    HashParameter
          |     **
          |     hash
          |   )
          |  BodyStatement
          |   CompoundStatement
          |    Separators
          |     Separator
          |      ;
          |  WsOrNl
          |  end""".stripMargin
      }
    }
  }
}
