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
            |  Separator
            |   ;
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
            |    Parameter
            |     MandatoryParameter
            |      x
            |   )
            |  Separator
            |   ;
            |  BodyStatement
            |   CompoundStatement
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
            |    Parameter
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
            |  Separator
            |   ;
            |  BodyStatement
            |   CompoundStatement
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
            |    Parameter
            |     MandatoryParameter
            |      x
            |    ,
            |    WsOrNl
            |    Parameter
            |     ProcParameter
            |      &
            |      y
            |   )
            |  Separator
            |   ;
            |  WsOrNl
            |  BodyStatement
            |   CompoundStatement
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
            |    Parameter
            |     ArrayParameter
            |      *
            |      arr
            |   )
            |  Separator
            |   ;
            |  WsOrNl
            |  BodyStatement
            |   CompoundStatement
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
            |    Parameter
            |     HashParameter
            |      **
            |      hash
            |   )
            |  Separator
            |   ;
            |  WsOrNl
            |  BodyStatement
            |   CompoundStatement
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
            |    Parameter
            |     ArrayParameter
            |      *
            |      arr
            |    ,
            |    WsOrNl
            |    Parameter
            |     HashParameter
            |      **
            |      hash
            |   )
            |  Separator
            |   ;
            |  WsOrNl
            |  BodyStatement
            |   CompoundStatement
            |  end""".stripMargin
      }

      "it contains an optional parameter before a mandatory one" in {
        val code = "def foo(x=1,y); end"
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
            |    Parameter
            |     OptionalParameter
            |      x
            |      =
            |      PrimaryExpression
            |       LiteralPrimary
            |        NumericLiteralLiteral
            |         NumericLiteral
            |          UnsignedNumericLiteral
            |           1
            |    ,
            |    Parameter
            |     MandatoryParameter
            |      y
            |   )
            |  Separator
            |   ;
            |  WsOrNl
            |  BodyStatement
            |   CompoundStatement
            |  end""".stripMargin
      }

      "it contains a keyword parameter" in {
        val code = "def foo(x: 1); end"
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
            |    Parameter
            |     KeywordParameter
            |      x
            |      :
            |      WsOrNl
            |      PrimaryExpression
            |       LiteralPrimary
            |        NumericLiteralLiteral
            |         NumericLiteral
            |          UnsignedNumericLiteral
            |           1
            |   )
            |  Separator
            |   ;
            |  WsOrNl
            |  BodyStatement
            |   CompoundStatement
            |  end""".stripMargin
      }

      "it contains a mandatory keyword parameter" in {
        val code = "def foo(x:) ; end"
        printAst(_.primary(), code) shouldBe
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
            |    Parameter
            |     KeywordParameter
            |      x
            |      :
            |   )
            |  WsOrNl
            |  BodyStatement
            |   CompoundStatement
            |    Separators
            |     Separator
            |      ;
            |  WsOrNl
            |  end""".stripMargin
      }

      "it contains two mandatory keyword parameters" in {
        val code = "def foo(name:, surname:) ; end"
        printAst(_.primary(), code) shouldBe
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
            |    Parameter
            |     KeywordParameter
            |      name
            |      :
            |    ,
            |    WsOrNl
            |    Parameter
            |     KeywordParameter
            |      surname
            |      :
            |   )
            |  WsOrNl
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

  "A multi-line method definition" should {

    "be parsed as a primary expression" when {

      "it contains a `rescue` clause" in {
        val code = """def foo
                      |  1/0
                      |  rescue ZeroDivisionError => e
                      |end""".stripMargin
        printAst(_.primary(), code) shouldBe
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
            |  Separator
            |  WsOrNl
            |  BodyStatement
            |   CompoundStatement
            |    Statements
            |     ExpressionOrCommandStatement
            |      ExpressionExpressionOrCommand
            |       MultiplicativeExpression
            |        PrimaryExpression
            |         LiteralPrimary
            |          NumericLiteralLiteral
            |           NumericLiteral
            |            UnsignedNumericLiteral
            |             1
            |        /
            |        PrimaryExpression
            |         LiteralPrimary
            |          NumericLiteralLiteral
            |           NumericLiteral
            |            UnsignedNumericLiteral
            |             0
            |    Separators
            |     Separator
            |   WsOrNl
            |   RescueClause
            |    rescue
            |    ExceptionClass
            |     PrimaryExpression
            |      VariableReferencePrimary
            |       VariableIdentifierVariableReference
            |        VariableIdentifier
            |         ZeroDivisionError
            |    WsOrNl
            |    ExceptionVariableAssignment
            |     =>
            |     VariableIdentifierOnlySingleLeftHandSide
            |      VariableIdentifier
            |       e
            |    ThenClause
            |     Separator
            |     CompoundStatement
            |  end""".stripMargin

      }
    }

  }

  "An endless method definition" should {

    "be parsed as a primary expression" when {

      "it contains no arguments" in {
        val code = "def foo = x"
        printAst(_.primary(), code) shouldEqual
          """MethodDefinitionPrimary
            | MethodDefinition
            |  def
            |  WsOrNl
            |  MethodIdentifier
            |   foo
            |  MethodParameterPart
            |  =
            |  WsOrNl
            |  PrimaryExpression
            |   VariableReferencePrimary
            |    VariableIdentifierVariableReference
            |     VariableIdentifier
            |      x""".stripMargin
      }

      "it contains a line break right after `=`" in {
        val code = "def foo =\n x"
        printAst(_.primary(), code) shouldEqual
          """MethodDefinitionPrimary
            | MethodDefinition
            |  def
            |  WsOrNl
            |  MethodIdentifier
            |   foo
            |  MethodParameterPart
            |  =
            |  WsOrNl
            |  WsOrNl
            |  PrimaryExpression
            |   VariableReferencePrimary
            |    VariableIdentifierVariableReference
            |     VariableIdentifier
            |      x""".stripMargin
      }

      "it contains no arguments and a string literal on the RHS" in {
        val code = """def foo = "something""""
        printAst(_.primary(), code) shouldEqual
          """MethodDefinitionPrimary
            | MethodDefinition
            |  def
            |  WsOrNl
            |  MethodIdentifier
            |   foo
            |  MethodParameterPart
            |  =
            |  WsOrNl
            |  PrimaryExpression
            |   StringExpressionPrimary
            |    SimpleStringExpression
            |     DoubleQuotedStringLiteral
            |      "
            |      something
            |      """".stripMargin
      }

      "it contains a single mandatory argument" in {
        val code = "def id(x) = x"
        printAst(_.primary(), code) shouldEqual
          """MethodDefinitionPrimary
            | MethodDefinition
            |  def
            |  WsOrNl
            |  MethodIdentifier
            |   id
            |  MethodParameterPart
            |   (
            |   Parameters
            |    Parameter
            |     MandatoryParameter
            |      x
            |   )
            |  =
            |  WsOrNl
            |  PrimaryExpression
            |   VariableReferencePrimary
            |    VariableIdentifierVariableReference
            |     VariableIdentifier
            |      x""".stripMargin
      }
    }

    "not be recognized" when {

      // This test exists to make sure that `foo2=` is not parsed as an endless method, as
      // endless methods cannot end in `=`.
      "its name ends in `=`" in {
        val code =
          """def foo1
            |end
            |def foo2=(arg)
            |end
            |""".stripMargin

        printAst(_.compoundStatement(), code) shouldEqual
          """CompoundStatement
            | Statements
            |  ExpressionOrCommandStatement
            |   ExpressionExpressionOrCommand
            |    PrimaryExpression
            |     MethodDefinitionPrimary
            |      MethodDefinition
            |       def
            |       WsOrNl
            |       SimpleMethodNamePart
            |        DefinedMethodName
            |         MethodName
            |          MethodIdentifier
            |           foo1
            |       MethodParameterPart
            |       Separator
            |       BodyStatement
            |        CompoundStatement
            |       end
            |  Separators
            |   Separator
            |  ExpressionOrCommandStatement
            |   ExpressionExpressionOrCommand
            |    PrimaryExpression
            |     MethodDefinitionPrimary
            |      MethodDefinition
            |       def
            |       WsOrNl
            |       SimpleMethodNamePart
            |        DefinedMethodName
            |         AssignmentLikeMethodIdentifier
            |          foo2
            |          =
            |       MethodParameterPart
            |        (
            |        Parameters
            |         Parameter
            |          MandatoryParameter
            |           arg
            |        )
            |       Separator
            |       BodyStatement
            |        CompoundStatement
            |       end
            | Separators
            |  Separator""".stripMargin

      }
    }
  }

  "method definition with proc parameters" should {
    "have correct structure for proc parameters with name" in {
      val code =
        """def foo(&block)
          |   yield
          |end
          |""".stripMargin

      printAst(_.primary(), code) shouldBe
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
          |    Parameter
          |     ProcParameter
          |      &
          |      block
          |   )
          |  Separator
          |  WsOrNl
          |  BodyStatement
          |   CompoundStatement
          |    Statements
          |     ExpressionOrCommandStatement
          |      ExpressionExpressionOrCommand
          |       PrimaryExpression
          |        YieldWithOptionalArgumentPrimary
          |         YieldWithOptionalArgument
          |          yield
          |    Separators
          |     Separator
          |  end""".stripMargin
    }

    "have correct structure for proc parameters with no name" in {
      val code =
        """def foo(&)
          |   yield
          |end
          |""".stripMargin

      printAst(_.primary(), code) shouldBe
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
          |    Parameter
          |     ProcParameter
          |      &
          |   )
          |  Separator
          |  WsOrNl
          |  BodyStatement
          |   CompoundStatement
          |    Statements
          |     ExpressionOrCommandStatement
          |      ExpressionExpressionOrCommand
          |       PrimaryExpression
          |        YieldWithOptionalArgumentPrimary
          |         YieldWithOptionalArgument
          |          yield
          |    Separators
          |     Separator
          |  end""".stripMargin
    }
  }

}
