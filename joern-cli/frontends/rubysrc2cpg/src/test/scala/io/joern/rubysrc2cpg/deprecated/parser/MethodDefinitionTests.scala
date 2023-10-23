package io.joern.rubysrc2cpg.deprecated.parser

class MethodDefinitionTests extends RubyParserAbstractTest {

  "A one-line empty method definition" should {

    "be parsed as a primary expression" when {

      "it contains no parameters" in {
        val code = "def foo; end"
        printAst(_.primary(), code) shouldEqual
          """MethodDefinitionPrimary
            | MethodDefinition
            |  def
            |  SimpleMethodNamePart
            |   DefinedMethodName
            |    MethodName
            |     MethodIdentifier
            |      foo
            |  MethodParameterPart
            |  BodyStatement
            |   CompoundStatement
            |    ;
            |  end""".stripMargin
      }

      "it contains a mandatory parameter" in {
        val code = "def foo(x);end"
        printAst(_.primary(), code) shouldEqual
          """MethodDefinitionPrimary
            | MethodDefinition
            |  def
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
            |  BodyStatement
            |   CompoundStatement
            |    ;
            |  end""".stripMargin
      }

      "it contains an optional numeric parameter" in {
        val code = "def foo(x=1);end"
        printAst(_.primary(), code) shouldEqual
          """MethodDefinitionPrimary
            | MethodDefinition
            |  def
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
            |  BodyStatement
            |   CompoundStatement
            |    ;
            |  end""".stripMargin
      }

      "it contains two parameters, the last of which a &-parameter" in {
        val code = "def foo(x, &y); end"
        printAst(_.primary(), code) shouldEqual
          """MethodDefinitionPrimary
            | MethodDefinition
            |  def
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
            |    Parameter
            |     ProcParameter
            |      &
            |      y
            |   )
            |  BodyStatement
            |   CompoundStatement
            |    ;
            |  end""".stripMargin
      }

      "it contains a named (array) splatting argument" in {
        val code = "def foo(*arr); end"
        printAst(_.primary(), code) shouldEqual
          """MethodDefinitionPrimary
            | MethodDefinition
            |  def
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
            |  BodyStatement
            |   CompoundStatement
            |    ;
            |  end""".stripMargin
      }

      "it contains a named (hash) splatting argument" in {
        val code = "def foo(**hash); end"
        printAst(_.primary(), code) shouldEqual
          """MethodDefinitionPrimary
            | MethodDefinition
            |  def
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
            |  BodyStatement
            |   CompoundStatement
            |    ;
            |  end""".stripMargin
      }

      "it contains both a named array and hash splatting argument" in {
        val code = "def foo(*arr, **hash); end"
        printAst(_.primary(), code) shouldEqual
          """MethodDefinitionPrimary
            | MethodDefinition
            |  def
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
            |    Parameter
            |     HashParameter
            |      **
            |      hash
            |   )
            |  BodyStatement
            |   CompoundStatement
            |    ;
            |  end""".stripMargin
      }

      "it contains an optional parameter before a mandatory one" in {
        val code = "def foo(x=1,y); end"
        printAst(_.primary(), code) shouldEqual
          """MethodDefinitionPrimary
            | MethodDefinition
            |  def
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
            |  BodyStatement
            |   CompoundStatement
            |    ;
            |  end""".stripMargin
      }

      "it contains a keyword parameter" in {
        val code = "def foo(x: 1); end"
        printAst(_.primary(), code) shouldEqual
          """MethodDefinitionPrimary
            | MethodDefinition
            |  def
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
            |      PrimaryExpression
            |       LiteralPrimary
            |        NumericLiteralLiteral
            |         NumericLiteral
            |          UnsignedNumericLiteral
            |           1
            |   )
            |  BodyStatement
            |   CompoundStatement
            |    ;
            |  end""".stripMargin
      }

      "it contains a mandatory keyword parameter" in {
        val code = "def foo(x:) ; end"
        printAst(_.primary(), code) shouldBe
          """MethodDefinitionPrimary
            | MethodDefinition
            |  def
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
            |  BodyStatement
            |   CompoundStatement
            |    ;
            |  end""".stripMargin
      }

      "it contains two mandatory keyword parameters" in {
        val code = "def foo(name:, surname:) ; end"
        printAst(_.primary(), code) shouldBe
          """MethodDefinitionPrimary
            | MethodDefinition
            |  def
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
            |    Parameter
            |     KeywordParameter
            |      surname
            |      :
            |   )
            |  BodyStatement
            |   CompoundStatement
            |    ;
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
            |  SimpleMethodNamePart
            |   DefinedMethodName
            |    MethodName
            |     MethodIdentifier
            |      foo
            |  MethodParameterPart
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
            |   RescueClause
            |    rescue
            |    ExceptionClass
            |     PrimaryExpression
            |      VariableReferencePrimary
            |       VariableIdentifierVariableReference
            |        VariableIdentifier
            |         ZeroDivisionError
            |    ExceptionVariableAssignment
            |     =>
            |     VariableIdentifierOnlySingleLeftHandSide
            |      VariableIdentifier
            |       e
            |    ThenClause
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
            |  MethodIdentifier
            |   foo
            |  MethodParameterPart
            |  =
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
            |  MethodIdentifier
            |   foo
            |  MethodParameterPart
            |  =
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
            |  MethodIdentifier
            |   foo
            |  MethodParameterPart
            |  =
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
            |       SimpleMethodNamePart
            |        DefinedMethodName
            |         MethodName
            |          MethodIdentifier
            |           foo1
            |       MethodParameterPart
            |       BodyStatement
            |        CompoundStatement
            |       end
            |  ExpressionOrCommandStatement
            |   ExpressionExpressionOrCommand
            |    PrimaryExpression
            |     MethodDefinitionPrimary
            |      MethodDefinition
            |       def
            |       SimpleMethodNamePart
            |        DefinedMethodName
            |         AssignmentLikeMethodIdentifier
            |          foo2=
            |       MethodParameterPart
            |        (
            |        Parameters
            |         Parameter
            |          MandatoryParameter
            |           arg
            |        )
            |       BodyStatement
            |        CompoundStatement
            |       end""".stripMargin

      }

      // This test makes sure that the `end` after `def foo2=` is not parsed as part of its definition,
      // which could happen if `foo2=` was parsed as two separate tokens (LOCAL_VARIABLE_IDENTIFIER, EQ)
      // instead of just ASSIGNMENT_LIKE_METHOD_IDENTIFIER.
      // Issue report: https://github.com/joernio/joern/issues/3270
      "its name ends in `=` and the next keyword is `end`" in {
        val code =
          """module SomeModule
            |def foo1
            |    return unless true
            |end
            |def foo2=(arg)
            |end
            |end
            |""".stripMargin
        printAst(_.compoundStatement(), code) shouldEqual
          """CompoundStatement
            | Statements
            |  ExpressionOrCommandStatement
            |   ExpressionExpressionOrCommand
            |    PrimaryExpression
            |     ModuleDefinitionPrimary
            |      ModuleDefinition
            |       module
            |       ClassOrModuleReference
            |        SomeModule
            |       BodyStatement
            |        CompoundStatement
            |         Statements
            |          ExpressionOrCommandStatement
            |           ExpressionExpressionOrCommand
            |            PrimaryExpression
            |             MethodDefinitionPrimary
            |              MethodDefinition
            |               def
            |               SimpleMethodNamePart
            |                DefinedMethodName
            |                 MethodName
            |                  MethodIdentifier
            |                   foo1
            |               MethodParameterPart
            |               BodyStatement
            |                CompoundStatement
            |                 Statements
            |                  ModifierStatement
            |                   ExpressionOrCommandStatement
            |                    InvocationExpressionOrCommand
            |                     ReturnArgsInvocationWithoutParentheses
            |                      return
            |                   unless
            |                   ExpressionOrCommandStatement
            |                    ExpressionExpressionOrCommand
            |                     PrimaryExpression
            |                      VariableReferencePrimary
            |                       PseudoVariableIdentifierVariableReference
            |                        TruePseudoVariableIdentifier
            |                         true
            |               end
            |          ExpressionOrCommandStatement
            |           ExpressionExpressionOrCommand
            |            PrimaryExpression
            |             MethodDefinitionPrimary
            |              MethodDefinition
            |               def
            |               SimpleMethodNamePart
            |                DefinedMethodName
            |                 AssignmentLikeMethodIdentifier
            |                  foo2=
            |               MethodParameterPart
            |                (
            |                Parameters
            |                 Parameter
            |                  MandatoryParameter
            |                   arg
            |                )
            |               BodyStatement
            |                CompoundStatement
            |               end
            |       end""".stripMargin
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
          |  BodyStatement
          |   CompoundStatement
          |    Statements
          |     ExpressionOrCommandStatement
          |      ExpressionExpressionOrCommand
          |       PrimaryExpression
          |        YieldWithOptionalArgumentPrimary
          |         YieldWithOptionalArgument
          |          yield
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
          |  BodyStatement
          |   CompoundStatement
          |    Statements
          |     ExpressionOrCommandStatement
          |      ExpressionExpressionOrCommand
          |       PrimaryExpression
          |        YieldWithOptionalArgumentPrimary
          |         YieldWithOptionalArgument
          |          yield
          |  end""".stripMargin
    }
  }

  "method definition for mandatory parameters" should {
    "have correct structure for mandatory parameters" in {
      val code = "def foo(bar:) end"
      printAst(_.primary(), code) shouldBe
        """MethodDefinitionPrimary
          | MethodDefinition
          |  def
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
          |      bar
          |      :
          |   )
          |  BodyStatement
          |   CompoundStatement
          |  end""".stripMargin
    }

    "have correct structure for a hash created using a method" in {
      val code =
        """def data
          |    {
          |     first_link:,
          |     action_link_group:,
          |    }
          |end""".stripMargin

      printAst(_.primary(), code) shouldBe
        """MethodDefinitionPrimary
          | MethodDefinition
          |  def
          |  SimpleMethodNamePart
          |   DefinedMethodName
          |    MethodName
          |     MethodIdentifier
          |      data
          |  MethodParameterPart
          |  BodyStatement
          |   CompoundStatement
          |    Statements
          |     ExpressionOrCommandStatement
          |      ExpressionExpressionOrCommand
          |       PrimaryExpression
          |        HashConstructorPrimary
          |         HashConstructor
          |          {
          |          HashConstructorElements
          |           HashConstructorElement
          |            Association
          |             PrimaryExpression
          |              VariableReferencePrimary
          |               VariableIdentifierVariableReference
          |                VariableIdentifier
          |                 first_link
          |             :
          |           ,
          |           HashConstructorElement
          |            Association
          |             PrimaryExpression
          |              VariableReferencePrimary
          |               VariableIdentifierVariableReference
          |                VariableIdentifier
          |                 action_link_group
          |             :
          |          ,
          |          }
          |  end""".stripMargin
    }

    "have correct structure when a method parameter is defined using whitespace" in {
      val code =
        """class SampleClass
          |  def sample_method( first_param:, second_param:)
          |  end
          |end
          |""".stripMargin

      printAst(_.primary(), code) shouldBe
        """ClassDefinitionPrimary
          | ClassDefinition
          |  class
          |  ClassOrModuleReference
          |   SampleClass
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
          |              sample_method
          |          MethodParameterPart
          |           (
          |           Parameters
          |            Parameter
          |             KeywordParameter
          |              first_param
          |              :
          |            ,
          |            Parameter
          |             KeywordParameter
          |              second_param
          |              :
          |           )
          |          BodyStatement
          |           CompoundStatement
          |          end
          |  end""".stripMargin
    }

    "have correct structure when method parameters are defined using new line" in {
      val code =
        """class SomeClass
          |  def initialize(
          |              name, age)
          |  end
          |end
          |""".stripMargin

      printAst(_.primary(), code) shouldBe
        """ClassDefinitionPrimary
          | ClassDefinition
          |  class
          |  ClassOrModuleReference
          |   SomeClass
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
          |              initialize
          |          MethodParameterPart
          |           (
          |           Parameters
          |            Parameter
          |             MandatoryParameter
          |              name
          |            ,
          |            Parameter
          |             MandatoryParameter
          |              age
          |           )
          |          BodyStatement
          |           CompoundStatement
          |          end
          |  end""".stripMargin
    }

    "have correct structure when method parameters are defined using wsOrNL" in {
      val code =
        """class SomeClass
          |  def initialize(
          |              name, age
          |              )
          |  end
          |end
          |""".stripMargin

      printAst(_.primary(), code) shouldBe
        """ClassDefinitionPrimary
          | ClassDefinition
          |  class
          |  ClassOrModuleReference
          |   SomeClass
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
          |              initialize
          |          MethodParameterPart
          |           (
          |           Parameters
          |            Parameter
          |             MandatoryParameter
          |              name
          |            ,
          |            Parameter
          |             MandatoryParameter
          |              age
          |           )
          |          BodyStatement
          |           CompoundStatement
          |          end
          |  end""".stripMargin
    }

    "have correct structure when keyword parameters are defined using wsOrNL" in {
      val code =
        """class SomeClass
          |  def initialize(
          |              name: nil, age
          |              )
          |  end
          |end
          |""".stripMargin

      printAst(_.primary(), code) shouldBe
        """ClassDefinitionPrimary
          | ClassDefinition
          |  class
          |  ClassOrModuleReference
          |   SomeClass
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
          |              initialize
          |          MethodParameterPart
          |           (
          |           Parameters
          |            Parameter
          |             KeywordParameter
          |              name
          |              :
          |              PrimaryExpression
          |               VariableReferencePrimary
          |                PseudoVariableIdentifierVariableReference
          |                 NilPseudoVariableIdentifier
          |                  nil
          |            ,
          |            Parameter
          |             MandatoryParameter
          |              age
          |           )
          |          BodyStatement
          |           CompoundStatement
          |          end
          |  end""".stripMargin
    }
  }

}
