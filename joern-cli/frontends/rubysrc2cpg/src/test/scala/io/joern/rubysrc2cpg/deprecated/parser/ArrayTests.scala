package io.joern.rubysrc2cpg.deprecated.parser

class ArrayTests extends RubyParserAbstractTest {

  "An empty array literal" should {

    "be parsed as a primary expression" when {

      "it uses the traditional [, ] delimiters" in {
        val code = "[]"
        printAst(_.primary(), code) shouldEqual
          """ArrayConstructorPrimary
            | BracketedArrayConstructor
            |  [
            |  ]""".stripMargin
      }

      "it uses the %w[ ] delimiters" in {
        val code = "%w[]"
        printAst(_.primary(), code) shouldEqual
          """ArrayConstructorPrimary
            | NonExpandedWordArrayConstructor
            |  %w[
            |  ]""".stripMargin
      }

      "it uses the %W< > delimiters" in {
        val code = "%W<>"
        printAst(_.primary(), code) shouldEqual
          """ArrayConstructorPrimary
            | ExpandedWordArrayConstructor
            |  %W<
            |  >""".stripMargin
      }

      "it uses the %i[ ] delimiters" in {
        val code = "%i[]"
        printAst(_.primary(), code) shouldEqual
          """ArrayConstructorPrimary
            | NonExpandedSymbolArrayConstructor
            |  %i[
            |  ]""".stripMargin
      }

      "it uses the %I{ } delimiters" in {
        val code = "%I{}"
        printAst(_.primary(), code) shouldEqual
          """ArrayConstructorPrimary
            | ExpandedSymbolArrayConstructor
            |  %I{
            |  }""".stripMargin
      }
    }
  }

  "A non-empty word array literal" should {

    "be parsed as a primary expression" when {

      "it uses the %w[ ] delimiters" in {
        val code = "%w[x y z]"
        printAst(_.primary(), code) shouldEqual
          """ArrayConstructorPrimary
            | NonExpandedWordArrayConstructor
            |  %w[
            |  NonExpandedArrayElements
            |   NonExpandedArrayElement
            |    x
            |   NonExpandedArrayElement
            |    y
            |   NonExpandedArrayElement
            |    z
            |  ]""".stripMargin
      }

      "it uses the %w( ) delimiters" in {
        val code = "%w(x y z)"
        printAst(_.primary(), code) shouldEqual
          """ArrayConstructorPrimary
            | NonExpandedWordArrayConstructor
            |  %w(
            |  NonExpandedArrayElements
            |   NonExpandedArrayElement
            |    x
            |   NonExpandedArrayElement
            |    y
            |   NonExpandedArrayElement
            |    z
            |  )""".stripMargin
      }

      "it uses the %w{ } delimiters" in {
        val code = "%w{x y z}"
        printAst(_.primary(), code) shouldEqual
          """ArrayConstructorPrimary
            | NonExpandedWordArrayConstructor
            |  %w{
            |  NonExpandedArrayElements
            |   NonExpandedArrayElement
            |    x
            |   NonExpandedArrayElement
            |    y
            |   NonExpandedArrayElement
            |    z
            |  }""".stripMargin
      }

      "it uses the %w< > delimiters" in {
        val code = "%w<x\\ y>"
        printAst(_.primary(), code) shouldEqual
          """ArrayConstructorPrimary
            | NonExpandedWordArrayConstructor
            |  %w<
            |  NonExpandedArrayElements
            |   NonExpandedArrayElement
            |    x
            |    \ 
            |    y
            |  >""".stripMargin
      }

      "it uses the %w- - delimiters" in {
        val code = "%w-x y z-"
        printAst(_.primary(), code) shouldEqual
          """ArrayConstructorPrimary
            | NonExpandedWordArrayConstructor
            |  %w-
            |  NonExpandedArrayElements
            |   NonExpandedArrayElement
            |    x
            |   NonExpandedArrayElement
            |    y
            |   NonExpandedArrayElement
            |    z
            |  -""".stripMargin
      }

      "it spans multiple lines" in {
        val code =
          """%w(
            | bob
            | cod
            | dod
            |)""".stripMargin
        printAst(_.primary(), code) shouldEqual
          """ArrayConstructorPrimary
            | NonExpandedWordArrayConstructor
            |  %w(
            |  NonExpandedArrayElements
            |   NonExpandedArrayElement
            |    b
            |    o
            |    b
            |   NonExpandedArrayElement
            |    c
            |    o
            |    d
            |   NonExpandedArrayElement
            |    d
            |    o
            |    d
            |  )""".stripMargin

      }

      "it uses the %W( ) delimiters and contains a numeric interpolation" in {
        val code = "%W(x#{1})"
        printAst(_.primary(), code) shouldEqual
          """ArrayConstructorPrimary
            | ExpandedWordArrayConstructor
            |  %W(
            |  ExpandedArrayElements
            |   ExpandedArrayElement
            |    x
            |    DelimitedArrayItemInterpolation
            |     #{
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
            |     }
            |  )""".stripMargin
      }

      "it spans multiple lines and contains a numeric interpolation" in {
        val code =
          """%W[
            | x#{0}
            |]""".stripMargin
        printAst(_.primary(), code) shouldEqual
          """ArrayConstructorPrimary
            | ExpandedWordArrayConstructor
            |  %W[
            |  ExpandedArrayElements
            |   ExpandedArrayElement
            |    x
            |    DelimitedArrayItemInterpolation
            |     #{
            |     CompoundStatement
            |      Statements
            |       ExpressionOrCommandStatement
            |        ExpressionExpressionOrCommand
            |         PrimaryExpression
            |          LiteralPrimary
            |           NumericLiteralLiteral
            |            NumericLiteral
            |             UnsignedNumericLiteral
            |              0
            |     }
            |  ]""".stripMargin
      }
    }
  }

  "A non-empty symbol array literal" should {

    "be parsed as a primary expression" when {

      "it uses the %i< > delimiters" in {
        val code = "%i<x y>"
        printAst(_.primary(), code) shouldEqual
          """ArrayConstructorPrimary
            | NonExpandedSymbolArrayConstructor
            |  %i<
            |  NonExpandedArrayElements
            |   NonExpandedArrayElement
            |    x
            |   NonExpandedArrayElement
            |    y
            |  >""".stripMargin
      }

      "it uses the %i{ } delimiters" in {
        val code = "%i{x\\ y}"
        printAst(_.primary(), code) shouldEqual
          """ArrayConstructorPrimary
            | NonExpandedSymbolArrayConstructor
            |  %i{
            |  NonExpandedArrayElements
            |   NonExpandedArrayElement
            |    x
            |    \ 
            |    y
            |  }""".stripMargin
      }

      "it uses the %i[ ] delimiters nestedly" in {
        val code = "%i[x [y]]"
        printAst(_.primary(), code) shouldEqual
          """ArrayConstructorPrimary
            | NonExpandedSymbolArrayConstructor
            |  %i[
            |  NonExpandedArrayElements
            |   NonExpandedArrayElement
            |    x
            |   NonExpandedArrayElement
            |    [
            |    y
            |    ]
            |  ]""".stripMargin

      }

      "it uses the %i( ) delimiters in a multi-line fashion" in {
        val code =
          """%i(
            |x y
            |z
            |)""".stripMargin
        printAst(_.primary(), code) shouldEqual
          """ArrayConstructorPrimary
            | NonExpandedSymbolArrayConstructor
            |  %i(
            |  NonExpandedArrayElements
            |   NonExpandedArrayElement
            |    x
            |   NonExpandedArrayElement
            |    y
            |   NonExpandedArrayElement
            |    z
            |  )""".stripMargin
      }

      "it uses the %I( ) delimiters and contains a numeric interpolation" in {
        val code = "%I(x#{0} x1)"
        printAst(_.primary(), code) shouldEqual
          """ArrayConstructorPrimary
            | ExpandedSymbolArrayConstructor
            |  %I(
            |  ExpandedArrayElements
            |   ExpandedArrayElement
            |    x
            |    DelimitedArrayItemInterpolation
            |     #{
            |     CompoundStatement
            |      Statements
            |       ExpressionOrCommandStatement
            |        ExpressionExpressionOrCommand
            |         PrimaryExpression
            |          LiteralPrimary
            |           NumericLiteralLiteral
            |            NumericLiteral
            |             UnsignedNumericLiteral
            |              0
            |     }
            |   ExpandedArrayElement
            |    x
            |    1
            |  )""".stripMargin
      }
    }
  }
}
