package io.joern.rubysrc2cpg.parser

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

      "it uses the %i[ ] delimiters" in {
        val code = "%i[]"
        printAst(_.primary(), code) shouldEqual
          """ArrayConstructorPrimary
            | NonExpandedSymbolArrayConstructor
            |  %i[
            |  ]""".stripMargin
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
            |  NonExpandedWordArrayElements
            |   NonExpandedWordArrayElement
            |    x
            |   NonExpandedWordArrayElement
            |    y
            |   NonExpandedWordArrayElement
            |    z
            |  ]""".stripMargin
      }

      "it uses the %w( ) delimiters" in {
        val code = "%w(x y z)"
        printAst(_.primary(), code) shouldEqual
          """ArrayConstructorPrimary
            | NonExpandedWordArrayConstructor
            |  %w(
            |  NonExpandedWordArrayElements
            |   NonExpandedWordArrayElement
            |    x
            |   NonExpandedWordArrayElement
            |    y
            |   NonExpandedWordArrayElement
            |    z
            |  )""".stripMargin
      }

      "it uses the %w{ } delimiters" in {
        val code = "%w{x y z}"
        printAst(_.primary(), code) shouldEqual
          """ArrayConstructorPrimary
            | NonExpandedWordArrayConstructor
            |  %w{
            |  NonExpandedWordArrayElements
            |   NonExpandedWordArrayElement
            |    x
            |   NonExpandedWordArrayElement
            |    y
            |   NonExpandedWordArrayElement
            |    z
            |  }""".stripMargin
      }

      "it uses the %w< > delimiters" in {
        val code = "%w<x\\ y>"
        printAst(_.primary(), code) shouldEqual
          """ArrayConstructorPrimary
            | NonExpandedWordArrayConstructor
            |  %w<
            |  NonExpandedWordArrayElements
            |   NonExpandedWordArrayElement
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
            |  NonExpandedWordArrayElements
            |   NonExpandedWordArrayElement
            |    x
            |   NonExpandedWordArrayElement
            |    y
            |   NonExpandedWordArrayElement
            |    z
            |  -""".stripMargin
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
            |  NonExpandedSymbolArrayElements
            |   NonExpandedSymbolArrayElement
            |    x
            |   NonExpandedSymbolArrayElement
            |    y
            |  >""".stripMargin
      }

      "it uses the %i{ } delimiters" in {
        val code = "%i{x\\ y}"
        printAst(_.primary(), code) shouldEqual
          """ArrayConstructorPrimary
            | NonExpandedSymbolArrayConstructor
            |  %i{
            |  NonExpandedSymbolArrayElements
            |   NonExpandedSymbolArrayElement
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
            |  NonExpandedSymbolArrayElements
            |   NonExpandedSymbolArrayElement
            |    x
            |   NonExpandedSymbolArrayElement
            |    [
            |    y
            |    ]
            |  ]""".stripMargin

      }
    }
  }
}
