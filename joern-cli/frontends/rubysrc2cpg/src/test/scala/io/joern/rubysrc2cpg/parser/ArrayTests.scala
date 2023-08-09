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
    }
  }
}
