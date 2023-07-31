package io.joern.rubysrc2cpg.parser

class ArrayTests extends RubyParserAbstractTest {

  "An empty array literal" should {

    "be parsed as a primary expression" when {

      "it uses the traditional [, ] delimiters" in {
        val code = "[]"
        printAst(_.primary(), code) shouldEqual
          """ArrayConstructorPrimary
            | ArrayConstructor
            |  [
            |  ]""".stripMargin
      }

      "it uses the %w[ ] delimiters" in {
        val code = "%w[]"
        printAst(_.primary(), code) shouldEqual
          """ArrayConstructorPrimary
            | ArrayConstructor
            |  %w[
            |  ]""".stripMargin

      }
    }
  }

  "A non-empty array literal" should {

    "be parsed as a primary expression" when {

      "it uses the %w[ ] delimiters" in {
        val code = "%w[x y z]"
        printAst(_.primary(), code) shouldEqual
          """ArrayConstructorPrimary
            | ArrayConstructor
            |  %w[
            |  x
            |  y
            |  z
            |  ]""".stripMargin
      }

      "it uses the %w( ) delimiters" in {
        val code = "%w(x y z)"
        printAst(_.primary(), code) shouldEqual
          """ArrayConstructorPrimary
            | ArrayConstructor
            |  %w(
            |  x
            |  y
            |  z
            |  )""".stripMargin
      }

      "it uses the %w{ } delimiters" in {
        val code = "%w{x y z}"
        printAst(_.primary(), code) shouldEqual
          """ArrayConstructorPrimary
            | ArrayConstructor
            |  %w{
            |  x
            |  y
            |  z
            |  }""".stripMargin
      }

      "it uses the %w- - delimiters" in {
        val code = "%w-x y z-"
        printAst(_.primary(), code) shouldEqual
          """ArrayConstructorPrimary
            | ArrayConstructor
            |  %w-
            |  x
            |  y
            |  z
            |  -""".stripMargin
      }
    }
  }

}
