package io.joern.rubysrc2cpg.parser

class SymbolTests extends RubyParserAbstractTest {

  "Keyword-named symbols" should {
    val symWhile = ":while"
    val symDef   = ":def"

    "be parsed as symbols" in {
      prettyPrint(_.symbol(), symWhile) shouldBe
        """Symbol
          | :while
          |""".stripMargin

      prettyPrint(_.symbol(), symDef) shouldBe
        """Symbol
          | :def
          |""".stripMargin
    }

    "be accepted as literals" in {
      accepts(_.literal(), symWhile) shouldBe true
      accepts(_.literal(), symDef) shouldBe true
    }
  }

  "Operator-named symbols" should {
    val symCaret   = ":^"
    val symEq2     = ":=="
    val symLRBrack = ":[]"

    "be parsed as primary expressions" in {
      prettyPrint(_.primary(), symCaret) shouldEqual
        """LiteralPrimary
          | Literal
          |  Symbol
          |   :^
          |""".stripMargin

      prettyPrint(_.primary(), symEq2) shouldEqual
        """LiteralPrimary
          | Literal
          |  Symbol
          |   :==
          |""".stripMargin

      prettyPrint(_.primary(), symLRBrack) shouldEqual
        """LiteralPrimary
          | Literal
          |  Symbol
          |   :[]
          |""".stripMargin
    }
  }
}
