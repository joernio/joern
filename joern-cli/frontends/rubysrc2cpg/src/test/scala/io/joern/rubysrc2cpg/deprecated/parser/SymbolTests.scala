package io.joern.rubysrc2cpg.deprecated.parser

class SymbolTests extends RubyParserAbstractTest {

  "Symbol literals" should {

    "be parsed as primary expressions" when {

      def symbolLiteralParseTreeText(symbolName: String): String =
        s"""LiteralPrimary
           | SymbolLiteral
           |  Symbol
           |   $symbolName""".stripMargin

      "they are named after keywords" in {
        val eg = Seq(
          ":__LINE__",
          ":__ENCODING__",
          ":__FILE__",
          ":BEGIN",
          ":END",
          ":alias",
          ":begin",
          ":break",
          ":case",
          ":class",
          ":def",
          ":defined?",
          ":do",
          ":else",
          ":elsif",
          ":end",
          ":ensure",
          ":for",
          ":false",
          ":if",
          ":in",
          ":module",
          ":next",
          ":nil",
          ":not",
          ":or",
          ":redo",
          ":rescue",
          ":retry",
          ":self",
          ":super",
          ":then",
          ":true",
          ":undef",
          ":unless",
          ":until",
          ":when",
          ":while",
          ":yield"
        )
        eg.map(code => printAst(_.primary(), code)) shouldEqual eg.map(symbolLiteralParseTreeText)
      }

      "they are named after operators" in {
        val eg = Seq(
          ":^",
          ":&",
          ":|",
          ":<=>",
          ":==",
          ":===",
          ":=~",
          ":>",
          ":>=",
          ":<",
          ":<=",
          ":<<",
          ":>>",
          ":+",
          ":-",
          ":*",
          ":/",
          ":%",
          ":**",
          ":~",
          ":+@",
          ":-@",
          ":[]",
          ":[]="
        )
        eg.map(code => printAst(_.primary(), code)) shouldEqual eg.map(symbolLiteralParseTreeText)
      }

      "they are given by a non-interpolated double-quoted string literal" in {
        val code = """:"x y z""""
        printAst(_.primary(), code) shouldEqual
          """LiteralPrimary
            | SymbolLiteral
            |  Symbol
            |   :
            |   SimpleStringExpression
            |    DoubleQuotedStringLiteral
            |     "
            |     x y z
            |     """".stripMargin
      }

      "they are given by an interpolated double-quoted string literal" in {
        val code = """:"#{10}""""
        printAst(_.primary(), code) shouldEqual
          """LiteralPrimary
            | SymbolLiteral
            |  Symbol
            |   :
            |   InterpolatedStringExpression
            |    StringInterpolation
            |     "
            |     InterpolatedStringSequence
            |      #{
            |      CompoundStatement
            |       Statements
            |        ExpressionOrCommandStatement
            |         ExpressionExpressionOrCommand
            |          PrimaryExpression
            |           LiteralPrimary
            |            NumericLiteralLiteral
            |             NumericLiteral
            |              UnsignedNumericLiteral
            |               10
            |      }
            |     """".stripMargin
      }
    }
  }

}
