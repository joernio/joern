package io.joern.rubysrc2cpg.deprecated.parser

class BeginStatementTests extends RubyParserAbstractTest {

  "BEGIN statement" should {

    "be parsed as a statement" when {

      "defined in a single line" in {
        val code = "BEGIN { 1 }"
        printAst(_.statement(), code) shouldEqual
          """BeginStatement
            | BEGIN
            | {
            | CompoundStatement
            |  Statements
            |   ExpressionOrCommandStatement
            |    ExpressionExpressionOrCommand
            |     PrimaryExpression
            |      LiteralPrimary
            |       NumericLiteralLiteral
            |        NumericLiteral
            |         UnsignedNumericLiteral
            |          1
            | }""".stripMargin
      }

      "empty (single-line)" in {
        val code = "BEGIN {}"
        printAst(_.statement(), code) shouldEqual
          """BeginStatement
            | BEGIN
            | {
            | CompoundStatement
            | }""".stripMargin
      }
    }
  }

}
