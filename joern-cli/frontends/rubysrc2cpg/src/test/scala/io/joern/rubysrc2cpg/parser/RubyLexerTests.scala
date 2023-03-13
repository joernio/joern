package io.joern.rubysrc2cpg.parser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.joern.rubysrc2cpg.parser.RubyLexer._
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import org.antlr.v4.runtime.Token.EOF

class RubyLexerTests extends AnyFlatSpec with Matchers {

  def tokenize(code: String): Iterable[Int] = {
    import scala.jdk.CollectionConverters.CollectionHasAsScala
    val stream =  new CommonTokenStream(new RubyLexer(CharStreams.fromString(code)))
    stream.fill() // Run the lexer
    stream.getTokens.asScala.map(_.getType)
  }

  "Single-line comments" should "be discarded" in {
    val code =
      """
        |# comment 1
        |  #comment 2
        |## comment 3
        |""".stripMargin

    tokenize(code) shouldBe Seq(NL, NL, WS, NL, NL, EOF)
  }

  "Multi-line comments" should "only be allowed if they start and end on the first column" in {
    val code =
      """
        |=begin Everything delimited by this =begin..=end
        |block is ignored.
        |  =end
        |=end This is the real closing delimiter
        |""".stripMargin

    tokenize(code) shouldBe Seq(NL, EOF)
  }

  "End-of-program marker (`__END__`)" should "only be recognized if it's on a line of its own" in {
    val code =
      """
        |# not valid:
        |__END__ # comment
        | __END__
        |# valid:
        |__END__
        |This is now part of the data section and thus removed from the
        |main lexer channel.  Even __END__ is removed from the main channel.
        |""".stripMargin

    tokenize(code) shouldBe Seq(NL, NL, LOCAL_VARIABLE_IDENTIFIER, WS, NL, WS, LOCAL_VARIABLE_IDENTIFIER, NL, NL, EOF)
  }

  "Prefixed decimal integer literals" should "be recognized as such" in {
    val eg = Seq("0d123456789", "0d1_2_3", "0d0")
    all(eg.map(tokenize)) shouldBe Seq(DECIMAL_INTEGER_LITERAL, EOF)
  }

  "Non-prefixed decimal integer literals" should "be recognized as such" in {
    val eg = Seq("123456789", "1_2_3", "0")
    all(eg.map(tokenize)) shouldBe Seq(DECIMAL_INTEGER_LITERAL, EOF)
  }

  "Non-prefixed octal integer literals" should "be not be mistaken for decimal integer literals" in {
    val eg = Seq("07", "01_2", "01", "0_123", "00")
    all(eg.map(tokenize)) shouldBe Seq(OCTAL_INTEGER_LITERAL, EOF)
  }

  "Prefixed octal integer literals" should "be recognized as such" in {
    val eg = Seq("0o0", "0o1_7", "0o1_2_3")
    all(eg.map(tokenize)) shouldBe Seq(OCTAL_INTEGER_LITERAL, EOF)
  }

  "Binary integer literals" should "be recognized as such" in {
    val eg = Seq("0b0", "0b1", "0b11", "0b1_0", "0b0_1_0")
    all(eg.map(tokenize)) shouldBe Seq(BINARY_INTEGER_LITERAL, EOF)
  }

  "Hexadecimal integer literals" should "be recognized as such" in {
    val eg = Seq("0xA", "0x0_f1", "0x0abcFF_8")
    all(eg.map(tokenize)) shouldBe Seq(HEXADECIMAL_INTEGER_LITERAL, EOF)
  }

  "Floating-point literals without exponent" should "be recognized as such" in {
    val eg = Seq("0.0", "1_2.2_1")
    all(eg.map(tokenize)) shouldBe Seq(FLOAT_LITERAL_WITHOUT_EXPONENT, EOF)
  }

  "Floating-point literals with exponent" should "be recognized as such" in {
    val eg = Seq("0e0", "1E+10", "12e-10", "1.2e4")
    all(eg.map(tokenize)) shouldBe Seq(FLOAT_LITERAL_WITH_EXPONENT, EOF)
  }

  "Keyword-named symbols" should "be recognized as such" in {
    val eg = Seq(":while", ":def", ":if")
    all(eg.map(tokenize)) shouldBe Seq(SYMBOL_LITERAL, EOF)
  }

  "Operator-named symbols" should "be recognized as such" in {
    val eg = Seq(":^", ":==", ":[]", ":[]=", ":+", ":%", ":**", ":>>", ":+@")
    all(eg.map(tokenize)) shouldBe Seq(SYMBOL_LITERAL, EOF)
  }

  "Local variable identifiers" should "be recognized as such" in {
    val eg = Seq("i", "x1", "old_value", "_internal", "_while")
    all(eg.map(tokenize)) shouldBe Seq(LOCAL_VARIABLE_IDENTIFIER, EOF)
  }

  "Constant identifiers" should "be recognized as such" in {
    val eg = Seq("PI", "Const")
    all(eg.map(tokenize)) shouldBe Seq(CONSTANT_IDENTIFIER, EOF)
  }

  "Global variable identifiers" should "be recognized as such" in {
    val eg = Seq("$foo", "$Foo", "$_", "$__foo")
    all(eg.map(tokenize)) shouldBe Seq(GLOBAL_VARIABLE_IDENTIFIER, EOF)
  }

  "Instance variable identifiers" should "be recognized as such" in {
    val eg = Seq("@x", "@_int", "@if", "@_", "@X0")
    all(eg.map(tokenize)) shouldBe Seq(INSTANCE_VARIABLE_IDENTIFIER, EOF)
  }

  "Class variable identifiers" should "be recognized as such" in {
    val eg = Seq("@@counter", "@@if", "@@While_0")
    all(eg.map(tokenize)) shouldBe Seq(CLASS_VARIABLE_IDENTIFIER, EOF)
  }

  "Single-quoted string literals" should "be recognized as such" in {
    val eg = Seq("''", "'\nx'", "'\\''", "'\\'\n\\\''")
    all(eg.map(tokenize)) shouldBe Seq(SINGLE_QUOTED_STRING_LITERAL, EOF)
  }

  "Non-interpolated, non-escaped double-quoted string literals" should "be recognized as such" in {
    val eg = Seq("\"something\"", "\"x\n\"")
    all(eg.map(tokenize)) shouldBe Seq(DOUBLE_QUOTED_STRING_START, DOUBLE_QUOTED_STRING_CHARACTER_SEQUENCE, DOUBLE_QUOTED_STRING_END, EOF)
  }

  "Double-quoted string literals containing identifier interpolations" should "be recognized as such" in {
    val eg = Seq("\"$x = #$x\"", "\"@xyz = #@xyz\"", "\"@@counter = #@@counter\"")
    all(eg.map(tokenize)) shouldBe Seq(
      DOUBLE_QUOTED_STRING_START,
      DOUBLE_QUOTED_STRING_CHARACTER_SEQUENCE,
      INTERPOLATED_CHARACTER_SEQUENCE,
      DOUBLE_QUOTED_STRING_END,
      EOF)
  }

  "Double-quoted string literals containing escaped `#` characters" should "not be mistaken for interpolations" in {
    val code = "\"x = \\#$x\""
    tokenize(code) shouldBe Seq(
      DOUBLE_QUOTED_STRING_START,
      DOUBLE_QUOTED_STRING_CHARACTER_SEQUENCE,
      DOUBLE_QUOTED_STRING_END,
      EOF
    )
  }

  "Double-quoted string literals containing `#`" should "not be mistaken for interpolations" in {
    val code = "\"x = #\""
    tokenize(code) shouldBe Seq(DOUBLE_QUOTED_STRING_START, DOUBLE_QUOTED_STRING_CHARACTER_SEQUENCE, DOUBLE_QUOTED_STRING_END, EOF)
  }

  "Interpolated double-quoted string literal" should "be recognized as such" in {
    val code = "\"x is #{1+1}\""
    tokenize(code) shouldBe Seq(
      DOUBLE_QUOTED_STRING_START,
      DOUBLE_QUOTED_STRING_CHARACTER_SEQUENCE,
      STRING_INTERPOLATION_BEGIN,
      DECIMAL_INTEGER_LITERAL,
      PLUS,
      DECIMAL_INTEGER_LITERAL,
      STRING_INTERPOLATION_END,
      DOUBLE_QUOTED_STRING_END,
      EOF)
  }

  "Recursively interpolated double-quoted string literal" should "be recognized as such" in {
    val code = "\"x is #{\"#{1+1}\"}!\""
    tokenize(code) shouldBe Seq(
      DOUBLE_QUOTED_STRING_START,
      DOUBLE_QUOTED_STRING_CHARACTER_SEQUENCE,
      STRING_INTERPOLATION_BEGIN,
      DOUBLE_QUOTED_STRING_START,
      STRING_INTERPOLATION_BEGIN,
      DECIMAL_INTEGER_LITERAL,
      PLUS,
      DECIMAL_INTEGER_LITERAL,
      STRING_INTERPOLATION_END,
      DOUBLE_QUOTED_STRING_END,
      STRING_INTERPOLATION_END,
      DOUBLE_QUOTED_STRING_CHARACTER_SEQUENCE,
      DOUBLE_QUOTED_STRING_END,
      EOF)
  }

  "Escaped `\"` in double-quoted string literal" should "not be mistaken for end of string" in {
    val code = "\"x is \\\"4\\\"\""
    tokenize(code) shouldBe Seq(DOUBLE_QUOTED_STRING_START, DOUBLE_QUOTED_STRING_CHARACTER_SEQUENCE, DOUBLE_QUOTED_STRING_END, EOF)
  }


}
