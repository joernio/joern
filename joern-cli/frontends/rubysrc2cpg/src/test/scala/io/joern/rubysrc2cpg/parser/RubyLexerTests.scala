package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.parser.RubyLexer._
import org.antlr.v4.runtime.Token.EOF
import org.antlr.v4.runtime._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RubyLexerTests extends AnyFlatSpec with Matchers {

  class RubySyntaxErrorListener extends BaseErrorListener {
    var errors = 0
    override def syntaxError(
      recognizer: Recognizer[_, _],
      offendingSymbol: Any,
      line: Int,
      charPositionInLine: Int,
      msg: String,
      e: RecognitionException
    ): Unit =
      errors += 1
  }

  def tokenize(code: String): Iterable[Int] = {
    val lexer               = new RubyLexer(CharStreams.fromString(code))
    val syntaxErrorListener = new RubySyntaxErrorListener
    lexer.addErrorListener(syntaxErrorListener)
    val stream = new CommonTokenStream(lexer)
    stream.fill() // Run the lexer
    if (syntaxErrorListener.errors > 0) {
      Seq()
    } else {
      import scala.jdk.CollectionConverters.CollectionHasAsScala
      stream.getTokens.asScala.map(_.getType)
    }
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
    all(eg.map(tokenize)) shouldBe Seq(
      DOUBLE_QUOTED_STRING_START,
      DOUBLE_QUOTED_STRING_CHARACTER_SEQUENCE,
      DOUBLE_QUOTED_STRING_END,
      EOF
    )
  }

  "Double-quoted string literals containing identifier interpolations" should "be recognized as such" in {
    val eg = Seq("\"$x = #$x\"", "\"@xyz = #@xyz\"", "\"@@counter = #@@counter\"")
    all(eg.map(tokenize)) shouldBe Seq(
      DOUBLE_QUOTED_STRING_START,
      DOUBLE_QUOTED_STRING_CHARACTER_SEQUENCE,
      INTERPOLATED_CHARACTER_SEQUENCE,
      DOUBLE_QUOTED_STRING_END,
      EOF
    )
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
    tokenize(code) shouldBe Seq(
      DOUBLE_QUOTED_STRING_START,
      DOUBLE_QUOTED_STRING_CHARACTER_SEQUENCE,
      DOUBLE_QUOTED_STRING_END,
      EOF
    )
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
      EOF
    )
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
      EOF
    )
  }

  "Escaped `\"` in double-quoted string literal" should "not be mistaken for end of string" in {
    val code = "\"x is \\\"4\\\"\""
    tokenize(code) shouldBe Seq(
      DOUBLE_QUOTED_STRING_START,
      DOUBLE_QUOTED_STRING_CHARACTER_SEQUENCE,
      DOUBLE_QUOTED_STRING_END,
      EOF
    )
  }

  "Empty regex literal" should "be recognized as such" in {
    val code = "//"
    tokenize(code) shouldBe Seq(REGULAR_EXPRESSION_START, REGULAR_EXPRESSION_END, EOF)
  }

  "Empty regex literal on the RHS of an assignment" should "be recognized as such" in {
    // This test exists to check if RubyLexer properly decided between SLASH and REGULAR_EXPRESSION_START
    val code = "x = //"
    tokenize(code) shouldBe Seq(
      LOCAL_VARIABLE_IDENTIFIER,
      WS,
      EQ,
      WS,
      REGULAR_EXPRESSION_START,
      REGULAR_EXPRESSION_END,
      EOF
    )
  }

  "Empty regex literal on the RHS of an association" should "be recognized as such" in {
    val code = "{x: //}"
    tokenize(code) shouldBe Seq(
      LCURLY,
      LOCAL_VARIABLE_IDENTIFIER,
      COLON,
      WS,
      REGULAR_EXPRESSION_START,
      REGULAR_EXPRESSION_END,
      RCURLY,
      EOF
    )
  }

  "Non-empty regex literal on the RHS of a keyword argument" should "be recognized as such" in {
    val code = "foo(x: /.*/)"
    tokenize(code) shouldBe Seq(
      LOCAL_VARIABLE_IDENTIFIER,
      LPAREN,
      LOCAL_VARIABLE_IDENTIFIER,
      COLON,
      WS,
      REGULAR_EXPRESSION_START,
      REGULAR_EXPRESSION_BODY,
      REGULAR_EXPRESSION_END,
      RPAREN,
      EOF
    )
  }

  "Non-empty regex literal on the RHS of an assignment" should "be recognized as such" in {
    val code = """NAME_REGEX = /\A[^0-9!\``@#\$%\^&*+_=]+\z/"""
    tokenize(code) shouldBe Seq(
      CONSTANT_IDENTIFIER,
      WS,
      EQ,
      WS,
      REGULAR_EXPRESSION_START,
      REGULAR_EXPRESSION_BODY,
      REGULAR_EXPRESSION_END,
      EOF
    )
  }

  "Regex literals without metacharacters" should "be recognized as such" in {
    val eg = Seq("/regexp/", "/a regexp/")
    all(eg.map(tokenize)) shouldBe Seq(REGULAR_EXPRESSION_START, REGULAR_EXPRESSION_BODY, REGULAR_EXPRESSION_END, EOF)
  }

  "Regex literals with metacharacters" should "be recognized as such" in {
    val eg = Seq("/(us|eu)/", "/[a-z]/", "/[A-Z]*/", "/(us|eu)?/", "/[0-9]+/")
    all(eg.map(tokenize)) shouldBe Seq(REGULAR_EXPRESSION_START, REGULAR_EXPRESSION_BODY, REGULAR_EXPRESSION_END, EOF)
  }

  "Regex literals with character classes" should "be recognized as such" in {
    val eg = Seq("/\\w/", "/\\W/", "/\\S/")
    all(eg.map(tokenize)) shouldBe Seq(REGULAR_EXPRESSION_START, REGULAR_EXPRESSION_BODY, REGULAR_EXPRESSION_END, EOF)
  }

  "Regex literals with groups" should "be recognized as such" in {
    val eg = Seq("/[aeiou]\\w{2}/", "/(\\d{2}:\\d{2}) (\\w+) (.*)/", "/(?<name>\\w+) (?<age>\\d+)/")
    all(eg.map(tokenize)) shouldBe Seq(REGULAR_EXPRESSION_START, REGULAR_EXPRESSION_BODY, REGULAR_EXPRESSION_END, EOF)
  }

  "Regex literals with options" should "be recognized as such" in {
    val eg = Seq("/./m", "/./i", "/./x", "/./o")
    all(eg.map(tokenize)) shouldBe Seq(REGULAR_EXPRESSION_START, REGULAR_EXPRESSION_BODY, REGULAR_EXPRESSION_END, EOF)
  }

  "Interpolated (with a local variable) regex literal" should "be recognized as such" in {
    val code = "/#{foo}/"
    tokenize(code) shouldBe Seq(
      REGULAR_EXPRESSION_START,
      REGULAR_EXPRESSION_INTERPOLATION_BEGIN,
      LOCAL_VARIABLE_IDENTIFIER,
      REGULAR_EXPRESSION_INTERPOLATION_END,
      REGULAR_EXPRESSION_END,
      EOF
    )
  }

  "Interpolated (with a numeric expression) regex literal" should "be recognized as such" in {
    val code = "/#{1+1}/"
    tokenize(code) shouldBe Seq(
      REGULAR_EXPRESSION_START,
      REGULAR_EXPRESSION_INTERPOLATION_BEGIN,
      DECIMAL_INTEGER_LITERAL,
      PLUS,
      DECIMAL_INTEGER_LITERAL,
      REGULAR_EXPRESSION_INTERPOLATION_END,
      REGULAR_EXPRESSION_END,
      EOF
    )
  }

  "Interpolated (with a local variable) regex literal containing also textual body elements" should "be recognized as such" in {
    val code = "/x\\.#{foo}\\./"
    tokenize(code) shouldBe Seq(
      REGULAR_EXPRESSION_START,
      REGULAR_EXPRESSION_BODY,
      REGULAR_EXPRESSION_INTERPOLATION_BEGIN,
      LOCAL_VARIABLE_IDENTIFIER,
      REGULAR_EXPRESSION_INTERPOLATION_END,
      REGULAR_EXPRESSION_BODY,
      REGULAR_EXPRESSION_END,
      EOF
    )
  }

  "Vacuously interpolated regex literal" should "be recognized as such" in {
    val code = "/#{}/"
    tokenize(code) shouldBe Seq(
      REGULAR_EXPRESSION_START,
      REGULAR_EXPRESSION_INTERPOLATION_BEGIN,
      REGULAR_EXPRESSION_INTERPOLATION_END,
      REGULAR_EXPRESSION_END,
      EOF
    )
  }

  "Division operator between identifiers" should "not be confused with regex start" in {
    val code = "x / y"
    tokenize(code) shouldBe Seq(LOCAL_VARIABLE_IDENTIFIER, WS, SLASH, WS, LOCAL_VARIABLE_IDENTIFIER, EOF)
  }

  "Addition between class fields" should "not be confused with +@ token" in {
    // This test exists to check if RubyLexer properly decided between PLUS and PLUSAT
    val code = "x+@y"
    tokenize(code) shouldBe Seq(LOCAL_VARIABLE_IDENTIFIER, PLUS, INSTANCE_VARIABLE_IDENTIFIER, EOF)
  }

  "Subtraction between class fields" should "not be confused with -@ token" in {
    // This test exists to check if RubyLexer properly decided between MINUS and MINUSAT
    val code = "x-@y"
    tokenize(code) shouldBe Seq(LOCAL_VARIABLE_IDENTIFIER, MINUS, INSTANCE_VARIABLE_IDENTIFIER, EOF)
  }

  "Invocation of command with regex literal" should "not be confused with binary division" in {
    val code = "puts /x/"
    tokenize(code) shouldBe Seq(
      LOCAL_VARIABLE_IDENTIFIER,
      WS,
      REGULAR_EXPRESSION_START,
      REGULAR_EXPRESSION_BODY,
      REGULAR_EXPRESSION_END,
      EOF
    )
  }

  "Multi-line string literal concatenation" should "be recognized as two string literals separated by whitespace" in {
    val code =
      """'abc' \
        |'cde'""".stripMargin
    tokenize(code) shouldBe Seq(SINGLE_QUOTED_STRING_LITERAL, WS, SINGLE_QUOTED_STRING_LITERAL, EOF)
  }

  "empty `%q` string literals" should "be recognized as such" in {
    val eg = Seq("%q()", "%q[]", "%q{}", "%q<>", "%q##", "%q!!", "%q--", "%q@@", "%q++", "%q**", "%q//", "%q&&")
    all(eg.map(tokenize)) shouldBe Seq(
      QUOTED_NON_EXPANDED_STRING_LITERAL_START,
      QUOTED_NON_EXPANDED_STRING_LITERAL_END,
      EOF
    )
  }

  "single-character `%q` string literals" should "be recognized as such" in {
    val eg =
      Seq("%q(x)", "%q[y]", "%q{z}", "%q<w>", "%q#a#", "%q!b!", "%q-_-", "%q@c@", "%q+d+", "%q*e*", "%q/#/", "%q&!&")
    all(eg.map(tokenize)) shouldBe Seq(
      QUOTED_NON_EXPANDED_STRING_LITERAL_START,
      QUOTED_NON_EXPANDED_CHARACTER,
      QUOTED_NON_EXPANDED_STRING_LITERAL_END,
      EOF
    )
  }

  "delimiter-escaped-single-character `%q` string literals" should "be recognized as such" in {
    val eg = Seq(
      "%q(\\))",
      "%q[\\]]",
      "%q{\\}}",
      "%q<\\>>",
      "%q#\\##",
      "%q!\\!!",
      "%q-\\--",
      "%q@\\@@",
      "%q+\\++",
      "%q*\\**",
      "%q/\\//",
      "%q&\\&&"
    )
    all(eg.map(tokenize)) shouldBe Seq(
      QUOTED_NON_EXPANDED_STRING_LITERAL_START,
      QUOTED_NON_EXPANDED_CHARACTER,
      QUOTED_NON_EXPANDED_STRING_LITERAL_END,
      EOF
    )
  }

  "nested `%q` string literals" should "be recognized as such" in {
    val eg = Seq("%q(()())", "%q[[][]]", "%q{{}{}}", "%q<<><>>")
    all(eg.map(tokenize)) shouldBe Seq(
      QUOTED_NON_EXPANDED_STRING_LITERAL_START,
      QUOTED_NON_EXPANDED_CHARACTER,
      QUOTED_NON_EXPANDED_CHARACTER,
      QUOTED_NON_EXPANDED_CHARACTER,
      QUOTED_NON_EXPANDED_CHARACTER,
      QUOTED_NON_EXPANDED_STRING_LITERAL_END,
      EOF
    )
  }

  "empty `%w` string array literals" should "be recognized as such" in {
    val eg = Seq("%w()", "%w[]", "%w{}", "%w<>", "%w##", "%w!!", "%w--", "%w@@", "%w++", "%w**", "%w//", "%w&&")
    all(eg.map(tokenize)) shouldBe Seq(
      QUOTED_NON_EXPANDED_STRING_ARRAY_LITERAL_START,
      QUOTED_NON_EXPANDED_STRING_ARRAY_LITERAL_END,
      EOF
    )
  }

  "single-character `%w` string array literals" should "be recognized as such" in {
    val eg =
      Seq("%w(x)", "%w[y]", "%w{z}", "%w<w>", "%w#a#", "%w!b!", "%w-_-", "%w@c@", "%w+d+", "%w*e*", "%w/#/", "%w&!&")
    all(eg.map(tokenize)) shouldBe Seq(
      QUOTED_NON_EXPANDED_STRING_ARRAY_LITERAL_START,
      QUOTED_NON_EXPANDED_STRING_ARRAY_CHARACTER,
      QUOTED_NON_EXPANDED_STRING_ARRAY_LITERAL_END,
      EOF
    )
  }

  "two-word `%w` string array literals" should "be recognized as such" in {
    val eg = Seq(
      "%w(xx y)",
      "%w[yy z]",
      "%w{z0 w}",
      "%w<w; 1>",
      "%w#a& ?#",
      "%w!b_ c!",
      "%w-_= +-",
      "%w@c\" d@",
      "%w+d/ *+",
      "%w*ef <*",
      "%w/#< >/",
      "%w&!! %&"
    )
    all(eg.map(tokenize)) shouldBe Seq(
      QUOTED_NON_EXPANDED_STRING_ARRAY_LITERAL_START,
      QUOTED_NON_EXPANDED_STRING_ARRAY_CHARACTER,
      QUOTED_NON_EXPANDED_STRING_ARRAY_CHARACTER,
      QUOTED_NON_EXPANDED_STRING_ARRAY_SEPARATOR,
      QUOTED_NON_EXPANDED_STRING_ARRAY_CHARACTER,
      QUOTED_NON_EXPANDED_STRING_ARRAY_LITERAL_END,
      EOF
    )
  }

  "empty `%i` symbol array literals" should "be recognized as such" in {
    val eg = Seq("%i()", "%i[]", "%i{}", "%i<>", "%i##", "%i!!", "%i--", "%i@@", "%i++", "%i**", "%i//", "%i&&")
    all(eg.map(tokenize)) shouldBe Seq(
      QUOTED_NON_EXPANDED_SYMBOL_ARRAY_LITERAL_START,
      QUOTED_NON_EXPANDED_SYMBOL_ARRAY_LITERAL_END,
      EOF
    )
  }

  "single-character `%i` symbol array literals" should "be recognized as such" in {
    val eg =
      Seq("%i(x)", "%i[y]", "%i{z}", "%i<w>", "%i#a#", "%i!b!", "%i-_-", "%i@c@", "%i+d+", "%i*e*", "%i/#/", "%i&!&")
    all(eg.map(tokenize)) shouldBe Seq(
      QUOTED_NON_EXPANDED_SYMBOL_ARRAY_LITERAL_START,
      QUOTED_NON_EXPANDED_SYMBOL_ARRAY_CHARACTER,
      QUOTED_NON_EXPANDED_SYMBOL_ARRAY_LITERAL_END,
      EOF
    )
  }

  "two-word `%i` symbol array literals" should "be recognized as such" in {
    val eg = Seq(
      "%i(xx y)",
      "%i[yy z]",
      "%i{z0 w}",
      "%i<w; 1>",
      "%i#a& ?#",
      "%i!b_ c!",
      "%i-_= +-",
      "%i@c\" d@",
      "%i+d/ *+",
      "%i*ef <*",
      "%i/#< >/",
      "%i&!! %&"
    )
    all(eg.map(tokenize)) shouldBe Seq(
      QUOTED_NON_EXPANDED_SYMBOL_ARRAY_LITERAL_START,
      QUOTED_NON_EXPANDED_SYMBOL_ARRAY_CHARACTER,
      QUOTED_NON_EXPANDED_SYMBOL_ARRAY_CHARACTER,
      QUOTED_NON_EXPANDED_SYMBOL_ARRAY_SEPARATOR,
      QUOTED_NON_EXPANDED_SYMBOL_ARRAY_CHARACTER,
      QUOTED_NON_EXPANDED_SYMBOL_ARRAY_LITERAL_END,
      EOF
    )
  }

  "identifier used in a keyword argument" should "not be mistaken for a symbol literal" in {
    // This test exists to check if RubyLexer properly decided between COLON and SYMBOL_LITERAL
    val code = "foo(x:y)"
    tokenize(code) shouldBe Seq(
      LOCAL_VARIABLE_IDENTIFIER,
      LPAREN,
      LOCAL_VARIABLE_IDENTIFIER,
      COLON,
      LOCAL_VARIABLE_IDENTIFIER,
      RPAREN,
      EOF
    )
  }
}
