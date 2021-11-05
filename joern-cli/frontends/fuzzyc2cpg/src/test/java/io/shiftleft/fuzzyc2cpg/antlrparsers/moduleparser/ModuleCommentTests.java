package io.shiftleft.fuzzyc2cpg.antlrparsers.moduleparser;

import static org.junit.Assert.assertEquals;

import org.antlr.v4.runtime.Token;
import org.junit.Test;

import io.shiftleft.fuzzyc2cpg.ModuleParser;

public class ModuleCommentTests extends ModuleParserTest {

  private void assertTokenEqualsString(Token tok, String expected) {
    assertEquals(expected, tok.getText());
  }

  @Test
  public void shouldParseSingleLineComments() {
    String input = "// This is a comment!";
    ModuleParser parser = createHiddenParser(input);
    assertTokenEqualsString(parser.getCurrentToken(), input);
  }

  @Test
  public void shouldParseSingleLineCommentInFunction() {
    String input = "static int altgid(void){\n" +
      "// This is a comment!\n" +
      "return 1;\n" +
      "}";
    ModuleParser parser = createHiddenParser(input);
    assertTokenEqualsString(parser.getCurrentToken(), "// This is a comment!\n");
  }

  @Test
  public void shouldParseSingleLineCommentWithEmojiInFunction() {
    String input = "static int altgid(void){\n" +
            "// This is the peach emoji: \uD83C\uDF51\n" +
            "return 1;\n" +
            "}";
    ModuleParser parser = createHiddenParser(input);
    assertTokenEqualsString(parser.getCurrentToken(), "// This is the peach emoji: \uD83C\uDF51\n");
  }


  @Test
  public void shouldParseSingleLineCommentInStruct() {
    String input = "struct foo {\n" +
      "int x;\n" +
      "int y; // This is a comment!\n" +
      "};";
    ModuleParser parser = createHiddenParser(input);
    assertTokenEqualsString(parser.getCurrentToken(), "// This is a comment!\n");
  }

  @Test
  public void shouldParseSingleLineCommentInline() {
    String input = "static int altgid(void){} // This is a comment!";
    ModuleParser parser = createHiddenParser(input);
    assertTokenEqualsString(parser.getCurrentToken(), "// This is a comment!");
  }

  @Test
  public void shouldParseBlockComments() {
    String input = "/* This is a block comment! */";
    ModuleParser parser = createHiddenParser(input);
    assertTokenEqualsString(parser.getCurrentToken(), "/* This is a block comment! */");
  }

  @Test
  public void shouldParseNestedBlockComments() {
    String input = "/* This is a /* nested */ block comment! */";
    ModuleParser parser = createHiddenParser(input);
    assertTokenEqualsString(parser.getCurrentToken(), "/* This is a /* nested */ block comment! */");
  }

  @Test
  public void shouldParseBlockCommentsInFunction() {
    String input = "static int altgid(void){\n" +
      "/* This is a block comment! */\n" +
      "return 1;\n" +
      "}";
    ModuleParser parser = createHiddenParser(input);
    assertTokenEqualsString(parser.getCurrentToken(), "/* This is a block comment! */");
  }

  @Test
  public void shouldParseBlockCommentsInFunctionDefinition() {
    String input = "static /* A weird comment! */ int altgid(void){\n" +
      "return 1;" +
      "}";
    ModuleParser parser = createHiddenParser(input);
    assertTokenEqualsString(parser.getCurrentToken(), "/* A weird comment! */");
  }

  @Test
  public void shouldParseBlockCommentsInStruct() {
    String input = "struct foo {\n" +
      "int x\n;" +
      "int y; /* This is a block comment! */\n" +
      "};";
    ModuleParser parser = createHiddenParser(input);
    assertTokenEqualsString(parser.getCurrentToken(), "/* This is a block comment! */");
  }

  @Test
  public void shouldParseBlockCommentsInStructDefinition() {
    String input = "struct /* A weird comment! */ foo {\n" +
      "int x;\n" +
      "int y;\n" +
      "};";
    ModuleParser parser = createHiddenParser(input);
    assertTokenEqualsString(parser.getCurrentToken(), "/* A weird comment! */");
  }

  @Test
  public void shouldParseMultilineBlockComments() {
    String input = "/*\n" +
      " This is a block comment!\n" +
      "*/\n";
    ModuleParser parser = createHiddenParser(input);
    assertTokenEqualsString(parser.getCurrentToken(), input.substring(0, input.length() - 1));
  }

  @Test
  public void shouldParseMultilineBlockCommentsInFunction() {
    String input = "static int altgid(void){\n" +
      "/* This is\n" +
      "a block comment!\n" +
      "*/\n" +
      "return 1;\n" +
      "}";
    ModuleParser parser = createHiddenParser(input);
    assertTokenEqualsString(
      parser.getCurrentToken(),
      "/* This is\n" +
        "a block comment!\n" +
        "*/");
  }

  @Test
  public void shouldParseMultilineBlockCommentsInFunctionDefinition() {
    String input = "static int \n" +
      "/* This is\n" +
      "a block comment!\n" +
      "*/\n" +
      "altgid(void){\n" +
      "return 1;\n" +
      "}";
    ModuleParser parser = createHiddenParser(input);
    assertTokenEqualsString(
      parser.getCurrentToken(),
      "/* This is\n" +
      "a block comment!\n" +
      "*/");
  }

  @Test
  public void shouldParseMultilineBlockCommentsInStruct() {
    String input = "struct foo {\n" +
      "int x;\n" +
      "/* This is\n" +
      "a block comment!\n" +
      "*/\n" +
      "int y;\n" +
      "};";
    ModuleParser parser = createHiddenParser(input);
    assertTokenEqualsString(
      parser.getCurrentToken(),
      "/* This is\n" +
      "a block comment!\n" +
      "*/");
  }

  @Test
  public void shouldParseMultilineBlockCommentsInStructDefinition() {
    String input = "struct" + "/* This is\n" +
      "a block comment!\n" +
      "*/\n" +
      "foo {\n" +
      "int x;\n" +
      "int y;\n" +
      "};";
    ModuleParser parser = createHiddenParser(input);
    assertTokenEqualsString(
      parser.getCurrentToken(),
      "/* This is\n" +
        "a block comment!\n" +
        "*/");
  }
}
