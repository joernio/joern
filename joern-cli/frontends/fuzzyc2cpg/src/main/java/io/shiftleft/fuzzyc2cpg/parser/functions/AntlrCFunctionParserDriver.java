package io.shiftleft.fuzzyc2cpg.parser.functions;

import io.shiftleft.fuzzyc2cpg.FunctionLexer;
import io.shiftleft.fuzzyc2cpg.FunctionParser;
import io.shiftleft.fuzzyc2cpg.parser.TokenSubStream;
import io.shiftleft.fuzzyc2cpg.passes.astcreation.AntlrParserDriver;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.tree.ParseTree;

public class AntlrCFunctionParserDriver extends AntlrParserDriver {

  public AntlrCFunctionParserDriver() {
    super();
    setListener(new CFunctionParseTreeListener(this));
  }

  @Override
  public ParseTree parseTokenStreamImpl(TokenSubStream tokens) {
    FunctionParser parser = new FunctionParser(tokens);
    setAntlrParser(parser);
    ParseTree tree = null;

    try {
      setSLLMode(parser);
      tree = parser.statements();
    } catch (RuntimeException ex) {
      if (isRecognitionException(ex)) {
        tokens.reset();
        setLLStarMode(parser);
        tree = parser.statements();
      }

    }
    return tree;
  }

  @Override
  public Lexer createLexer(CharStream input) {
    return new FunctionLexer(input);
  }
}
