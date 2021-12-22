package io.joern.fuzzyc2cpg.parsetreetoast;

import io.joern.fuzzyc2cpg.FunctionLexer;
import io.joern.fuzzyc2cpg.ast.AstNode;
import io.joern.fuzzyc2cpg.parser.TokenSubStream;
import io.joern.fuzzyc2cpg.parser.functions.AntlrCFunctionParserDriver;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.tree.ParseTree;

public class FunctionContentTestUtil {

    public static AstNode parseAndWalk(String input) {
        AntlrCFunctionParserDriver driver = new AntlrCFunctionParserDriver();

        TokenSubStream tokens = tokenStreamFromString(input);
        driver.parseAndWalkTokenStream(tokens);
        return driver.builderStack.peek().getItem();
    }

    static ParseTree parse(String input) {
        AntlrCFunctionParserDriver driver = new AntlrCFunctionParserDriver();
        return driver.parseString(input);
    }

    private static TokenSubStream tokenStreamFromString(String input) {
        CharStream inputStream = CharStreams.fromString(input);
        FunctionLexer lex = new FunctionLexer(inputStream);
        return new TokenSubStream(lex);
    }

}
