package io.shiftleft.fuzzyc2cpg.antlrparsers.functionparser;


import io.shiftleft.fuzzyc2cpg.passes.astcreation.AntlrParserDriver;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.Token;

import io.shiftleft.fuzzyc2cpg.FunctionLexer;
import io.shiftleft.fuzzyc2cpg.FunctionParser;
import io.shiftleft.fuzzyc2cpg.parser.functions.AntlrCFunctionParserDriver;

public class FunctionParserTestBase {
	protected AntlrParserDriver createFunctionDriver() {
		return new AntlrCFunctionParserDriver();
	}

	protected FunctionParser createHiddenParser(String input) {
		CharStream inputStream = CharStreams.fromString(input);
		FunctionLexer lexer = new FunctionLexer(inputStream);
		CommonTokenStream cts = new CommonTokenStream(lexer, Token.HIDDEN_CHANNEL);
		return new FunctionParser(cts);
	}
}
