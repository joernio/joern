package io.shiftleft.fuzzyc2cpg.antlrparsers.moduleparser;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.Token;

import io.shiftleft.fuzzyc2cpg.ModuleLexer;
import io.shiftleft.fuzzyc2cpg.ModuleParser;

public class ModuleParserTest {

	protected ModuleParser createParser(String input) {
		return createParser(input, Token.DEFAULT_CHANNEL);
	}

	protected ModuleParser createHiddenParser(String input) {
		return createParser(input, Token.HIDDEN_CHANNEL);
	}

	protected ModuleParser createParser(String input, int chan) {
		CharStream inputStream = CharStreams.fromString(input);
		ModuleLexer lex = new ModuleLexer(inputStream);
		CommonTokenStream tokens = new CommonTokenStream(lex, chan);
		ModuleParser parser = new ModuleParser(tokens);
		return parser;
	}
}
