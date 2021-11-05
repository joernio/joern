package io.shiftleft.fuzzyc2cpg.antlrparsers.functionparser;

import static org.junit.Assert.assertTrue;

import io.shiftleft.fuzzyc2cpg.passes.astcreation.AntlrParserDriver;
import org.antlr.v4.runtime.tree.ParseTree;
import org.junit.Test;

public class ForLoopTests extends FunctionParserTestBase
{

	@Test
	public void testEmptyFor()
	{
		String input = "for(; ;){}";
		AntlrParserDriver functionParser = createFunctionDriver();
		ParseTree tree = functionParser.parseString(input);
		String output = tree.toStringTree(functionParser.getAntlrParser());
		assertTrue(output.contains("selection_or_iteration"));
	}

	@Test
	public void testDeclInFor()
	{
		String input = "for(int k = 0; k < 10; k++ ){}";
		AntlrParserDriver functionParser = createFunctionDriver();
		ParseTree tree = functionParser.parseString(input);
		String output = tree.toStringTree(functionParser.getAntlrParser());
		assertTrue(output.contains(
				"for ( (for_init_statement (simple_decl (var_decl (type_name (base_type int))"));
	}

	@Test
	public void testComplexFor()
	{
		String input = "for(int k = 0; k < 10; ( k += ((c = text[k]) >= sBMHCharSetSize) ? patlen : skip[c]) ){}";
		AntlrParserDriver functionParser = createFunctionDriver();
		ParseTree tree = functionParser.parseString(input);
		String output = tree.toStringTree(functionParser.getAntlrParser());
		assertTrue(output.contains("assign_expr"));
	}
}
