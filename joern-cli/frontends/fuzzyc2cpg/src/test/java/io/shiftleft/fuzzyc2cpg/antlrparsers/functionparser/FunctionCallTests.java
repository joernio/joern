package io.shiftleft.fuzzyc2cpg.antlrparsers.functionparser;

import static org.junit.Assert.assertTrue;

import io.shiftleft.fuzzyc2cpg.passes.astcreation.AntlrParserDriver;
import org.antlr.v4.runtime.tree.ParseTree;
import org.junit.Test;

public class FunctionCallTests extends FunctionParserTestBase
{

	@Test
	public void testFunctionCall()
	{
		String input = "foo(x);";
		AntlrParserDriver functionParser = createFunctionDriver();
		ParseTree tree = functionParser.parseString(input);
		String output = tree.toStringTree(functionParser.getAntlrParser());
		assertTrue(output.contains("function_argument_list"));
	}

	@Test
	public void testTwoParameters()
	{
		String input = "foo(x,y);";
		AntlrParserDriver functionParser = createFunctionDriver();
		ParseTree tree = functionParser.parseString(input);
		String output = tree.toStringTree(functionParser.getAntlrParser());
		assertTrue(output.contains(", (function_argument"));
	}

	@Test
	public void testCallViaPtr()
	{
		String input = "ptr->foo(x);";
		AntlrParserDriver functionParser = createFunctionDriver();
		ParseTree tree = functionParser.parseString(input);
		String output = tree.toStringTree(functionParser.getAntlrParser());
		assertTrue(output.contains("function_argument_list"));
	}

	@Test
	public void testCallWithExprInArg()
	{
		String input = "foo(x == 1, x++);";
		AntlrParserDriver functionParser = createFunctionDriver();
		ParseTree tree = functionParser.parseString(input);
		String output = tree.toStringTree(functionParser.getAntlrParser());
		assertTrue(output.contains("function_argument_list"));
	}
}
