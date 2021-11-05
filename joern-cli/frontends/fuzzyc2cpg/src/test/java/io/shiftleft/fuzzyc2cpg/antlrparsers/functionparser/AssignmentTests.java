package io.shiftleft.fuzzyc2cpg.antlrparsers.functionparser;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import io.shiftleft.fuzzyc2cpg.passes.astcreation.AntlrParserDriver;
import org.antlr.v4.runtime.tree.ParseTree;
import org.junit.Test;

public class AssignmentTests extends FunctionParserTestBase {

	@Test
	public void testAssignmentExpr() {
		String input = "x = y + 1;";
		AntlrParserDriver functionParser = createFunctionDriver();
		ParseTree tree = functionParser.parseString(input);
		String output = tree.toStringTree(functionParser.getAntlrParser());
		assertTrue(output.contains("assign_expr"));
	}

	@Test
	public void testComplexAssignment() {
		String input = "k += ((c = text[k]) >= sBMHCharSetSize) ? patlen : skip[c];";
		AntlrParserDriver functionParser = createFunctionDriver();
		ParseTree tree = functionParser.parseString(input);
		String output = tree.toStringTree(functionParser.getAntlrParser());
		assertTrue(output.contains("assign_expr"));
	}

	@Test
	public void testPrivateInName() {
		String input = "struct acpi_battery *battery = m->private;";
		AntlrParserDriver functionParser = createFunctionDriver();
		ParseTree tree = functionParser.parseString(input);
		String output = tree.toStringTree(functionParser.getAntlrParser());
		assertTrue(output.contains("simple_decl"));
	}
}
