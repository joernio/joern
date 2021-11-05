package io.shiftleft.fuzzyc2cpg.antlrparsers.moduleparser;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import io.shiftleft.fuzzyc2cpg.ModuleParser;

import org.junit.Ignore;
import org.junit.Test;

public class OtherTests extends ModuleParserTest
{

	@Test
	public void testNestedFunctionName()
	{
		String input = "int (foo)(){}";

		ModuleParser parser = createParser(input);
		String output = parser.function_def().toStringTree(parser);
		assertTrue(output.startsWith("(function_def "));
	}

	@Test
	public void testOperatorOverloading()
	{
		String input = "inline bool operator == (const PlMessageHeader &b) const {}";

		ModuleParser parser = createParser(input);
		String output = parser.function_def().toStringTree(parser);

		assertTrue(output.startsWith("(function_def "));
	}

	@Test
	public void testExceptionSpecificationCpp()
	{
		String input = "int foo() throw(){}";

		ModuleParser parser = createParser(input);
		String output = parser.function_def().toStringTree(parser);

		assertTrue(output.startsWith("(function_def "));
	}

	@Test
	public void testMultilineString() {
		String input = "char* c = \"This is \"\n" +
			"\"a multiline \"\n" +
			"\"string.\";";

		ModuleParser parser = createParser(input);
		String output = parser.var_decl().toStringTree(parser);
		assertEquals(
			"(var_decl (type_name (base_type char)) (init_declarator_list (init_declarator (declarator (ptrs (ptr_operator *)) (identifier c)) = (assign_expr_w_ (assign_water \"This is \"\\n\"a multiline \"\\n\"string.\"))) ;))",
			output);
	}

	@Test
	public void testStringConcatWithIdentifier() {
		String input = "char* c = \"start\"SOME_VAR\"end\";";
		ModuleParser parser = createParser(input);
		String output = parser.var_decl().toStringTree(parser);
		assertEquals(
			"(var_decl (type_name (base_type char)) (init_declarator_list (init_declarator (declarator (ptrs (ptr_operator *)) (identifier c)) = (assign_expr_w_ (assign_water \"start\"SOME_VAR\"end\"))) ;))",
			output);
	}

	@Test
	public void testMultipleDeclarations() {
		String input = "int x, y;";
		ModuleParser parser = createParser(input);
		String output = parser.simple_decl().toStringTree(parser);

		assertEquals("(simple_decl (var_decl (type_name (base_type int)) (init_declarator_list (init_declarator (declarator (identifier x))) , (init_declarator (declarator (identifier y))) ;)))",
				     output);
	}

	// See https://github.com/ShiftLeftSecurity/fuzzyc2cpg/issues/158
	@Test
	public void testEmojiFuncComment() {
		String input = "int meaning_of_life() {\n" +
					   "  // This is the peach emoji: \uD83C\uDF51\n" +
					   "  const auto x = *y;\n" +
				 	   "  return 42;\n" +
				       "}";

		ModuleParser parser = createParser(input);
		String output = parser.function_def().toStringTree(parser);

		assertEquals("(function_def (return_type (type_name (base_type int))) (function_name (identifier meaning_of_life)) (function_param_list ( )) (compound_statement { const auto x = * y ; return 42 ; }))",
				output);
	}
}
