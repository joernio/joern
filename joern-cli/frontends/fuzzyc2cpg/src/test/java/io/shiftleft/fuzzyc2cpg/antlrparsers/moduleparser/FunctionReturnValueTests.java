package io.shiftleft.fuzzyc2cpg.antlrparsers.moduleparser;

import static org.junit.Assert.assertTrue;

import io.shiftleft.fuzzyc2cpg.ModuleParser;
import org.junit.Test;


public class FunctionReturnValueTests extends ModuleParserTest
{

	@Test
	public void testFunction_defNoReturnValue()
	{
		String input = "main(){foo}";
		String expected = "(function_def (function_name (identifier main)) (function_param_list ( )) (compound_statement {";

		ModuleParser parser = createParser(input);
		String output = parser.function_def().toStringTree(parser);

		assertTrue(output.startsWith(expected));
	}

	@Test
	public void testFunction_defReturnValue()
	{
		String input = "int main(){}";
		String expected = "(function_def (return_type (type_name (base_type int))) (function_name (identifier main)) (function_param_list ( )) (compound_statement { }))";

		ModuleParser parser = createParser(input);
		String output = parser.function_def().toStringTree(parser);
		assertTrue(output.equals(expected));
	}

	@Test
	public void testFunction_defPtrReturnValue()
	{
		String input = "int *foo(){}";
		String expected = "(function_def (return_type (type_name (base_type int)) (ptr_operator *)) (function_name (identifier foo)) (function_param_list ( )) (compound_statement { }))";

		ModuleParser parser = createParser(input);
		String output = parser.function_def().toStringTree(parser);
		assertTrue(output.equals(expected));
	}

	@Test
	public void testFunction_defStaticUnsigned()
	{
		String input = "static unsigned my_atoi(const char *p){}";

		ModuleParser parser = createParser(input);
		String output = parser.function_def().toStringTree(parser);
		assertTrue(output.startsWith(
				"(function_def (return_type (function_decl_specifiers static) (type_name unsigned)) (function_name (identifier my_atoi))"));
	}

}
