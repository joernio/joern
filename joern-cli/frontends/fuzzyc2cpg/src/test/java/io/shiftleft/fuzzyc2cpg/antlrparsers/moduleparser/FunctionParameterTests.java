package io.shiftleft.fuzzyc2cpg.antlrparsers.moduleparser;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import io.shiftleft.fuzzyc2cpg.ModuleParser;

import org.antlr.v4.runtime.tree.ParseTree;
import org.junit.Test;


public class FunctionParameterTests extends ModuleParserTest {

	@Test
	public void testFunctionPtrParam()
	{
		String input = "int foo(char *(*param)(void)){}";

		ModuleParser parser = createParser(input);
		String output = parser.function_def().toStringTree(parser);

		assertEquals("(function_def (return_type (type_name (base_type int))) (function_name (identifier foo)) (function_param_list ( (parameter_decl_clause (parameter_decl (param_decl_specifiers (type_name (base_type char))) (parameter_id (parameter_ptrs (ptrs (ptr_operator *))) ( (parameter_id (parameter_ptrs (ptrs (ptr_operator *))) (parameter_name (identifier param))) ) (type_suffix (param_type_list ( void )))))) )) (compound_statement { }))",
					 output);
	}

	@Test
	public void testVoidParamList()
	{
		String input = "static int altgid(void){}";

		ModuleParser parser = createParser(input);
		String output = parser.function_def().toStringTree(parser);
		assertTrue(output.startsWith("(function_def "));
	}

	@Test
	public void testParamVoidPtr()
	{
		String input = "static int altgid(void *ptr){}";

		ModuleParser parser = createParser(input);
		String output = parser.function_def().toStringTree(parser);
		assertTrue(output.startsWith("(function_def"));
	}

	@Test
	public void testLinux__user()
	{
		String input = "static long aio_read_events_ring(struct kioctx *ctx, struct io_event __user *event, long nr){}";

		ModuleParser parser = createParser(input);
		String output = parser.function_def().toStringTree(parser);
		assertTrue(output.startsWith("(function_def"));
	}

	@Test
	public void testParamConstVoidPtr()
	{
		String input = "static ssize_t _7z_write_data(struct archive_write *a, const void *buff, size_t s){}";

		ModuleParser parser = createParser(input);
		String output = parser.function_def().toStringTree(parser);
		assertTrue(output.startsWith("(function_def"));
	}

	@Test
	public void testConstConstPtr()
	{
		String input = "static void black_box(const example_s * const * alias_to_alias) {}";

		ModuleParser parser = createParser(input);
		String output = parser.function_def().toStringTree(parser);
		assertTrue(output.startsWith("(function_def"));
	}

	@Test
	public void testMoveParameters() {
		String input = "void foo(std::string&& s) {}";
		ModuleParser parser = createParser(input);
		String output = parser.function_def().toStringTree(parser);
		assertEquals("(function_def (return_type (type_name (base_type void))) (function_name (identifier foo)) (function_param_list ( (parameter_decl_clause (parameter_decl (param_decl_specifiers (type_name (base_type std) :: (base_type string))) (parameter_id (parameter_ptrs (rvalue_ref &&)) (parameter_name (identifier s))))) )) (compound_statement { }))",
					 output);
	}
}
