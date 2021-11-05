package io.shiftleft.fuzzyc2cpg.antlrparsers.functionparser;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import io.shiftleft.fuzzyc2cpg.passes.astcreation.AntlrParserDriver;
import org.antlr.v4.runtime.tree.ParseTree;
import org.junit.Test;

public class FunctionParserTest extends FunctionParserTestBase
{

	private String generateParserOutput(String input) {
		AntlrParserDriver functionParser = createFunctionDriver();
		ParseTree tree = functionParser.parseString(input);
		return tree.toStringTree(functionParser.getAntlrParser());
	}

	@Test
	public void testIf()
	{
		String input = "if(foo){}";
		String output = generateParserOutput(input);
		assertTrue(output.contains("(selection_or_iteration if"));
	}

	@Test
	public void testStructInFunc()
	{
		String input = "class foo{ int x; };";
    	String output = generateParserOutput(input);;
		assertTrue(output.contains("class_def"));
	}

	@Test
	public void testSizeofStruct()
	{
		String input = "while((buffer + len) > (tmp + sizeof(struct stun_attrib))) {}";
    	String output = generateParserOutput(input);
		assertTrue(output.contains("selection_or_iteration while"));
	}

	@Test
	public void testAutoWithinIf() {
		String input = "if (auto x = 1) { return 1; } else { return 2; }";
    	String output = generateParserOutput(input);
		assertTrue(output.contains("(base_type auto)) (declarator (identifier x))"));
	}

	@Test
	public void testMultilineString() {
		String input = "char* c = \"This is \"\n" +
			"\"a multiline \"\n" +
			"\"string.\";";

    	String output = generateParserOutput(input);
		assertEquals(
			"(statements (statement (simple_decl (var_decl (type_name (base_type char)) (init_declarator_list (init_declarator (declarator (ptrs (ptr_operator *)) (identifier c)) = (initializer (assign_expr (conditional_expression (or_expression (and_expression (inclusive_or_expression (exclusive_or_expression (bit_and_expression (equality_expression (relational_expression (shift_expression (additive_expression (multiplicative_expression (cast_expression (unary_expression (postfix_expression (primary_expression (constant \"This is \"\\n\"a multiline \"\\n\"string.\"))))))))))))))))))) ;)))))",
			output);
	}

	@Test
    public void testStringConcatWithIdentifier() {
	    String input = "char* c = \"start\"SOME_VAR\"end\";";
		String output = generateParserOutput(input);

		assertEquals("(statements (statement (simple_decl (var_decl (type_name (base_type char)) (init_declarator_list (init_declarator (declarator (ptrs (ptr_operator *)) (identifier c)) = (initializer (assign_expr (conditional_expression (or_expression (and_expression (inclusive_or_expression (exclusive_or_expression (bit_and_expression (equality_expression (relational_expression (shift_expression (additive_expression (multiplicative_expression (cast_expression (unary_expression (postfix_expression (primary_expression (constant \"start\"SOME_VAR\"end\"))))))))))))))))))) ;)))))",
			output);
    }

	@Test
	public void testAssignmentWithEmojiComment() {
		String input = "// This is the peach emoji: \uD83C\uDF51\n" +
					   "const auto derefed = *ref;\n";
		String output = generateParserOutput(input);
		assertEquals("(statements (statement (simple_decl (var_decl (type_name const (base_type auto)) (init_declarator_list (init_declarator (declarator (identifier derefed)) = (initializer (assign_expr (conditional_expression (or_expression (and_expression (inclusive_or_expression (exclusive_or_expression (bit_and_expression (equality_expression (relational_expression (shift_expression (additive_expression (multiplicative_expression (cast_expression (unary_expression (unary_op_and_cast_expr (unary_operator *) (cast_expression (unary_expression (postfix_expression (primary_expression (identifier ref)))))))))))))))))))))) ;)))))",
					 output);
	}

	@Test
	public void testNew() {
		String input = "int x = new uint8_t[42];";
		String output = generateParserOutput(input);
		assertEquals("(statements (statement (simple_decl (var_decl (type_name (base_type int)) (init_declarator_list (init_declarator (declarator (identifier x)) = (initializer (assign_expr (conditional_expression (or_expression (and_expression (inclusive_or_expression (exclusive_or_expression (bit_and_expression (equality_expression (relational_expression (shift_expression (additive_expression (multiplicative_expression (cast_expression (unary_expression (new_expression new (type_name (base_type uint8_t)) [ (conditional_expression (or_expression (and_expression (inclusive_or_expression (exclusive_or_expression (bit_and_expression (equality_expression (relational_expression (shift_expression (additive_expression (multiplicative_expression (cast_expression (unary_expression (postfix_expression (primary_expression (constant 42)))))))))))))))) ]))))))))))))))))) ;)))))", output);
	}

	@Test
	public void testDelete() {
		String input = "delete n;";
		String output = generateParserOutput(input);
		assertEquals("(statements (statement (expr_statement (expr (assign_expr (conditional_expression (or_expression (and_expression (inclusive_or_expression (exclusive_or_expression (bit_and_expression (equality_expression (relational_expression (shift_expression (additive_expression (multiplicative_expression (cast_expression (unary_expression (delete_expression delete (identifier n))))))))))))))))) ;)))",
					 output);
	}

	@Test
	public void testArrayDelete() {
		String input = "delete[] n;";
		String output = generateParserOutput(input);
		assertEquals("(statements (statement (expr_statement (expr (assign_expr (conditional_expression (or_expression (and_expression (inclusive_or_expression (exclusive_or_expression (bit_and_expression (equality_expression (relational_expression (shift_expression (additive_expression (multiplicative_expression (cast_expression (unary_expression (delete_expression delete [ ] (identifier n))))))))))))))))) ;)))",
					 output);
	}

	@Test
	public void testCPPConstCast() {
		String input = "int x = const_cast<int>(n);";
		String output = generateParserOutput(input);
		assertEquals("(statements (statement (simple_decl (var_decl (type_name (base_type int)) (init_declarator_list (init_declarator (declarator (identifier x)) = (initializer (assign_expr (conditional_expression (or_expression (and_expression (inclusive_or_expression (exclusive_or_expression (bit_and_expression (equality_expression (relational_expression (shift_expression (additive_expression (multiplicative_expression (cast_expression (cpp_cast_identifier const_cast) < (cast_target (type_name (base_type int))) > ( (cast_expression (unary_expression (postfix_expression (primary_expression (identifier n))))) )))))))))))))))) ;)))))",
				output);
	}

	@Test
	public void testCPPStaticCast() {
		String input = "int x = static_cast<int>(n);";
		String output = generateParserOutput(input);
		assertEquals("(statements (statement (simple_decl (var_decl (type_name (base_type int)) (init_declarator_list (init_declarator (declarator (identifier x)) = (initializer (assign_expr (conditional_expression (or_expression (and_expression (inclusive_or_expression (exclusive_or_expression (bit_and_expression (equality_expression (relational_expression (shift_expression (additive_expression (multiplicative_expression (cast_expression (cpp_cast_identifier static_cast) < (cast_target (type_name (base_type int))) > ( (cast_expression (unary_expression (postfix_expression (primary_expression (identifier n))))) )))))))))))))))) ;)))))",
				output);
	}

	@Test
	public void testCPPDynamicCast() {
		String input = "int x = dynamic_cast<int>(n);";
		String output = generateParserOutput(input);
		assertEquals("(statements (statement (simple_decl (var_decl (type_name (base_type int)) (init_declarator_list (init_declarator (declarator (identifier x)) = (initializer (assign_expr (conditional_expression (or_expression (and_expression (inclusive_or_expression (exclusive_or_expression (bit_and_expression (equality_expression (relational_expression (shift_expression (additive_expression (multiplicative_expression (cast_expression (cpp_cast_identifier dynamic_cast) < (cast_target (type_name (base_type int))) > ( (cast_expression (unary_expression (postfix_expression (primary_expression (identifier n))))) )))))))))))))))) ;)))))",
				output);
	}

	@Test
	public void testCPPReinterpretCast() {
		String input = "int x = reinterpret_cast<int>(n);";
		String output = generateParserOutput(input);
		assertEquals("(statements (statement (simple_decl (var_decl (type_name (base_type int)) (init_declarator_list (init_declarator (declarator (identifier x)) = (initializer (assign_expr (conditional_expression (or_expression (and_expression (inclusive_or_expression (exclusive_or_expression (bit_and_expression (equality_expression (relational_expression (shift_expression (additive_expression (multiplicative_expression (cast_expression (cpp_cast_identifier reinterpret_cast) < (cast_target (type_name (base_type int))) > ( (cast_expression (unary_expression (postfix_expression (primary_expression (identifier n))))) )))))))))))))))) ;)))))",
				output);
	}
}
