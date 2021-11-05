package io.shiftleft.fuzzyc2cpg.antlrparsers.moduleparser;

import static org.junit.Assert.assertTrue;

import io.shiftleft.fuzzyc2cpg.ModuleLexer;
import io.shiftleft.fuzzyc2cpg.ModuleParser;
import io.shiftleft.fuzzyc2cpg.ModuleParser.Class_defContext;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.junit.Test;

public class ClassDeclarationTest
{

	private ModuleParser createParser(String input)
	{
		CharStream inputStream = CharStreams.fromString(input);
		ModuleLexer lex = new ModuleLexer(inputStream);
		CommonTokenStream tokens = new CommonTokenStream(lex);
		ModuleParser parser = new ModuleParser(tokens);
		return parser;
	}

	@Test
	public void testSimpleStructDef()
	{
		String input = "struct foo{int x;};";

		ModuleParser parser = createParser(input);
		String output = parser.simple_decl().toStringTree(parser);
		assertTrue(output.startsWith(
				"(simple_decl (var_decl (class_def (class_key struct) (class_name (identifier foo))"));
	}

	@Test
	public void testAnonymousStructDef()
	{
		String input = "struct {int x;}v;";

		ModuleParser parser = createParser(input);
		String output = parser.simple_decl().toStringTree(parser);
		assertTrue(output
				.startsWith("(simple_decl (var_decl (class_def (class_key struct) {"));
	}

	@Test
	public void testStructureInitArray()
	{
		String input = "struct archive_contents"
				+ "{ const char *f; struct contents *c; } files[] "
				+ "= {{\"sparse\",archive_contents_sparse }, {\"sparse2\", archive_contents_sparse2} };";

		ModuleParser parser = createParser(input);
		String output = parser.simple_decl().toStringTree(parser);

		assertTrue(output.contains("assign_expr"));
	}

	@Test
	public void testStructureInitSimple()
	{
		String input = "struct foo{ int x; } y;";
		ModuleParser parser = createParser(input);
		String output = parser.simple_decl().toStringTree(parser);
		assertTrue(output.startsWith(
				"(simple_decl (var_decl (class_def (class_key struct) (class_name (identifier foo)) { int x ; }) (init_declarator_list (init_declarator (declarator (identifier y))) ;)))"));
	}

	@Test
	public void testFunctionPrototype()
	{
		String input = "int foo(int x);";
		ModuleParser parser = createParser(input);
		String output = parser.simple_decl().toStringTree(parser);
		assertTrue(output.startsWith(
				"(simple_decl (var_decl (type_name (base_type int)) (init_declarator_list (init_declarator (declarator (identifier foo) (type_suffix (param_type_list ( (param_type"));
	}

	@Test
	public void testClassContentExtraction()
	{
		String input = "class foo{ foobar; }";

		ModuleParser parser = createParser(input);
		Class_defContext class_def = parser.class_def();

		int startIndex = class_def.OPENING_CURLY().getSymbol().getTokenIndex();
		int stopIndex = class_def.stop.getTokenIndex();
		assertTrue((startIndex == 2) && (stopIndex == 5));
	}

}
