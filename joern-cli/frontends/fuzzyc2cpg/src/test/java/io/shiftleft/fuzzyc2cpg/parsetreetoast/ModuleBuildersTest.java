package io.shiftleft.fuzzyc2cpg.parsetreetoast;

import io.shiftleft.fuzzyc2cpg.passes.astcreation.AntlrCModuleParserDriver;
import java.util.List;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.junit.Test;

import io.shiftleft.fuzzyc2cpg.ModuleLexer;
import io.shiftleft.fuzzyc2cpg.ast.AstNode;
import io.shiftleft.fuzzyc2cpg.ast.declarations.ClassDefStatement;
import io.shiftleft.fuzzyc2cpg.ast.declarations.IdentifierDecl;
import io.shiftleft.fuzzyc2cpg.ast.functionDef.*;
import io.shiftleft.fuzzyc2cpg.ast.langc.functiondef.FunctionDef;
import io.shiftleft.fuzzyc2cpg.ast.langc.functiondef.Parameter;
import io.shiftleft.fuzzyc2cpg.ast.langc.functiondef.ParameterType;
import io.shiftleft.fuzzyc2cpg.ast.statements.IdentifierDeclStatement;
import io.shiftleft.fuzzyc2cpg.parser.TokenSubStream;

import static org.junit.Assert.*;

public class ModuleBuildersTest
{

	@Test
	public void testNestedStructs()
	{
		String input = "struct x{ struct y { struct z{}; }; }; abc";
		List<AstNode> codeItems = parseInput(input);
		ClassDefStatement classDef = (ClassDefStatement) codeItems.get(0);
		ClassDefStatement yClass = (ClassDefStatement) classDef.content
				.getStatements().get(0);
		ClassDefStatement zClass = (ClassDefStatement) yClass.content
				.getStatements().get(0);

		assertTrue(codeItems.size() == 1);
		assertTrue(yClass.getIdentifier().getEscapedCodeStr().equals("y"));
		assertTrue(zClass.getIdentifier().getEscapedCodeStr().equals("z"));
	}

	@Test
	public void testStructName()
	{
		String input = "struct foo{};";
		List<AstNode> codeItems = parseInput(input);
		ClassDefStatement codeItem = (ClassDefStatement) codeItems.get(0);
		assertTrue(codeItem.identifier.getEscapedCodeStr().equals("foo"));
	}

	@Test
	public void testUnnamedStruct()
	{
		String input = "struct {int x; } a;";
		List<AstNode> codeItems = parseInput(input);
		ClassDefStatement codeItem = (ClassDefStatement) codeItems.get(0);
		assertTrue(codeItem.identifier.getEscapedCodeStr().equals("<unnamed>"));
	}

	@Test
	public void testStructContent()
	{
		String input = "struct foo{};";
		List<AstNode> codeItems = parseInput(input);
		ClassDefStatement codeItem = (ClassDefStatement) codeItems.get(0);
		assertTrue(codeItem.content != null);
	}

	@Test
	public void testStructFunctionPointer()
	{
		String input = "struct foo{ int*** (**bar)(long*); };";
		List<AstNode> codeItems = parseInput(input);
		ClassDefStatement codeItem = (ClassDefStatement) codeItems.get(0);
		IdentifierDeclStatement ptrStatement =
				(IdentifierDeclStatement) codeItem.content.getStatements().get(0);
		IdentifierDecl ptrItem =
				(IdentifierDecl) ptrStatement.getIdentifierDeclList().get(0);

		assertEquals(ptrItem.getType().completeType, "int * * * ( * * ) ( long * )");
	}


	@Test
	public void testFunctionInClass()
	{
		String input = "class foo{ bar(){} };";
		List<AstNode> codeItems = parseInput(input);
		ClassDefStatement codeItem = (ClassDefStatement) codeItems.get(0);
		FunctionDefBase funcItem = (FunctionDefBase) codeItem.content.getStatements()
				.get(0);
		assertTrue(funcItem.getName().equals("bar"));
	}

	@Test
	public void testDecl()
	{
		String input = "int foo;";
		List<AstNode> codeItems = parseInput(input);
		IdentifierDeclStatement codeItem = (IdentifierDeclStatement) codeItems
				.get(0);
		IdentifierDecl decl = (IdentifierDecl) codeItem.getIdentifierDeclList()
				.get(0);
		assertTrue(decl.getName().getEscapedCodeStr().equals("foo"));
	}

	@Test
	public void testDeclListAfterClass()
	{
		String input = "class foo{int x;} y;";
		List<AstNode> codeItems = parseInput(input);
		IdentifierDeclStatement codeItem = (IdentifierDeclStatement) codeItems
				.get(codeItems.size() - 1);
		IdentifierDecl decl = (IdentifierDecl) codeItem.getIdentifierDeclList()
				.get(0);
		assertTrue(decl.getName().getEscapedCodeStr().equals("y"));
	}

	@Test
	public void testClassDefBeforeContent()
	{
		String input = "class foo{int x;}";
		List<AstNode> codeItems = parseInput(input);

		ClassDefStatement classCodeItem = (ClassDefStatement) codeItems.get(0);
		IdentifierDeclStatement identifierCodeItem = (IdentifierDeclStatement) classCodeItem.content
				.getStatements().get(0);
		IdentifierDecl decl = (IdentifierDecl) identifierCodeItem
				.getIdentifierDeclList().get(0);

		assertTrue(classCodeItem.identifier.getEscapedCodeStr().equals("foo"));
		assertTrue(decl.getName().getEscapedCodeStr().equals("x"));
	}

	@Test
	public void testFuncName()
	{
		String input = "void foo(){};";
		List<AstNode> codeItems = parseInput(input);
		FunctionDefBase codeItem = (FunctionDefBase) codeItems.get(0);
		assertTrue(codeItem.getName().equals("foo"));
	}

	@Test
	public void testFuncSignature()
	{
		String input = "void foo(int x, char **ptr){};";
		List<AstNode> codeItems = parseInput(input);
		FunctionDefBase codeItem = (FunctionDefBase) codeItems.get(0);
		assertTrue(codeItem.getEscapedCodeStr()
				.equals("foo (int x,char **ptr)"));
	}

	@Test
	public void testSimpleParamList()
	{
		String input = "int foo(int x){}";
		List<AstNode> codeItems = parseInput(input);
		FunctionDefBase codeItem = (FunctionDefBase) codeItems.get(0);

		assertTrue(codeItem.getChildCount() == 4);
	}

	@Test
	public void testParamListGetCodeStr()
	{
		String input = "int foo(char *myParam, myType x){}";
		List<AstNode> codeItems = parseInput(input);
		FunctionDefBase codeItem = (FunctionDefBase) codeItems.get(0);
		String codeStr = codeItem.getParameterList().getEscapedCodeStr();
		assertTrue(codeStr.equals("char *myParam,myType x"));
	}

	@Test
	public void testParamGetCodeStr()
	{
		String input = "int foo(char *myParam, myType x){}";
		List<AstNode> codeItems = parseInput(input);
		FunctionDefBase codeItem = (FunctionDefBase) codeItems.get(0);
		ParameterBase parameter = codeItem.getParameterList().getParameter(0);
		String codeStr = parameter.getEscapedCodeStr();
		assertTrue(codeStr.equals("char *myParam"));
	}

	@Test
	public void testParamName()
	{
		String input = "int foo(myType myParam){}";
		List<AstNode> codeItems = parseInput(input);
		FunctionDefBase codeItem = (FunctionDefBase) codeItems.get(0);
		String name = codeItem.getParameterList().getParameter(0).getName();
		assertTrue(name.equals("myParam"));
	}

	@Test
	public void testParamType()
	{
		String input = "int foo(char *myParam){}";
		List<AstNode> codeItems = parseInput(input);
		FunctionDefBase codeItem = (FunctionDefBase) codeItems.get(0);
		ParameterType type = (ParameterType)codeItem.getParameterList().getParameter(0).getType();
		assertTrue(type.getEscapedCodeStr().equals("char *"));
	}

	@Test
	public void testFunctionPtrParam()
	{
		String input = "int foo(void (*ptr)(char *)){}";
		List<AstNode> codeItems = parseInput(input);
		FunctionDefBase codeItem = (FunctionDefBase) codeItems.get(0);
		assertTrue(codeItem.getName().equals("foo"));
	}

	@Test
	public void testEmptyParamList()
	{
		String input = "int foo(){}";
		List<AstNode> codeItems = parseInput(input);
		FunctionDefBase codeItem = (FunctionDefBase) codeItems.get(0);
		assertTrue(codeItem.getChildCount() == 4);
		assertTrue(codeItem.getParameterList().getEscapedCodeStr().equals(""));
	}

	@Test
	public void testEmptyParamListLocation()
	{
		String input = "int foo(){}";
		List<AstNode> codeItems = parseInput(input);
		FunctionDefBase codeItem = (FunctionDefBase) codeItems.get(0);
		assertTrue(codeItem.getParameterList().size() == 0);
	}

	@Test
	public void testTemplateAssociationWithClass() {
		String input = "template <typename T> class Foo { T t; };";
		List<AstNode> codeItems = parseInput(input);
		ClassDefStatement classDef = (ClassDefStatement) codeItems.get(0);
		TemplateParameterList templates = (TemplateParameterList) classDef.getChild(0);

		assertEquals("Foo", classDef.getIdentifier().getEscapedCodeStr());
		assertEquals(1, templates.getChildCount());
		assertEquals(1, templates.size());

		Template template = (Template) templates.getChild(0);

		assertEquals(1, template.getChildCount());
		assertEquals("T", template.getName());
	}

	@Test
	public void testTemplateTemplateAssociationWithClass() {
		String input = "template <template<typename> typename T, typename Z> class Foo { T<Z> t; };";
		List<AstNode> codeItems = parseInput(input);
		ClassDefStatement classDef = (ClassDefStatement) codeItems.get(0);
		TemplateParameterList templates = (TemplateParameterList) classDef.getChild(0);

		assertEquals("Foo", classDef.getIdentifier().getEscapedCodeStr());
		assertEquals(2, templates.size());
		assertEquals(2, templates.getChildCount());

		Template firstTemplate = (Template) templates.getChild(0);
		Template secondTemplate = (Template) templates.getChild(1);

		assertEquals(1, firstTemplate.getChildCount());
		assertEquals(1, secondTemplate.getChildCount());
		assertEquals("T", firstTemplate.getName());
		assertEquals("Z", secondTemplate.getName());
	}

	@Test
	public void testTemplateAssociationWithFunction() {
		String input = "template <typename T, typename Z> T foo(T x) { }";
		List<AstNode> codeItems = parseInput(input);
		FunctionDef functionDef = (FunctionDef) codeItems.get(0);
		TemplateParameterList templates = (TemplateParameterList) functionDef.getChild(1);

		assertEquals("foo", functionDef.getIdentifier().getEscapedCodeStr());
		assertEquals(2, templates.size());
		assertEquals(2, templates.getChildCount());

		Template firstTemplate = (Template) templates.getChild(0);
		Template secondTemplate = (Template) templates.getChild(1);

		assertEquals(1, firstTemplate.getChildCount());
		assertEquals(1, secondTemplate.getChildCount());
		assertEquals("T", firstTemplate.getName());
		assertEquals("Z", secondTemplate.getName());
	}

	@Test
	public void testTemplateTemplateAssociationWithFunction() {
		String input = "template <template<typename> typename T, typename Z> T<Z> foo(T<Z> x) { }";
		List<AstNode> codeItems = parseInput(input);
		FunctionDef functionDef = (FunctionDef) codeItems.get(0);
		TemplateParameterList templates = (TemplateParameterList) functionDef.getChild(1);

		assertEquals("foo", functionDef.getIdentifier().getEscapedCodeStr());
		assertEquals(2, templates.size());
		assertEquals(2, templates.getChildCount());

		Template firstTemplate = (Template) templates.getChild(0);
		Template secondTemplate = (Template) templates.getChild(1);

		assertEquals(1, firstTemplate.getChildCount());
		assertEquals(1, secondTemplate.getChildCount());
		assertEquals("T", firstTemplate.getName());
		assertEquals("Z", secondTemplate.getName());
	}

	// Test for https://github.com/ShiftLeftSecurity/joern/issues/126
	@Test
	public void testFunctionWithAnonParameter() {
		String input = "template <class C> C foo(C) {}";
		List<AstNode> codeItems = parseInput(input);
		FunctionDef functionDef = (FunctionDef) codeItems.get(0);
		ParameterList parameters = functionDef.getParameterList();
		TemplateParameterList templates = functionDef.getTemplateParameterList();

		assertEquals(1, parameters.size());
		Parameter param = (Parameter) parameters.getChild(0);
		assertEquals("<anonymous>", param.getName());
		assertEquals("C", param.getType().getEscapedCodeStr());

		assertEquals("foo", functionDef.getIdentifier().getEscapedCodeStr());
		assertEquals(1, templates.size());
		assertEquals(1, templates.getChildCount());

		Template firstTemplate = (Template) templates.getChild(0);
		assertEquals(1, firstTemplate.getChildCount());
		assertEquals("C", firstTemplate.getName());
	}

	@Test
	public void testFunctionWithAnonRefParameter() {
		String input = "template <class C> C foo(C&) {}";
		List<AstNode> codeItems = parseInput(input);
		FunctionDef functionDef = (FunctionDef) codeItems.get(0);
		ParameterList parameters = functionDef.getParameterList();
		TemplateParameterList templates = functionDef.getTemplateParameterList();

		assertEquals(1, parameters.size());
		Parameter param = (Parameter) parameters.getChild(0);
		assertEquals("<anonymous>", param.getName());
		assertEquals("C &", param.getType().getEscapedCodeStr());

		assertEquals("foo", functionDef.getIdentifier().getEscapedCodeStr());
		assertEquals(1, templates.size());
		assertEquals(1, templates.getChildCount());

		Template firstTemplate = (Template) templates.getChild(0);
		assertEquals(1, firstTemplate.getChildCount());
		assertEquals("C", firstTemplate.getName());
	}

	// Test for https://github.com/ShiftLeftSecurity/joern/issues/126
	@Test
	public void testFunctionDeclWithAnonParameter() {
		String input = "template <class C> C foo(C);";
		List<AstNode> codeItems = parseInput(input);
		FunctionDef functionDef = (FunctionDef) codeItems.get(0);
		ParameterList parameters = functionDef.getParameterList();
		TemplateParameterList templates = functionDef.getTemplateParameterList();

		assertEquals(1, parameters.size());
		Parameter param = (Parameter) parameters.getChild(0);
		assertEquals("<anonymous>", param.getName());
		assertEquals("C", param.getType().getEscapedCodeStr());

		assertEquals("foo", functionDef.getIdentifier().getEscapedCodeStr());
		assertEquals(1, templates.size());
		assertEquals(1, templates.getChildCount());

		Template firstTemplate = (Template) templates.getChild(0);
		assertEquals(1, firstTemplate.getChildCount());
		assertEquals("C", firstTemplate.getName());
	}

	@Test
	public void testFunctionDeclWithAnonRefParameter() {
		String input = "template <class C> C foo(C&);";
		List<AstNode> codeItems = parseInput(input);
		FunctionDef functionDef = (FunctionDef) codeItems.get(0);
		ParameterList parameters = functionDef.getParameterList();
		TemplateParameterList templates = functionDef.getTemplateParameterList();

		assertEquals(1, parameters.size());
		Parameter param = (Parameter) parameters.getChild(0);
		assertEquals("<anonymous>", param.getName());
		assertEquals("C &", param.getType().getEscapedCodeStr());

		assertEquals("foo", functionDef.getIdentifier().getEscapedCodeStr());
		assertEquals(1, templates.size());
		assertEquals(1, templates.getChildCount());

		Template firstTemplate = (Template) templates.getChild(0);
		assertEquals(1, firstTemplate.getChildCount());
		assertEquals("C", firstTemplate.getName());
	}

	@Test
	public void testTemplateAssociationWithFunctionDecl() {
		String input = "template <typename T, typename Z> T foo(T x);";
		List<AstNode> codeItems = parseInput(input);
		FunctionDef functionDef = (FunctionDef) codeItems.get(0);
		ParameterList parameters = functionDef.getParameterList();
		TemplateParameterList templates = functionDef.getTemplateParameterList();

		assertEquals("foo", functionDef.getIdentifier().getEscapedCodeStr());

		assertEquals(1, parameters.size());
		Parameter param = (Parameter) parameters.getChild(0);
		assertEquals("x", param.getName());
		assertEquals("T", param.getType().getEscapedCodeStr());

		assertEquals(2, templates.size());
		assertEquals(2, templates.getChildCount());

		Template firstTemplate = (Template) templates.getChild(0);
		Template secondTemplate = (Template) templates.getChild(1);

		assertEquals(1, firstTemplate.getChildCount());
		assertEquals(1, secondTemplate.getChildCount());
		assertEquals("T", firstTemplate.getName());
		assertEquals("Z", secondTemplate.getName());
	}

	@Test
	public void testTemplateTemplateAssociationWithFunctionDecl() {
		String input = "template <template<typename> typename T, typename Z> T<Z> foo(T<Z> x);";
		List<AstNode> codeItems = parseInput(input);
		FunctionDef functionDef = (FunctionDef) codeItems.get(0);
		TemplateParameterList templates = (TemplateParameterList) functionDef.getChild(1);

		assertEquals("foo", functionDef.getIdentifier().getEscapedCodeStr());
		assertEquals(2, templates.size());
		assertEquals(2, templates.getChildCount());

		Template firstTemplate = (Template) templates.getChild(0);
		Template secondTemplate = (Template) templates.getChild(1);

		assertEquals(1, firstTemplate.getChildCount());
		assertEquals(1, secondTemplate.getChildCount());
		assertEquals("T", firstTemplate.getName());
		assertEquals("Z", secondTemplate.getName());
	}

	private List<AstNode> parseInput(String input)
	{
		AntlrCModuleParserDriver parser = new AntlrCModuleParserDriver();
		tests.languages.c.parseTreeToAST.TestAntlrParserDriverObserver testProcessor = new tests.languages.c.parseTreeToAST.TestAntlrParserDriverObserver();
		parser.addObserver(testProcessor);

		CharStream inputStream = CharStreams.fromString(input);
		ModuleLexer lex = new ModuleLexer(inputStream);
		TokenSubStream tokens = new TokenSubStream(lex);

		parser.parseAndWalkTokenStream(tokens);
		return testProcessor.codeItems;
	}

}
