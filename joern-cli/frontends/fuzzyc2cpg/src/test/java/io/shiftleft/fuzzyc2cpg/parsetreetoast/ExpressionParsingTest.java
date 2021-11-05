package io.shiftleft.fuzzyc2cpg.parsetreetoast;

import static org.junit.Assert.assertTrue;

import io.shiftleft.fuzzyc2cpg.ast.declarations.IdentifierDecl;
import io.shiftleft.fuzzyc2cpg.ast.expressions.AdditiveExpression;
import io.shiftleft.fuzzyc2cpg.ast.expressions.AndExpression;
import io.shiftleft.fuzzyc2cpg.ast.expressions.AssignmentExpression;
import io.shiftleft.fuzzyc2cpg.ast.expressions.BitAndExpression;
import io.shiftleft.fuzzyc2cpg.ast.expressions.CastExpression;
import io.shiftleft.fuzzyc2cpg.ast.expressions.ConditionalExpression;
import io.shiftleft.fuzzyc2cpg.ast.expressions.EqualityExpression;
import io.shiftleft.fuzzyc2cpg.ast.expressions.ExclusiveOrExpression;
import io.shiftleft.fuzzyc2cpg.ast.expressions.InclusiveOrExpression;
import io.shiftleft.fuzzyc2cpg.ast.expressions.MultiplicativeExpression;
import io.shiftleft.fuzzyc2cpg.ast.expressions.OrExpression;
import io.shiftleft.fuzzyc2cpg.ast.expressions.RelationalExpression;
import io.shiftleft.fuzzyc2cpg.ast.expressions.ShiftExpression;
import io.shiftleft.fuzzyc2cpg.ast.langc.expressions.CallExpression;
import io.shiftleft.fuzzyc2cpg.ast.logical.statements.BlockStarter;
import io.shiftleft.fuzzyc2cpg.ast.logical.statements.CompoundStatement;
import io.shiftleft.fuzzyc2cpg.ast.expressions.Condition;
import io.shiftleft.fuzzyc2cpg.ast.statements.ExpressionStatement;
import io.shiftleft.fuzzyc2cpg.ast.statements.IdentifierDeclStatement;
import org.junit.Test;

public class ExpressionParsingTest
{

	@Test
	public void testMostBasicAssignment()
	{
		String input = "x = y;";
		CompoundStatement contentItem = (CompoundStatement) FunctionContentTestUtil
				.parseAndWalk(input);
		ExpressionStatement statementItem = (ExpressionStatement) contentItem
				.getStatements().get(0);
		AssignmentExpression expr = (AssignmentExpression) statementItem.getExpression();

		assertTrue(expr.getLeft().getEscapedCodeStr().equals("x"));
		assertTrue(expr.getRight().getEscapedCodeStr().equals("y"));
	}

	@Test
	public void testBasicAssignmentChain()
	{
		String input = "x = y = z;";
		CompoundStatement contentItem = (CompoundStatement) FunctionContentTestUtil
				.parseAndWalk(input);
		ExpressionStatement statementItem = (ExpressionStatement) contentItem
				.getStatements().get(0);
		AssignmentExpression expr = (AssignmentExpression) statementItem.getExpression();
		assertTrue(expr.getLeft().getEscapedCodeStr().equals("x"));
		assertTrue(expr.getRight().getEscapedCodeStr().equals("y = z"));
	}

	@Test
	public void testMostBasicLocalVar()
	{
		String input = "int x;";
		CompoundStatement contentItem = (CompoundStatement) FunctionContentTestUtil
				.parseAndWalk(input);
		IdentifierDeclStatement statementItem = (IdentifierDeclStatement) contentItem
				.getStatements().get(0);
		IdentifierDecl identifierDecl = (IdentifierDecl) statementItem
				.getIdentifierDeclList().get(0);
		assertTrue(identifierDecl.getName().getEscapedCodeStr().equals("x"));
	}

	@Test
	public void testConditionalExpr()
	{
		String input = "foo = cond? x : y;";
		CompoundStatement contentItem = (CompoundStatement) FunctionContentTestUtil
				.parseAndWalk(input);
		ExpressionStatement statementItem = (ExpressionStatement) contentItem
				.getStatements().get(0);
		AssignmentExpression expr = (AssignmentExpression) statementItem.getExpression();
		ConditionalExpression right = (ConditionalExpression) expr.getRight();
		assertTrue(right.getChild(0).getEscapedCodeStr().equals("cond"));
	}

	@Test
	public void testOrExpr()
	{
		String input = "x || y;";
		CompoundStatement contentItem = (CompoundStatement) FunctionContentTestUtil
				.parseAndWalk(input);
		ExpressionStatement statementItem = (ExpressionStatement) contentItem
				.getStatements().get(0);
		OrExpression expr = (OrExpression) statementItem.getExpression();
		assertTrue(expr.getLeft().getEscapedCodeStr().equals("x"));
	}

	@Test
	public void testAndExpr()
	{
		String input = "x && y;";
		CompoundStatement contentItem = (CompoundStatement) FunctionContentTestUtil
				.parseAndWalk(input);
		ExpressionStatement statementItem = (ExpressionStatement) contentItem
				.getStatements().get(0);
		AndExpression expr = (AndExpression) statementItem.getExpression();
		assertTrue(expr.getLeft().getEscapedCodeStr().equals("x"));
	}

	@Test
	public void testInclusiveOrExpr()
	{
		String input = "x | y;";
		CompoundStatement contentItem = (CompoundStatement) FunctionContentTestUtil
				.parseAndWalk(input);
		ExpressionStatement statementItem = (ExpressionStatement) contentItem
				.getStatements().get(0);
		InclusiveOrExpression expr = (InclusiveOrExpression) statementItem
				.getExpression();
		assertTrue(expr.getLeft().getEscapedCodeStr().equals("x"));
	}

	@Test
	public void testExclusiveOrExpr()
	{
		String input = "x ^ y;";
		CompoundStatement contentItem = (CompoundStatement) FunctionContentTestUtil
				.parseAndWalk(input);
		ExpressionStatement statementItem = (ExpressionStatement) contentItem
				.getStatements().get(0);
		ExclusiveOrExpression expr = (ExclusiveOrExpression) statementItem
				.getExpression();
		assertTrue(expr.getLeft().getEscapedCodeStr().equals("x"));
	}

	@Test
	public void testBitAndExpr()
	{
		String input = "if(x & y){};";
		CompoundStatement contentItem = (CompoundStatement) FunctionContentTestUtil
				.parseAndWalk(input);
		BlockStarter starter = (BlockStarter) contentItem.getStatements()
				.get(0);
		BitAndExpression expr = (BitAndExpression) ((Condition)starter.getCondition())
				.getExpression();
		assertTrue(expr.getLeft().getEscapedCodeStr().equals("x"));
	}

	@Test
	public void EqualityExpr()
	{
		String input = "if(x == y){};";
		CompoundStatement contentItem = (CompoundStatement) FunctionContentTestUtil
				.parseAndWalk(input);
		BlockStarter starter = (BlockStarter) contentItem.getStatements()
				.get(0);
		EqualityExpression expr = (EqualityExpression) ((Condition)starter.getCondition())
				.getExpression();
		assertTrue(expr.getLeft().getEscapedCodeStr().equals("x"));
	}

	@Test
	public void RelationalExpr()
	{
		String input = "if(x < y){};";
		CompoundStatement contentItem = (CompoundStatement) FunctionContentTestUtil
				.parseAndWalk(input);
		BlockStarter starter = (BlockStarter) contentItem.getStatements()
				.get(0);
		RelationalExpression expr = (RelationalExpression) ((Condition)starter
				.getCondition()).getExpression();
		assertTrue(expr.getLeft().getEscapedCodeStr().equals("x"));
	}

	@Test
	public void ShiftExpr()
	{
		String input = "if(x >> y){};";
		CompoundStatement contentItem = (CompoundStatement) FunctionContentTestUtil
				.parseAndWalk(input);
		BlockStarter starter = (BlockStarter) contentItem.getStatements()
				.get(0);
		ShiftExpression expr = (ShiftExpression) ((Condition)starter.getCondition())
				.getExpression();
		assertTrue(expr.getLeft().getEscapedCodeStr().equals("x"));
	}

	@Test
	public void AdditiveExpr()
	{
		String input = "if(x + y){};";
		CompoundStatement contentItem = (CompoundStatement) FunctionContentTestUtil
				.parseAndWalk(input);
		BlockStarter starter = (BlockStarter) contentItem.getStatements()
				.get(0);
		AdditiveExpression expr = (AdditiveExpression) ((Condition)starter.getCondition())
				.getExpression();
		assertTrue(expr.getLeft().getEscapedCodeStr().equals("x"));
	}

	@Test
	public void MultiplicativeExpr()
	{
		String input = "if(x * y){};";
		CompoundStatement contentItem = (CompoundStatement) FunctionContentTestUtil
				.parseAndWalk(input);
		BlockStarter starter = (BlockStarter) contentItem.getStatements()
				.get(0);
		MultiplicativeExpression expr = (MultiplicativeExpression) ((Condition)starter
				.getCondition()).getExpression();
		assertTrue(expr.getLeft().getEscapedCodeStr().equals("x"));
	}

	@Test
	public void CastExpr()
	{
		String input = "if((some_type) y){};";
		CompoundStatement contentItem = (CompoundStatement) FunctionContentTestUtil
				.parseAndWalk(input);
		BlockStarter starter = (BlockStarter) contentItem.getStatements()
				.get(0);
		CastExpression expr = (CastExpression) ((Condition)starter.getCondition())
				.getExpression();
		assertTrue(
				expr.getCastTarget().getEscapedCodeStr().equals("some_type"));
	}

	@Test
	public void funCall()
	{
		String input = "if(foo()){};";
		CompoundStatement contentItem = (CompoundStatement) FunctionContentTestUtil
				.parseAndWalk(input);
		BlockStarter starter = (BlockStarter) contentItem.getStatements()
				.get(0);
		CallExpression expr = (CallExpression) ((Condition)starter.getCondition())
				.getExpression();
		assertTrue(expr.getTargetFunc().getEscapedCodeStr().equals("foo"));
	}

}
