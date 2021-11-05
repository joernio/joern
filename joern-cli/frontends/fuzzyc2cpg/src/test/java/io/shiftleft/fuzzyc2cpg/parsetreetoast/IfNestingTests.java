package io.shiftleft.fuzzyc2cpg.parsetreetoast;

import static org.junit.Assert.assertTrue;

import io.shiftleft.fuzzyc2cpg.ast.expressions.Expression;
import io.shiftleft.fuzzyc2cpg.ast.langc.statements.blockstarters.ElseStatement;
import io.shiftleft.fuzzyc2cpg.ast.langc.statements.blockstarters.IfStatement;
import io.shiftleft.fuzzyc2cpg.ast.logical.statements.BlockStarter;
import io.shiftleft.fuzzyc2cpg.ast.logical.statements.CompoundStatement;
import io.shiftleft.fuzzyc2cpg.ast.expressions.Condition;
import org.junit.Test;


public class IfNestingTests
{

	@Test
	public void ifBlockCompound()
	{
		String input = "if(foo){}";
		CompoundStatement compound = (CompoundStatement) FunctionContentTestUtil
				.parseAndWalk(input);
		assertFirstChildIsIfStatement(compound);
	}

	@Test
	public void ifBlockNoCompound()
	{
		String input = "if(foo) bar();";
		CompoundStatement compound = (CompoundStatement) FunctionContentTestUtil
				.parseAndWalk(input);
		assertFirstChildIsIfStatement(compound);
	}

	@Test
	public void nestedIfBlocksNoCompound()
	{
		String input = "if(foo) if(fooAgain) bar();";
		CompoundStatement compound = (CompoundStatement) FunctionContentTestUtil
				.parseAndWalk(input);
		IfStatement ifStatement = (IfStatement) compound.getStatements().get(0);
		IfStatement innerStatement = (IfStatement) ifStatement.getStatement();

		assertFirstChildIsIfStatement(compound);
		assertTrue(innerStatement.getCondition() != null);
	}

	@Test
	public void conditionString()
	{
		String input = "if(foo){}";
		CompoundStatement item = (CompoundStatement) FunctionContentTestUtil
				.parseAndWalk(input);
		BlockStarter starter = (BlockStarter) item.getStatements().get(0);
		Expression condition = ((Condition)starter.getCondition()).getExpression();
		assertTrue(condition.getEscapedCodeStr().equals("foo"));
	}

	@Test
	public void ifElse()
	{
		String input = "if(foo) lr->f = stdin; else lr->f = fopen(pathname, \"r\");";
		CompoundStatement compound = (CompoundStatement) FunctionContentTestUtil
				.parseAndWalk(input);

		assertFirstChildIsIfStatement(compound);
		assertFirstIfHasElse(compound);
	}

	@Test
	public void ifElseChain()
	{
		String input = "if(foo1) bar1(); else if(foo2) bar2(); else if(foo3) bar3();";
		CompoundStatement compound = (CompoundStatement) FunctionContentTestUtil
				.parseAndWalk(input);

		IfStatement ifItem = (IfStatement) compound.getStatements().get(0);
		for (int i = 0; i < 2; i++)
		{
			assertHasElse(ifItem);
			ifItem = (IfStatement) ifItem.getElseNode().getStatement();
		}
	}

	@Test
	public void ifInElse()
	{
		String input = "if (foo1){} else { if (foo2) { foo(); } }";
		CompoundStatement compound = (CompoundStatement) FunctionContentTestUtil
				.parseAndWalk(input);
		IfStatement ifItem = (IfStatement) compound.getStatements().get(0);

		assertFirstChildIsIfStatement(compound);
		assertFirstIfHasElse(compound);

		ElseStatement elseNode = ifItem.getElseNode();
		CompoundStatement innerCompound = (CompoundStatement) elseNode
				.getStatement();
		assertTrue(innerCompound.getChildCount() == 1);
		IfStatement innerIf = (IfStatement) innerCompound.getChild(0);
		assertTrue(innerIf.getCondition() != null);
	}

	private void assertFirstChildIsIfStatement(CompoundStatement compound)
	{
		IfStatement ifStatement = (IfStatement) compound.getStatements().get(0);
		assertTrue(compound.getStatements().size() == 1);
		assertTrue(ifStatement.getCondition() != null);
	}

	private void assertFirstIfHasElse(CompoundStatement compound)
	{
		IfStatement ifItem = (IfStatement) compound.getStatements().get(0);
		assertHasElse(ifItem);
	}

	private void assertHasElse(IfStatement ifItem)
	{
		ElseStatement elseNode = ifItem.getElseNode();
		assertTrue(elseNode != null);
		assertTrue(elseNode.getChild(0) != null);
	}

}
