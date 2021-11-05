package tests.languages.c.parseTreeToAST;

import io.shiftleft.fuzzyc2cpg.ast.AstNode;
import io.shiftleft.fuzzyc2cpg.ast.AstNodeBuilder;
import java.util.LinkedList;
import java.util.List;
import java.util.Stack;

import io.shiftleft.fuzzyc2cpg.parser.AntlrParserDriverObserver;
import org.antlr.v4.runtime.ParserRuleContext;

public class TestAntlrParserDriverObserver implements AntlrParserDriverObserver
{

	public List<AstNode> codeItems;

	public TestAntlrParserDriverObserver()
	{
		codeItems = new LinkedList<>();
	}

	@Override
	public void begin() {

	}

	@Override
	public void end() {

	}

	@Override
	public void startOfUnit(ParserRuleContext ctx, String filename)
	{

	}

	@Override
	public void endOfUnit(ParserRuleContext ctx, String filename)
	{

	}

	@Override
	public <T extends AstNode> void processItem(T item, Stack<AstNodeBuilder<? extends AstNode>> builderStack)
	{
		codeItems.add(item);
	}

}
