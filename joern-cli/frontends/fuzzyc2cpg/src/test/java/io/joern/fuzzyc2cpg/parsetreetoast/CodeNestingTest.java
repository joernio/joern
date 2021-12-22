package io.joern.fuzzyc2cpg.parsetreetoast;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import io.joern.fuzzyc2cpg.FunctionParser.StatementsContext;
import io.joern.fuzzyc2cpg.ast.declarations.ClassDefStatement;
import io.joern.fuzzyc2cpg.ast.declarations.IdentifierDecl;
import io.joern.fuzzyc2cpg.ast.expressions.Argument;
import io.joern.fuzzyc2cpg.ast.expressions.ArgumentList;
import io.joern.fuzzyc2cpg.ast.expressions.AssignmentExpression;
import io.joern.fuzzyc2cpg.ast.langc.expressions.CallExpression;
import io.joern.fuzzyc2cpg.ast.langc.statements.blockstarters.IfStatement;
import io.joern.fuzzyc2cpg.ast.logical.statements.BlockStarter;
import io.joern.fuzzyc2cpg.ast.logical.statements.CompoundStatement;
import io.joern.fuzzyc2cpg.ast.expressions.Condition;
import io.joern.fuzzyc2cpg.ast.statements.ExpressionStatement;
import io.joern.fuzzyc2cpg.ast.statements.IdentifierDeclStatement;
import io.joern.fuzzyc2cpg.ast.statements.blockstarters.ForStatement;
import org.junit.Test;

public class CodeNestingTest {

    @Test
    public void testLineNumbers() {
        String input = "if(foo)\nbar();\nfoo()\n";
        StatementsContext ctx = (StatementsContext) FunctionContentTestUtil
                .parse(input);
        assert (ctx.start.getLine() == 1);
        assert (ctx.stop.getLine() == 3);
    }

    @Test
    public void emptyContent() {
        String input = "";
        CompoundStatement item = (CompoundStatement) FunctionContentTestUtil
                .parseAndWalk(input);
        assert (item.getStatements().size() == 0);
    }

    @Test
    public void compoundWithoutBlockStart() {
        String input = "bar(); {}";
        CompoundStatement item = (CompoundStatement) FunctionContentTestUtil
                .parseAndWalk(input);
        assertEquals(2, item.getStatements().size());
    }

    @Test
    public void assignmentInCondition() {
        String input = "if(foo = bar){}";
        CompoundStatement item = (CompoundStatement) FunctionContentTestUtil
                .parseAndWalk(input);
        BlockStarter starter = (BlockStarter) item.getStatements().get(0);
        AssignmentExpression condition = (AssignmentExpression) ((Condition) starter.getCondition())
                .getExpression();
        assertEquals("foo = bar", condition.getEscapedCodeStr());
    }

    @Test
    public void whileInElse() {
        String input = "if(foo){bar();}else{ while(foo1){ if(bar2){} } }";
        CompoundStatement contentItem = (CompoundStatement) FunctionContentTestUtil
                .parseAndWalk(input);
        IfStatement ifItem = (IfStatement) contentItem.getStatements().get(0);
        assertNotNull(ifItem);
    }

    @Test
    public void complexIfElseNesting() {
        String input = "if (A){ if (B){ } if (C){ } } else { }";
        CompoundStatement contentItem = (CompoundStatement) FunctionContentTestUtil
                .parseAndWalk(input);
        IfStatement ifItem = (IfStatement) contentItem.getStatements().get(0);
        assertNotNull(ifItem.getElseNode());
    }

    @Test
    public void testFor() {
        String input = "for(i = 0; i < 10; i++){}";
        CompoundStatement contentItem = (CompoundStatement) FunctionContentTestUtil
                .parseAndWalk(input);
        ForStatement forItem = (ForStatement) contentItem.getStatements()
                .get(0);

        String condExprString = ((Condition) forItem.getCondition()).getExpression()
                .getEscapedCodeStr();
        assertEquals("i < 10", condExprString);

    }

    @Test
    public void testDeclInFor() {
        String input = "for(int i = 0; i < 10; i++){}";
        CompoundStatement contentItem = (CompoundStatement) FunctionContentTestUtil
                .parseAndWalk(input);
        ForStatement forItem = (ForStatement) contentItem.getStatements()
                .get(0);


        String condExprString = ((Condition) forItem.getCondition()).getExpression()
                .getEscapedCodeStr();
        assertEquals("i < 10", condExprString);

    }

    @Test
    public void testVarDeclName() {
        String input = "int x = 2*y;";
        CompoundStatement contentItem = (CompoundStatement) FunctionContentTestUtil
                .parseAndWalk(input);
        IdentifierDeclStatement declStatement = (IdentifierDeclStatement) contentItem
                .getStatements().get(0);
        IdentifierDecl decl = (IdentifierDecl) declStatement.getChild(0);
        assertEquals("x", decl.getName().getEscapedCodeStr());
    }

    @Test
    public void testVarDeclType() {
        String input = "int x = 2*y;";
        CompoundStatement contentItem = (CompoundStatement) FunctionContentTestUtil
                .parseAndWalk(input);
        IdentifierDeclStatement declStatement = (IdentifierDeclStatement) contentItem
                .getStatements().get(0);
        IdentifierDecl decl = (IdentifierDecl) declStatement.getChild(0);
        assertEquals("int", decl.getType().getEscapedCodeStr());
    }

    @Test
    public void testAssignment() {
        String input = "const char *m = \"Usage: untar [-tvx] [-f file] [file]\\n\";";
        CompoundStatement contentItem = (CompoundStatement) FunctionContentTestUtil
                .parseAndWalk(input);
        IdentifierDeclStatement declStatement = (IdentifierDeclStatement) contentItem
                .getStatements().get(0);
        IdentifierDecl decl = (IdentifierDecl) declStatement.getChild(0);

        AssignmentExpression assign = (AssignmentExpression) decl
                .getChild(decl.getChildCount() - 1);
        assertEquals("m", assign.getLeft().getEscapedCodeStr());
        assertEquals("\"Usage: untar [-tvx] [-f file] [file]\\n\"", assign.getRight().getEscapedCodeStr());
    }

    @Test
    public void testDeclRightAfterStruct() {
        String input = "struct foo{ int x; } foo;";
        CompoundStatement contentItem = (CompoundStatement) FunctionContentTestUtil
                .parseAndWalk(input);
        assertEquals(1, contentItem.getChildCount());
        ClassDefStatement classDef = (ClassDefStatement) contentItem
                .getChild(0);
        assertEquals(2, classDef.getChildCount());
        IdentifierDecl decl = (IdentifierDecl) classDef.getChild(1);
        assertEquals("foo", decl.getName().getEscapedCodeStr());
    }

    @Test
    public void testCall() {
        String input = "foo(x);";
        CompoundStatement contentItem = (CompoundStatement) FunctionContentTestUtil
                .parseAndWalk(input);
        ExpressionStatement stmt = (ExpressionStatement) contentItem
                .getStatements().get(0);
        CallExpression expr = (CallExpression) stmt.getChild(0);
        assertEquals("foo", expr.getTargetFunc().getEscapedCodeStr());
        ArgumentList argList = (ArgumentList) expr.getChild(1);
        Argument arg = (Argument) argList.getChild(0);
        assertNotNull(arg);
    }

    @Test
    public void testCallWithTwoArguments() {
        String input = "foo(x,y);";
        CompoundStatement contentItem = (CompoundStatement) FunctionContentTestUtil
                .parseAndWalk(input);
        ExpressionStatement stmt = (ExpressionStatement) contentItem
                .getStatements().get(0);
        CallExpression expr = (CallExpression) stmt.getChild(0);
        assertEquals("foo", expr.getTargetFunc().getEscapedCodeStr());
    }

}
