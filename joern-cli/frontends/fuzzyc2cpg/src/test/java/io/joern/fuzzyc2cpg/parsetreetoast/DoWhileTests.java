package io.joern.fuzzyc2cpg.parsetreetoast;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import io.joern.fuzzyc2cpg.ast.langc.statements.blockstarters.IfStatement;
import io.joern.fuzzyc2cpg.ast.logical.statements.CompoundStatement;
import io.joern.fuzzyc2cpg.ast.expressions.Condition;
import io.joern.fuzzyc2cpg.ast.statements.blockstarters.DoStatement;
import io.joern.fuzzyc2cpg.ast.statements.blockstarters.WhileStatement;
import org.junit.Test;


public class DoWhileTests {

    @Test
    public void testDoWhile() {
        String input = "do{ foo(); }while(bar);";
        CompoundStatement contentItem = (CompoundStatement) FunctionContentTestUtil
                .parseAndWalk(input);
        DoStatement doItem = (DoStatement) contentItem.getStatements().get(0);

        String condExprString = ((Condition) doItem.getCondition()).getExpression()
                .getEscapedCodeStr();
        assertEquals("bar", condExprString);

    }

    @Test
    public void testWhileInDoWhile() {
        String input = "do{ while(foo0) foo(); }while(bar);";
        CompoundStatement contentItem = (CompoundStatement) FunctionContentTestUtil
                .parseAndWalk(input);
        DoStatement doItem = (DoStatement) contentItem.getStatements().get(0);

        CompoundStatement doCompound = (CompoundStatement) doItem
                .getStatement();
        WhileStatement whileStatement = (WhileStatement) doCompound.getChild(0);
        assertNotNull(whileStatement.getCondition());

        String condExprString = ((Condition) doItem.getCondition()).getExpression()
                .getEscapedCodeStr();
        assertEquals("bar", condExprString);
    }

    @Test
    public void testIfElseInDoWhile() {
        String input = "do{ if(foo)foo0(); else x++; }while(bar);";
        CompoundStatement contentItem = (CompoundStatement) FunctionContentTestUtil
                .parseAndWalk(input);
        DoStatement doItem = (DoStatement) contentItem.getStatements().get(0);

        String condExprString = ((Condition) doItem.getCondition()).getExpression()
                .getEscapedCodeStr();
        assertEquals("bar", condExprString);
    }

    @Test
    public void testDoWhileInIf() {
        String input = "if(foo) do x++; while(bar); ";

        CompoundStatement contentItem = (CompoundStatement) FunctionContentTestUtil
                .parseAndWalk(input);
        IfStatement ifStatement = (IfStatement) contentItem.getStatements()
                .get(0);
        DoStatement doItem = (DoStatement) ifStatement.getStatement();

        String condExprString = ((Condition) doItem.getCondition()).getExpression()
                .getEscapedCodeStr();
        assertEquals("bar", condExprString);
    }

    @Test
    public void testNestedDoWhile() {
        String input = "do{ do foo(); while(x); }while(bar);";
        CompoundStatement contentItem = (CompoundStatement) FunctionContentTestUtil
                .parseAndWalk(input);
        DoStatement doItem = (DoStatement) contentItem.getStatements().get(0);

        String condExprString = ((Condition) doItem.getCondition()).getExpression()
                .getEscapedCodeStr();
        assertEquals("bar", condExprString);
    }

}
