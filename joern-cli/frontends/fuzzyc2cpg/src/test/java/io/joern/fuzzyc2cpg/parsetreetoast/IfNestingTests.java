package io.joern.fuzzyc2cpg.parsetreetoast;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import io.joern.fuzzyc2cpg.ast.expressions.Expression;
import io.joern.fuzzyc2cpg.ast.langc.statements.blockstarters.ElseStatement;
import io.joern.fuzzyc2cpg.ast.langc.statements.blockstarters.IfStatement;
import io.joern.fuzzyc2cpg.ast.logical.statements.BlockStarter;
import io.joern.fuzzyc2cpg.ast.logical.statements.CompoundStatement;
import io.joern.fuzzyc2cpg.ast.expressions.Condition;
import org.junit.Test;


public class IfNestingTests {

    @Test
    public void ifBlockCompound() {
        String input = "if(foo){}";
        CompoundStatement compound = (CompoundStatement) FunctionContentTestUtil
                .parseAndWalk(input);
        assertFirstChildIsIfStatement(compound);
    }

    @Test
    public void ifBlockNoCompound() {
        String input = "if(foo) bar();";
        CompoundStatement compound = (CompoundStatement) FunctionContentTestUtil
                .parseAndWalk(input);
        assertFirstChildIsIfStatement(compound);
    }

    @Test
    public void nestedIfBlocksNoCompound() {
        String input = "if(foo) if(fooAgain) bar();";
        CompoundStatement compound = (CompoundStatement) FunctionContentTestUtil
                .parseAndWalk(input);
        IfStatement ifStatement = (IfStatement) compound.getStatements().get(0);
        IfStatement innerStatement = (IfStatement) ifStatement.getStatement();

        assertFirstChildIsIfStatement(compound);
        assertNotNull(innerStatement.getCondition());
    }

    @Test
    public void conditionString() {
        String input = "if(foo){}";
        CompoundStatement item = (CompoundStatement) FunctionContentTestUtil
                .parseAndWalk(input);
        BlockStarter starter = (BlockStarter) item.getStatements().get(0);
        Expression condition = ((Condition) starter.getCondition()).getExpression();
        assertEquals("foo", condition.getEscapedCodeStr());
    }

    @Test
    public void ifElse() {
        String input = "if(foo) lr->f = stdin; else lr->f = fopen(pathname, \"r\");";
        CompoundStatement compound = (CompoundStatement) FunctionContentTestUtil
                .parseAndWalk(input);

        assertFirstChildIsIfStatement(compound);
        assertFirstIfHasElse(compound);
    }

    @Test
    public void ifElseChain() {
        String input = "if(foo1) bar1(); else if(foo2) bar2(); else if(foo3) bar3();";
        CompoundStatement compound = (CompoundStatement) FunctionContentTestUtil
                .parseAndWalk(input);

        IfStatement ifItem = (IfStatement) compound.getStatements().get(0);
        for (int i = 0; i < 2; i++) {
            assertHasElse(ifItem);
            ifItem = (IfStatement) ifItem.getElseNode().getStatement();
        }
    }

    @Test
    public void ifInElse() {
        String input = "if (foo1){} else { if (foo2) { foo(); } }";
        CompoundStatement compound = (CompoundStatement) FunctionContentTestUtil
                .parseAndWalk(input);
        IfStatement ifItem = (IfStatement) compound.getStatements().get(0);

        assertFirstChildIsIfStatement(compound);
        assertFirstIfHasElse(compound);

        ElseStatement elseNode = ifItem.getElseNode();
        CompoundStatement innerCompound = (CompoundStatement) elseNode
                .getStatement();
        assertEquals(1, innerCompound.getChildCount());
        IfStatement innerIf = (IfStatement) innerCompound.getChild(0);
        assertNotNull(innerIf.getCondition());
    }

    private void assertFirstChildIsIfStatement(CompoundStatement compound) {
        IfStatement ifStatement = (IfStatement) compound.getStatements().get(0);
        assertEquals(1, compound.getStatements().size());
        assertNotNull(ifStatement.getCondition());
    }

    private void assertFirstIfHasElse(CompoundStatement compound) {
        IfStatement ifItem = (IfStatement) compound.getStatements().get(0);
        assertHasElse(ifItem);
    }

    private void assertHasElse(IfStatement ifItem) {
        ElseStatement elseNode = ifItem.getElseNode();
        assertNotNull(elseNode);
        assertNotNull(elseNode.getChild(0));
    }

}
