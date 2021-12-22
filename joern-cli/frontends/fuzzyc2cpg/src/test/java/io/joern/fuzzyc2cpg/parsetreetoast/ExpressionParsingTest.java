package io.joern.fuzzyc2cpg.parsetreetoast;

import static org.junit.Assert.assertEquals;

import io.joern.fuzzyc2cpg.ast.declarations.IdentifierDecl;
import io.joern.fuzzyc2cpg.ast.expressions.AdditiveExpression;
import io.joern.fuzzyc2cpg.ast.expressions.AndExpression;
import io.joern.fuzzyc2cpg.ast.expressions.AssignmentExpression;
import io.joern.fuzzyc2cpg.ast.expressions.BitAndExpression;
import io.joern.fuzzyc2cpg.ast.expressions.CastExpression;
import io.joern.fuzzyc2cpg.ast.expressions.ConditionalExpression;
import io.joern.fuzzyc2cpg.ast.expressions.EqualityExpression;
import io.joern.fuzzyc2cpg.ast.expressions.ExclusiveOrExpression;
import io.joern.fuzzyc2cpg.ast.expressions.InclusiveOrExpression;
import io.joern.fuzzyc2cpg.ast.expressions.MultiplicativeExpression;
import io.joern.fuzzyc2cpg.ast.expressions.OrExpression;
import io.joern.fuzzyc2cpg.ast.expressions.RelationalExpression;
import io.joern.fuzzyc2cpg.ast.expressions.ShiftExpression;
import io.joern.fuzzyc2cpg.ast.langc.expressions.CallExpression;
import io.joern.fuzzyc2cpg.ast.logical.statements.BlockStarter;
import io.joern.fuzzyc2cpg.ast.logical.statements.CompoundStatement;
import io.joern.fuzzyc2cpg.ast.expressions.Condition;
import io.joern.fuzzyc2cpg.ast.statements.ExpressionStatement;
import io.joern.fuzzyc2cpg.ast.statements.IdentifierDeclStatement;
import org.junit.Test;

public class ExpressionParsingTest {

    @Test
    public void testMostBasicAssignment() {
        String input = "x = y;";
        CompoundStatement contentItem = (CompoundStatement) FunctionContentTestUtil
                .parseAndWalk(input);
        ExpressionStatement statementItem = (ExpressionStatement) contentItem
                .getStatements().get(0);
        AssignmentExpression expr = (AssignmentExpression) statementItem.getExpression();

        assertEquals("x", expr.getLeft().getEscapedCodeStr());
        assertEquals("y", expr.getRight().getEscapedCodeStr());
    }

    @Test
    public void testBasicAssignmentChain() {
        String input = "x = y = z;";
        CompoundStatement contentItem = (CompoundStatement) FunctionContentTestUtil
                .parseAndWalk(input);
        ExpressionStatement statementItem = (ExpressionStatement) contentItem
                .getStatements().get(0);
        AssignmentExpression expr = (AssignmentExpression) statementItem.getExpression();
        assertEquals("x", expr.getLeft().getEscapedCodeStr());
        assertEquals("y = z", expr.getRight().getEscapedCodeStr());
    }

    @Test
    public void testMostBasicLocalVar() {
        String input = "int x;";
        CompoundStatement contentItem = (CompoundStatement) FunctionContentTestUtil
                .parseAndWalk(input);
        IdentifierDeclStatement statementItem = (IdentifierDeclStatement) contentItem
                .getStatements().get(0);
        IdentifierDecl identifierDecl = (IdentifierDecl) statementItem
                .getIdentifierDeclList().get(0);
        assertEquals("x", identifierDecl.getName().getEscapedCodeStr());
    }

    @Test
    public void testConditionalExpr() {
        String input = "foo = cond? x : y;";
        CompoundStatement contentItem = (CompoundStatement) FunctionContentTestUtil
                .parseAndWalk(input);
        ExpressionStatement statementItem = (ExpressionStatement) contentItem
                .getStatements().get(0);
        AssignmentExpression expr = (AssignmentExpression) statementItem.getExpression();
        ConditionalExpression right = (ConditionalExpression) expr.getRight();
        assertEquals("cond", right.getChild(0).getEscapedCodeStr());
    }

    @Test
    public void testOrExpr() {
        String input = "x || y;";
        CompoundStatement contentItem = (CompoundStatement) FunctionContentTestUtil
                .parseAndWalk(input);
        ExpressionStatement statementItem = (ExpressionStatement) contentItem
                .getStatements().get(0);
        OrExpression expr = (OrExpression) statementItem.getExpression();
        assertEquals("x", expr.getLeft().getEscapedCodeStr());
    }

    @Test
    public void testAndExpr() {
        String input = "x && y;";
        CompoundStatement contentItem = (CompoundStatement) FunctionContentTestUtil
                .parseAndWalk(input);
        ExpressionStatement statementItem = (ExpressionStatement) contentItem
                .getStatements().get(0);
        AndExpression expr = (AndExpression) statementItem.getExpression();
        assertEquals("x", expr.getLeft().getEscapedCodeStr());
    }

    @Test
    public void testInclusiveOrExpr() {
        String input = "x | y;";
        CompoundStatement contentItem = (CompoundStatement) FunctionContentTestUtil
                .parseAndWalk(input);
        ExpressionStatement statementItem = (ExpressionStatement) contentItem
                .getStatements().get(0);
        InclusiveOrExpression expr = (InclusiveOrExpression) statementItem
                .getExpression();
        assertEquals("x", expr.getLeft().getEscapedCodeStr());
    }

    @Test
    public void testExclusiveOrExpr() {
        String input = "x ^ y;";
        CompoundStatement contentItem = (CompoundStatement) FunctionContentTestUtil
                .parseAndWalk(input);
        ExpressionStatement statementItem = (ExpressionStatement) contentItem
                .getStatements().get(0);
        ExclusiveOrExpression expr = (ExclusiveOrExpression) statementItem
                .getExpression();
        assertEquals("x", expr.getLeft().getEscapedCodeStr());
    }

    @Test
    public void testBitAndExpr() {
        String input = "if(x & y){};";
        CompoundStatement contentItem = (CompoundStatement) FunctionContentTestUtil
                .parseAndWalk(input);
        BlockStarter starter = (BlockStarter) contentItem.getStatements()
                .get(0);
        BitAndExpression expr = (BitAndExpression) ((Condition) starter.getCondition())
                .getExpression();
        assertEquals("x", expr.getLeft().getEscapedCodeStr());
    }

    @Test
    public void EqualityExpr() {
        String input = "if(x == y){};";
        CompoundStatement contentItem = (CompoundStatement) FunctionContentTestUtil
                .parseAndWalk(input);
        BlockStarter starter = (BlockStarter) contentItem.getStatements()
                .get(0);
        EqualityExpression expr = (EqualityExpression) ((Condition) starter.getCondition())
                .getExpression();
        assertEquals("x", expr.getLeft().getEscapedCodeStr());
    }

    @Test
    public void RelationalExpr() {
        String input = "if(x < y){};";
        CompoundStatement contentItem = (CompoundStatement) FunctionContentTestUtil
                .parseAndWalk(input);
        BlockStarter starter = (BlockStarter) contentItem.getStatements()
                .get(0);
        RelationalExpression expr = (RelationalExpression) ((Condition) starter
                .getCondition()).getExpression();
        assertEquals("x", expr.getLeft().getEscapedCodeStr());
    }

    @Test
    public void ShiftExpr() {
        String input = "if(x >> y){};";
        CompoundStatement contentItem = (CompoundStatement) FunctionContentTestUtil
                .parseAndWalk(input);
        BlockStarter starter = (BlockStarter) contentItem.getStatements()
                .get(0);
        ShiftExpression expr = (ShiftExpression) ((Condition) starter.getCondition())
                .getExpression();
        assertEquals("x", expr.getLeft().getEscapedCodeStr());
    }

    @Test
    public void AdditiveExpr() {
        String input = "if(x + y){};";
        CompoundStatement contentItem = (CompoundStatement) FunctionContentTestUtil
                .parseAndWalk(input);
        BlockStarter starter = (BlockStarter) contentItem.getStatements()
                .get(0);
        AdditiveExpression expr = (AdditiveExpression) ((Condition) starter.getCondition())
                .getExpression();
        assertEquals("x", expr.getLeft().getEscapedCodeStr());
    }

    @Test
    public void MultiplicativeExpr() {
        String input = "if(x * y){};";
        CompoundStatement contentItem = (CompoundStatement) FunctionContentTestUtil
                .parseAndWalk(input);
        BlockStarter starter = (BlockStarter) contentItem.getStatements()
                .get(0);
        MultiplicativeExpression expr = (MultiplicativeExpression) ((Condition) starter
                .getCondition()).getExpression();
        assertEquals("x", expr.getLeft().getEscapedCodeStr());
    }

    @Test
    public void CastExpr() {
        String input = "if((some_type) y){};";
        CompoundStatement contentItem = (CompoundStatement) FunctionContentTestUtil
                .parseAndWalk(input);
        BlockStarter starter = (BlockStarter) contentItem.getStatements()
                .get(0);
        CastExpression expr = (CastExpression) ((Condition) starter.getCondition())
                .getExpression();
        assertEquals("some_type", expr.getCastTarget().getEscapedCodeStr());
    }

    @Test
    public void funCall() {
        String input = "if(foo()){};";
        CompoundStatement contentItem = (CompoundStatement) FunctionContentTestUtil
                .parseAndWalk(input);
        BlockStarter starter = (BlockStarter) contentItem.getStatements()
                .get(0);
        CallExpression expr = (CallExpression) ((Condition) starter.getCondition())
                .getExpression();
        assertEquals("foo", expr.getTargetFunc().getEscapedCodeStr());
    }

}
