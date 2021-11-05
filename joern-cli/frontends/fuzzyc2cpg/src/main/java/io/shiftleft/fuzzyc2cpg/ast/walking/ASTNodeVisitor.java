package io.shiftleft.fuzzyc2cpg.ast.walking;

import io.shiftleft.fuzzyc2cpg.ast.AstNode;
import io.shiftleft.fuzzyc2cpg.ast.declarations.ClassDefStatement;
import io.shiftleft.fuzzyc2cpg.ast.declarations.IdentifierDecl;
import io.shiftleft.fuzzyc2cpg.ast.declarations.IdentifierDeclType;
import io.shiftleft.fuzzyc2cpg.ast.expressions.*;
import io.shiftleft.fuzzyc2cpg.ast.functionDef.*;
import io.shiftleft.fuzzyc2cpg.ast.langc.expressions.CallExpression;
import io.shiftleft.fuzzyc2cpg.ast.langc.expressions.SizeofExpression;
import io.shiftleft.fuzzyc2cpg.ast.langc.functiondef.FunctionDef;
import io.shiftleft.fuzzyc2cpg.ast.langc.functiondef.Parameter;
import io.shiftleft.fuzzyc2cpg.ast.langc.functiondef.ParameterType;
import io.shiftleft.fuzzyc2cpg.ast.langc.statements.blockstarters.ElseStatement;
import io.shiftleft.fuzzyc2cpg.ast.langc.statements.blockstarters.IfStatement;
import io.shiftleft.fuzzyc2cpg.ast.logical.statements.BlockCloser;
import io.shiftleft.fuzzyc2cpg.ast.logical.statements.BlockStarter;
import io.shiftleft.fuzzyc2cpg.ast.logical.statements.BlockStarterWithStmtAndCnd;
import io.shiftleft.fuzzyc2cpg.ast.logical.statements.BreakOrContinueStatement;
import io.shiftleft.fuzzyc2cpg.ast.logical.statements.CompoundStatement;
import io.shiftleft.fuzzyc2cpg.ast.logical.statements.JumpStatement;
import io.shiftleft.fuzzyc2cpg.ast.logical.statements.Label;
import io.shiftleft.fuzzyc2cpg.ast.logical.statements.Statement;
import io.shiftleft.fuzzyc2cpg.ast.statements.ExpressionHolder;
import io.shiftleft.fuzzyc2cpg.ast.statements.ExpressionHolderStatement;
import io.shiftleft.fuzzyc2cpg.ast.statements.ExpressionStatement;
import io.shiftleft.fuzzyc2cpg.ast.statements.IdentifierDeclStatement;
import io.shiftleft.fuzzyc2cpg.ast.statements.blockstarters.CatchList;
import io.shiftleft.fuzzyc2cpg.ast.statements.blockstarters.CatchStatement;
import io.shiftleft.fuzzyc2cpg.ast.statements.blockstarters.DoStatement;
import io.shiftleft.fuzzyc2cpg.ast.statements.blockstarters.ForStatement;
import io.shiftleft.fuzzyc2cpg.ast.statements.blockstarters.IfStatementBase;
import io.shiftleft.fuzzyc2cpg.ast.statements.blockstarters.NamespaceStatement;
import io.shiftleft.fuzzyc2cpg.ast.statements.blockstarters.SwitchStatement;
import io.shiftleft.fuzzyc2cpg.ast.statements.blockstarters.TryStatement;
import io.shiftleft.fuzzyc2cpg.ast.statements.blockstarters.WhileStatement;
import io.shiftleft.fuzzyc2cpg.ast.statements.jump.BreakStatement;
import io.shiftleft.fuzzyc2cpg.ast.statements.jump.ContinueStatement;
import io.shiftleft.fuzzyc2cpg.ast.statements.jump.GotoStatement;
import io.shiftleft.fuzzyc2cpg.ast.statements.jump.ReturnStatement;
import io.shiftleft.fuzzyc2cpg.ast.statements.jump.ThrowStatement;

public interface ASTNodeVisitor {

  default void visit(ClassDefStatement statement) {
    visit((Statement)statement);
  }

  default void visit(IdentifierDecl identifierDecl) {
    visit((AstNode)identifierDecl);
  }

  default void visit(IdentifierDeclType identifierDeclType) {
    visit((AstNode)identifierDeclType);
  }

  default void visit(AdditiveExpression expression) {
    visit((BinaryOperationExpression)expression);
  }

  default void visit(AndExpression expression) {
    visit((BinaryOperationExpression)expression);
  }

  default void visit(Argument argument) {
    visit((ExpressionHolder)argument);
  }

  default void visit(ArgumentList argumentList) {
    visit((ExpressionHolder)argumentList);
  }

  default void visit(ArrayIndexing arrayIndexing) {
    visit((Expression)arrayIndexing);
  }

  default void visit(AssignmentExpression expression) {
    visit((BinaryExpression)expression);
  }

  default void visit(BinaryExpression expression) {
    visit((Expression)expression);
  }

  default void visit(BinaryOperationExpression expression) {
    visit((BinaryExpression)expression);
  }

  default void visit(BitAndExpression expression) {
    visit((BinaryOperationExpression)expression);
  }

  default void visit(Callee expression) {
    visit((ExpressionHolder)expression);
  }

  default void visit(CallExpressionBase expression) {
    visit((PostfixExpression)expression);
  }

  default void visit(CastExpression expression) {
    visit((Expression)expression);
  }

  default void visit(CastTarget expression) {
    visit((Expression)expression);
  }

  default void visit(ClassConstantExpression expression) {
    visit((MemberAccess)expression);
  }

  default void visit(Condition expression) {
    visit((ExpressionHolder)expression);
  }

  default void visit(ConditionalExpression expression) {
    visit((Expression)expression);
  }

  default void visit(Constant expression) {
    visit((Expression)expression);
  }

  default void visit(DoubleExpression expression) {
    visit((PrimaryExpression) expression);
  }

  default void visit(EqualityExpression expression) {
    visit((BinaryOperationExpression)expression);
  }

  default void visit(ExclusiveOrExpression expression) {
    visit((BinaryOperationExpression)expression);
  }

  default void visit(Expression expression) {
    visit((AstNode)expression);
  }

  default void visit(ExpressionList expression) {
    visit((Expression)expression);
  }

  default void visit(ForInit expression) {
    visit((Expression)expression);
  }

  default void visit(Identifier expression) {
    visit((Expression)expression);
  }

  default void visit(IdentifierList expression) {
    visit((AstNode)expression);
  }

  default void visit(IncDec expression) {
    visit((Expression)expression);
  }

  default void visit(InclusiveOrExpression expression) {
    visit((BinaryOperationExpression)expression);
  }

  default void visit(InitializerList expression) {
    visit((ExpressionHolder)expression);
  }

  default void visit(InstanceofExpression expression) {
    visit((Expression)expression);
  }

  default void visit(IntegerExpression expression) {
    visit((PrimaryExpression)expression);
  }

  default void visit(MemberAccess expression) {
    visit((PostfixExpression)expression);
  }

  default void visit(MultiplicativeExpression expression) {
    visit((BinaryOperationExpression)expression);
  }

  default void visit(NewExpression expression) {
    visit((CallExpressionBase)expression);
  }

  default void visit(DeleteExpression expression) {
    visit((CallExpressionBase) expression);
  }

  default void visit(OrExpression expression) {
    visit((BinaryOperationExpression)expression);
  }

  default void visit(PostfixExpression expression) {
    visit((Expression)expression);
  }

  default void visit(PostIncDecOperationExpression expression) {
    visit((PostfixExpression)expression);
  }

  default void visit(PrimaryExpression expression) {
    visit((Expression)expression);
  }

  default void visit(PropertyExpression expression) {
    visit((MemberAccess)expression);
  }

  default void visit(PtrMemberAccess expression) {
    visit((PostfixExpression)expression);
  }

  default void visit(RelationalExpression expression) {
    visit((BinaryOperationExpression)expression);
  }

  default void visit(ShiftExpression expression) {
    visit((BinaryOperationExpression)expression);
  }

  default void visit(Sizeof expression) {
    visit((Expression)expression);
  }

  default void visit(SizeofOperand expression) {
    visit((Expression)expression);
  }

  default void visit(StaticPropertyExpression expression) {
    visit((MemberAccess)expression);
  }

  default void visit(StringExpression expression) {
    visit((PrimaryExpression)expression);
  }

  default void visit(UnaryExpression expression) {
    visit((Expression)expression);
  }

  default void visit(UnaryOperationExpression expression) {
    visit((UnaryExpression)expression);
  }

  default void visit(UnaryOperator expression) {
    visit((Expression)expression);
  }

  default void visit(Variable expression) {
    visit((Expression)expression);
  }

  default void visit(FunctionDefBase functionDefBase) {
    visit((AstNode)functionDefBase);
  }

  default void visit(ParameterBase parameterBase) {
    visit((AstNode)parameterBase);
  }

  default void visit(ParameterList parameterList) {
    visit((AstNode)parameterList);
  }

  default void visit(ReturnType returnType) {
    visit((AstNode)returnType);
  }

  default void visit(CallExpression expression) {
    visit((CallExpressionBase)expression);
  }

  default void visit(SizeofExpression expression) {
    visit((Expression)expression);
  }

  default void visit(FunctionDef functionDef) {
    visit((FunctionDefBase)functionDef);
  }

  default void visit(Parameter parameter) {
    visit((ParameterBase)parameter);
  }

  default void visit(ParameterType parameterType) {
    visit((AstNode)parameterType);
  }

  default void visit(TemplateBase templateBase) {
    visit((AstNode) templateBase);
  }

  default void visit(Template template) {
    visit((TemplateBase) template);
  }

  default void visit(TemplateTypeName templateTypeName) {
    visit((AstNode) templateTypeName);
  }

  default void visit(TemplateParameterList templateParameterList) {
    visit((AstNode) templateParameterList);
  }

  default void visit(ElseStatement statement) {
    visit((BlockStarterWithStmtAndCnd)statement);
  }

  default void visit(IfStatement statement) {
    visit((IfStatementBase)statement);
  }

  default void visit(BlockCloser statement) {
    visit((Statement)statement);
  }

  default void visit(BlockStarter statement) {
    visit((Statement)statement);
  }

  default void visit(BlockStarterWithStmtAndCnd statement) {
    visit((BlockStarter)statement);
  }

  default void visit(BreakOrContinueStatement statement) {
    visit((JumpStatement)statement);
  }

  default void visit(CompoundStatement statement) {
    visit((Statement)statement);
  }

  default void visit(JumpStatement statement) {
    visit((Statement)statement);
  }

  default void visit(Label statement) {
    visit((Statement)statement);
  }

  default void visit(Statement statement) {
    visit((AstNode)statement);
  }

  default void visit(CatchList statement) {
    visit((AstNode)statement);
  }

  default void visit(CatchStatement statement) {
    visit((BlockStarterWithStmtAndCnd)statement);
  }

  default void visit(DoStatement statement) {
    visit((BlockStarterWithStmtAndCnd)statement);
  }

  default void visit(ForStatement statement) {
    visit((BlockStarter)statement);
  }

  default void visit(IfStatementBase statement) {
    visit((BlockStarterWithStmtAndCnd)statement);
  }

  default void visit(NamespaceStatement statement) {
    visit((BlockStarter)statement);
  }

  default void visit(SwitchStatement statement) {
    visit((BlockStarterWithStmtAndCnd)statement);
  }

  default void visit(TryStatement statement) {
    visit((BlockStarter)statement);
  }

  default void visit(WhileStatement statement) {
    visit((BlockStarterWithStmtAndCnd)statement);
  }

  default void visit(BreakStatement statement) {
    visit((BreakOrContinueStatement)statement);
  }

  default void visit(ContinueStatement statement) {
    visit((BreakOrContinueStatement)statement);
  }

  default void visit(GotoStatement statement) {
    visit((JumpStatement)statement);
  }

  default void visit(ReturnStatement statement) {
    visit((JumpStatement)statement);
  }

  default void visit(ThrowStatement statement) {
    visit((JumpStatement)statement);
  }

  default void visit(ExpressionHolder expression) {
    visit((Expression)expression);
  }

  default void visit(ExpressionHolderStatement statement) {
    visit((Statement)statement);
  }

  default void visit(ExpressionStatement statement) {
    visit((ExpressionHolderStatement)statement);
  }

  default void visit(IdentifierDeclStatement statement) {
    visit((Statement)statement);
  }

  default void visit(AstNode node) {
    throw new RuntimeException("Unhandled node type: " + node.getClass());
  }

}
