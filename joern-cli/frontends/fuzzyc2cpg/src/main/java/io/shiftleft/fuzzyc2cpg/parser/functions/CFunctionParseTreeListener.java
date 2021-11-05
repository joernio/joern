package io.shiftleft.fuzzyc2cpg.parser.functions;

import io.shiftleft.fuzzyc2cpg.FunctionBaseListener;
import io.shiftleft.fuzzyc2cpg.FunctionParser;
import io.shiftleft.fuzzyc2cpg.parser.functions.builder.FunctionContentBuilder;
import io.shiftleft.fuzzyc2cpg.passes.astcreation.AntlrParserDriver;

/**
 * This is where hooks are registered for different types of parse tree nodes.
 */

public class CFunctionParseTreeListener extends FunctionBaseListener {

  private final AntlrParserDriver p;

  public CFunctionParseTreeListener(AntlrParserDriver aP) {
    p = aP;
  }

  @Override
  public void enterStatements(FunctionParser.StatementsContext ctx) {
    FunctionContentBuilder builder = new FunctionContentBuilder();
    builder.createNew(ctx);
    p.builderStack.push(builder);
  }

  @Override
  public void exitStatements(FunctionParser.StatementsContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.exitStatements(ctx);
  }

  @Override
  public void enterStatement(FunctionParser.StatementContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterStatement(ctx);
  }

  @Override
  public void exitStatement(FunctionParser.StatementContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.exitStatement(ctx);
  }

  @Override
  public void enterElse_statement(FunctionParser.Else_statementContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterElse(ctx);
  }

  @Override
  public void enterIf_statement(FunctionParser.If_statementContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterIf(ctx);
  }

  @Override
  public void enterFor_statement(FunctionParser.For_statementContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterFor(ctx);
  }

  @Override
  public void enterFor_init_statement(
      FunctionParser.For_init_statementContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterInitFor(ctx);
  }

  @Override
  public void exitFor_init_statement(
      FunctionParser.For_init_statementContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.exitInitFor(ctx);
  }

  @Override
  public void enterWhile_statement(FunctionParser.While_statementContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterWhile(ctx);
  }

  @Override
  public void enterDo_statement(FunctionParser.Do_statementContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterDo(ctx);
  }

  @Override
  public void enterSwitch_statement(
      FunctionParser.Switch_statementContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterSwitchStatement(ctx);
  }

  @Override
  public void enterLabel(FunctionParser.LabelContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterLabel(ctx);
  }

  @Override
  public void enterTry_statement(FunctionParser.Try_statementContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterTryStatement(ctx);
  }

  @Override
  public void enterCatch_statement(FunctionParser.Catch_statementContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterCatchStatement(ctx);
  }

  @Override
  public void enterBlock_starter(FunctionParser.Block_starterContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterBlockStarter(ctx);
  }

  @Override
  public void enterOpening_curly(FunctionParser.Opening_curlyContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterOpeningCurly(ctx);
  }

  @Override
  public void enterClosing_curly(FunctionParser.Closing_curlyContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterClosingCurly(ctx);
  }

  @Override
  public void enterExpr_statement(FunctionParser.Expr_statementContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterExprStatement(ctx);
  }

  @Override
  public void enterReturnStatement(FunctionParser.ReturnStatementContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterReturnStatement(ctx);
  }

  @Override
  public void enterBreakStatement(FunctionParser.BreakStatementContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterBreakStatement(ctx);
  }

  @Override
  public void enterContinueStatement(
      FunctionParser.ContinueStatementContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterContinueStatement(ctx);
  }

  @Override
  public void enterGotoStatement(FunctionParser.GotoStatementContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterGotoStatement(ctx);
  }

  @Override
  public void enterDeclByType(FunctionParser.DeclByTypeContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterDeclByType(ctx, ctx.type_name());
  }

  @Override
  public void exitDeclByType(FunctionParser.DeclByTypeContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.exitDeclByType();
  }

  @Override
  public void enterDeclByClass(FunctionParser.DeclByClassContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterDeclByClass(ctx);
  }

  @Override
  public void exitDeclByClass(FunctionParser.DeclByClassContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.exitDeclByClass();
  }

  @Override
  public void enterInitDeclSimple(FunctionParser.InitDeclSimpleContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterInitDeclSimple(ctx);
  }

  @Override
  public void exitInitDeclSimple(FunctionParser.InitDeclSimpleContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.exitInitDeclSimple();
  }

  @Override
  public void enterInitDeclWithAssign(
      FunctionParser.InitDeclWithAssignContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterInitDeclWithAssign(ctx);
  }

  @Override
  public void exitInitDeclWithAssign(
      FunctionParser.InitDeclWithAssignContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.exitInitDeclWithAssign(ctx);
  }

  @Override
  public void enterInitDeclWithCall(
      FunctionParser.InitDeclWithCallContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterInitDeclWithCall(ctx);
  }

  @Override
  public void exitInitDeclWithCall(FunctionParser.InitDeclWithCallContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.exitInitDeclWithCall();
  }

  @Override
  public void enterCondition(FunctionParser.ConditionContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterCondition(ctx);
  }

  @Override
  public void exitCondition(FunctionParser.ConditionContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.exitCondition(ctx);
  }

  @Override
  public void enterExpr(FunctionParser.ExprContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterExpression(ctx);
  }

  @Override
  public void exitExpr(FunctionParser.ExprContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.exitExpression(ctx);
  }

  @Override
  public void enterAssign_expr(FunctionParser.Assign_exprContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterAssignment(ctx);
  }

  @Override
  public void exitAssign_expr(FunctionParser.Assign_exprContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.exitAssignment(ctx);
  }

  @Override
  public void enterCndExpr(FunctionParser.CndExprContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterConditionalExpr(ctx);

  }

  @Override
  public void exitCndExpr(FunctionParser.CndExprContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.exitConditionalExpr(ctx);
  }

  @Override
  public void enterOr_expression(FunctionParser.Or_expressionContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterOrExpression(ctx);
  }

  @Override
  public void exitOr_expression(FunctionParser.Or_expressionContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.exitrOrExpression(ctx);
  }

  @Override
  public void enterAnd_expression(FunctionParser.And_expressionContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterAndExpression(ctx);
  }

  @Override
  public void exitAnd_expression(FunctionParser.And_expressionContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.exitAndExpression(ctx);
  }

  @Override
  public void enterInclusive_or_expression(
      FunctionParser.Inclusive_or_expressionContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterInclusiveOrExpression(ctx);
  }

  @Override
  public void exitInclusive_or_expression(
      FunctionParser.Inclusive_or_expressionContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.exitInclusiveOrExpression(ctx);
  }

  @Override
  public void enterExclusive_or_expression(
      FunctionParser.Exclusive_or_expressionContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterExclusiveOrExpression(ctx);
  }

  @Override
  public void exitExclusive_or_expression(
      FunctionParser.Exclusive_or_expressionContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.exitExclusiveOrExpression(ctx);
  }

  @Override
  public void enterBit_and_expression(
      FunctionParser.Bit_and_expressionContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterBitAndExpression(ctx);
  }

  @Override
  public void exitBit_and_expression(
      FunctionParser.Bit_and_expressionContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.exitBitAndExpression(ctx);
  }

  @Override
  public void enterEquality_expression(
      FunctionParser.Equality_expressionContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterEqualityExpression(ctx);
  }

  @Override
  public void exitEquality_expression(
      FunctionParser.Equality_expressionContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.exitEqualityExpression(ctx);
  }

  @Override
  public void enterRelational_expression(
      FunctionParser.Relational_expressionContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterRelationalExpression(ctx);
  }

  @Override
  public void exitRelational_expression(
      FunctionParser.Relational_expressionContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.exitRelationalExpression(ctx);
  }

  @Override
  public void enterShift_expression(
      FunctionParser.Shift_expressionContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterShiftExpression(ctx);
  }

  @Override
  public void exitShift_expression(FunctionParser.Shift_expressionContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.exitShiftExpression(ctx);
  }

  @Override
  public void enterAdditive_expression(
      FunctionParser.Additive_expressionContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterAdditiveExpression(ctx);
  }

  @Override
  public void exitAdditive_expression(
      FunctionParser.Additive_expressionContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.exitAdditiveExpression(ctx);
  }

  @Override
  public void enterMultiplicative_expression(
      FunctionParser.Multiplicative_expressionContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterMultiplicativeExpression(ctx);
  }

  @Override
  public void exitMultiplicative_expression(
      FunctionParser.Multiplicative_expressionContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.exitMultiplicativeExpression(ctx);
  }

  @Override
  public void enterCast_expression(FunctionParser.Cast_expressionContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterCastExpression(ctx);
  }

  @Override
  public void exitCast_expression(FunctionParser.Cast_expressionContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.exitCastExpression(ctx);
  }

  @Override
  public void enterCast_target(FunctionParser.Cast_targetContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterCast_target(ctx);
  }

  @Override
  public void exitCast_target(FunctionParser.Cast_targetContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.exitCast_target(ctx);
  }

  @Override
  public void enterFuncCall(FunctionParser.FuncCallContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterFuncCall(ctx);
  }

  @Override
  public void exitFuncCall(FunctionParser.FuncCallContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.exitFuncCall(ctx);
  }

  @Override
  public void enterSizeof_expression(FunctionParser.Sizeof_expressionContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterSizeofExpr(ctx);
  }

  @Override
  public void exitSizeof_expression(FunctionParser.Sizeof_expressionContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.exitSizeofExpr(ctx);
  }

  @Override
  public void enterSizeof(FunctionParser.SizeofContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterSizeof(ctx);
  }

  @Override
  public void exitSizeof(FunctionParser.SizeofContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.exitSizeof(ctx);
  }

  @Override
  public void enterUnary_op_and_cast_expr(FunctionParser.Unary_op_and_cast_exprContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterUnaryOpAndCastExpr(ctx);
  }

  @Override
  public void exitUnary_op_and_cast_expr(FunctionParser.Unary_op_and_cast_exprContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.exitUnaryOpAndCastExpr(ctx);
  }

  @Override
  public void enterUnary_operator(FunctionParser.Unary_operatorContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterUnaryOperator(ctx);
  }

  @Override
  public void exitUnary_operator(FunctionParser.Unary_operatorContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.exitUnaryOperator(ctx);
  }

  @Override
  public void enterFunction_argument_list( FunctionParser.Function_argument_listContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterArgumentList(ctx);
  }

  @Override
  public void exitFunction_argument_list( FunctionParser.Function_argument_listContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.exitArgumentList(ctx);
  }

  @Override
  public void enterInc_dec(FunctionParser.Inc_decContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterIncDec(ctx);
  }

  @Override
  public void exitInc_dec(FunctionParser.Inc_decContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.exitIncDec(ctx);
  }

  @Override
  public void enterArrayIndexing(FunctionParser.ArrayIndexingContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterArrayIndexing(ctx);
  }

  @Override
  public void exitArrayIndexing(FunctionParser.ArrayIndexingContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.exitArrayIndexing(ctx);
  }

  @Override
  public void enterMemberAccess(FunctionParser.MemberAccessContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterMemberAccess(ctx);
  }

  @Override
  public void exitMemberAccess(FunctionParser.MemberAccessContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.exitMemberAccess(ctx);
  }

  @Override
  public void enterPtrMemberAccess(FunctionParser.PtrMemberAccessContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterPtrMemberAccess(ctx);
  }

  @Override
  public void exitPtrMemberAccess(FunctionParser.PtrMemberAccessContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.exitPtrMemberAccess(ctx);
  }

  @Override
  public void enterIncDecOp(FunctionParser.IncDecOpContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterIncDecOp(ctx);
  }

  @Override
  public void exitIncDecOp(FunctionParser.IncDecOpContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.exitIncDecOp(ctx);
  }

  @Override
  public void enterPrimary_expression(
      FunctionParser.Primary_expressionContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterPrimary(ctx);
  }

  @Override
  public void exitPrimary_expression(
      FunctionParser.Primary_expressionContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.exitPrimary(ctx);
  }

  @Override
  public void enterUnary_expression(
      FunctionParser.Unary_expressionContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterUnaryExpression(ctx);
  }

  @Override
  public void exitUnary_expression(FunctionParser.Unary_expressionContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.exitUnaryExpression(ctx);
  }

  @Override
  public void enterConstant(FunctionParser.ConstantContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterConstant(ctx);
  }

  @Override
  public void exitConstant(FunctionParser.ConstantContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.exitConstant(ctx);
  }

  @Override
  public void enterIdentifier(FunctionParser.IdentifierContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterIdentifier(ctx);
  }

  @Override
  public void exitIdentifier(FunctionParser.IdentifierContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.exitIdentifier(ctx);
  }

  @Override
  public void enterFunction_argument(
      FunctionParser.Function_argumentContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterArgument(ctx);
  }

  @Override
  public void exitFunction_argument(
      FunctionParser.Function_argumentContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.exitArgument(ctx);
  }

  @Override
  public void enterInitializer_list(
      FunctionParser.Initializer_listContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterInitializerList(ctx);
  }

  @Override
  public void exitInitializer_list(FunctionParser.Initializer_listContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.exitInitializerList(ctx);
  }

  @Override
  public void enterSizeof_operand2(FunctionParser.Sizeof_operand2Context ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterSizeofOperand2(ctx);
  }

  @Override
  public void exitSizeof_operand2(FunctionParser.Sizeof_operand2Context ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.exitSizeofOperand2(ctx);
  }

  @Override
  public void enterSizeof_operand(FunctionParser.Sizeof_operandContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterSizeofOperand(ctx);
  }

  @Override
  public void exitSizeof_operand(FunctionParser.Sizeof_operandContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.exitSizeofOperand(ctx);
  }

  @Override
  public void enterThrowStatement(FunctionParser.ThrowStatementContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack
        .peek();
    builder.enterThrowStatement(ctx);
  }

  @Override
  public void enterNew_expression(FunctionParser.New_expressionContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack.peek();
    builder.enterNewExpr(ctx);
  }

  @Override
  public void exitNew_expression(FunctionParser.New_expressionContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack.peek();
    builder.exitNewExpr(ctx);
  }

  @Override
  public void enterDelete_expression(FunctionParser.Delete_expressionContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack.peek();
    builder.enterDeleteExpr(ctx);
  }

  @Override
  public void exitDelete_expression(FunctionParser.Delete_expressionContext ctx) {
    FunctionContentBuilder builder = (FunctionContentBuilder) p.builderStack.peek();
    builder.exitDeleteExpr(ctx);
  }
}