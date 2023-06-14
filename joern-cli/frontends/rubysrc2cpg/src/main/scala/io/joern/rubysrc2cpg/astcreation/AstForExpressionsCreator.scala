package io.joern.rubysrc2cpg.astcreation

import io.joern.rubysrc2cpg.parser.RubyParser._
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}

import scala.jdk.CollectionConverters.CollectionHasAsScala

trait AstForExpressionsCreator { this: AstCreator =>

  protected def astForPowerExpression(ctx: PowerExpressionContext): Ast = {
    val argsAst = ctx.expression().asScala.flatMap(astForExpressionContext)
    val call =
      callNode(ctx, ctx.getText, Operators.exponentiation, Operators.exponentiation, DispatchTypes.STATIC_DISPATCH)
    callAst(call, argsAst.toList)
  }

  protected def astForOrExpression(ctx: OperatorOrExpressionContext): Ast = {
    val argsAst = ctx.expression().asScala.flatMap(astForExpressionContext)
    val call    = callNode(ctx, ctx.getText, Operators.or, Operators.or, DispatchTypes.STATIC_DISPATCH)
    callAst(call, argsAst.toList)
  }

  protected def astForAndExpression(ctx: OperatorAndExpressionContext): Ast = {
    val argsAst = ctx.expression().asScala.flatMap(astForExpressionContext)
    val call    = callNode(ctx, ctx.getText, Operators.and, Operators.and, DispatchTypes.STATIC_DISPATCH)
    callAst(call, argsAst.toList)
  }

  protected def astForUnaryExpression(ctx: UnaryExpressionContext): Ast = ctx.op.getType match {
    case TILDE => astForUnaryTildeExpression(ctx)
    case PLUS  => astForUnaryPlusExpression(ctx)
    case EMARK => astForUnaryNotExpression(ctx)
  }

  protected def astForUnaryPlusExpression(ctx: UnaryExpressionContext): Ast = {
    val argsAst = astForExpressionContext(ctx.expression())
    val call    = callNode(ctx, ctx.getText, Operators.plus, Operators.plus, DispatchTypes.STATIC_DISPATCH)
    callAst(call, argsAst)
  }

  protected def astForUnaryTildeExpression(ctx: UnaryExpressionContext): Ast = {
    val argsAst = astForExpressionContext(ctx.expression())
    val call    = callNode(ctx, ctx.getText, Operators.not, Operators.not, DispatchTypes.STATIC_DISPATCH)
    callAst(call, argsAst)
  }

  protected def astForUnaryNotExpression(ctx: UnaryExpressionContext): Ast = {
    val argsAst = astForExpressionContext(ctx.expression())
    val call    = callNode(ctx, ctx.getText, Operators.not, Operators.not, DispatchTypes.STATIC_DISPATCH)
    callAst(call, argsAst)
  }

  protected def astForAdditiveExpression(ctx: AdditiveExpressionContext): Ast = ctx.op.getType match {
    case PLUS  => astForAdditivePlusExpression(ctx)
    case MINUS => astForAdditiveMinusExpression(ctx)
  }

  protected def astForAdditivePlusExpression(ctx: AdditiveExpressionContext): Ast = {
    val argsAst = ctx.expression().asScala.flatMap(astForExpressionContext)
    val call    = callNode(ctx, ctx.getText, Operators.addition, Operators.addition, DispatchTypes.STATIC_DISPATCH)
    callAst(call, argsAst.toList)
  }

  protected def astForAdditiveMinusExpression(ctx: AdditiveExpressionContext): Ast = {
    val argsAst = ctx.expression().asScala.flatMap(astForExpressionContext)
    val call = callNode(ctx, ctx.getText, Operators.subtraction, Operators.subtraction, DispatchTypes.STATIC_DISPATCH)
    callAst(call, argsAst.toList)
  }

  protected def astForMultiplicativeExpression(ctx: MultiplicativeExpressionContext): Ast = ctx.op.getType match {
    case STAR    => astForMultiplicativeStarExpression(ctx)
    case SLASH   => astForMultiplicativeSlashExpression(ctx)
    case PERCENT => astForMultiplicativePercentExpression(ctx)
  }

  protected def astForMultiplicativeStarExpression(ctx: MultiplicativeExpressionContext): Ast = {
    val argsAst = ctx.expression().asScala.flatMap(astForExpressionContext)
    val call =
      callNode(ctx, ctx.getText, Operators.multiplication, Operators.multiplication, DispatchTypes.STATIC_DISPATCH)
    callAst(call, argsAst.toList)
  }

  protected def astForMultiplicativeSlashExpression(ctx: MultiplicativeExpressionContext): Ast = {
    val argsAst = ctx.expression().asScala.flatMap(astForExpressionContext)
    val call    = callNode(ctx, ctx.getText, Operators.division, Operators.division, DispatchTypes.STATIC_DISPATCH)
    callAst(call, argsAst.toList)
  }

  protected def astForMultiplicativePercentExpression(ctx: MultiplicativeExpressionContext): Ast = {
    val argsAst = ctx.expression().asScala.flatMap(astForExpressionContext)
    val call    = callNode(ctx, ctx.getText, Operators.modulo, Operators.modulo, DispatchTypes.STATIC_DISPATCH)
    callAst(call, argsAst.toList)
  }

}
