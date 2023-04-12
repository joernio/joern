package io.joern.rubysrc2cpg.astcreation

import io.joern.rubysrc2cpg.parser.{RubyLexer, RubyParser, RubyParserVisitor}
import org.antlr.v4.runtime.tree.{ErrorNode, ParseTree, ParseTreeListener, ParseTreeWalker, RuleNode, TerminalNode}
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream, ParserRuleContext}
import io.joern.x2cpg.Ast.storeInDiffGraph
import io.joern.x2cpg.datastructures.{Global, Scope}
import io.joern.x2cpg.utils.AstPropertiesUtil.RootProperties
import io.joern.x2cpg.{Ast, AstCreatorBase, Defines}
import io.joern.x2cpg.utils.NodeBuilders.{
  fieldIdentifierNode,
  identifierNode,
  methodReturnNode,
  modifierNode,
  operatorCallNode
}
import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes.Call.PropertyDefaults
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.passes.IntervalKeyPool
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate

class AstCreator(filename: String, global: Global) extends AstCreatorBase(filename) {

  class RubyVisitor extends RubyParserVisitor[Any] {

    /** Visit a parse tree produced by {@link RubyParser# program}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitProgram(ctx: RubyParser.ProgramContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# compoundStatement}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitCompoundStatement(ctx: RubyParser.CompoundStatementContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# separators}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitSeparators(ctx: RubyParser.SeparatorsContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# separator}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitSeparator(ctx: RubyParser.SeparatorContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# statements}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitStatements(ctx: RubyParser.StatementsContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by the {@code endStatement} labeled alternative in {@link RubyParser# statement}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitEndStatement(ctx: RubyParser.EndStatementContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by the {@code beginStatement} labeled alternative in {@link RubyParser# statement}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitBeginStatement(ctx: RubyParser.BeginStatementContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by the {@code modifierStatement} labeled alternative in {@link RubyParser#
      * statement}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitModifierStatement(ctx: RubyParser.ModifierStatementContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by the {@code aliasStatement} labeled alternative in {@link RubyParser# statement}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitAliasStatement(ctx: RubyParser.AliasStatementContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by the {@code undefStatement} labeled alternative in {@link RubyParser# statement}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitUndefStatement(ctx: RubyParser.UndefStatementContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by the {@code expressionOrCommandStatement} labeled alternative in {@link
      * RubyParser# statement}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitExpressionOrCommandStatement(ctx: RubyParser.ExpressionOrCommandStatementContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by the {@code expressionExpressionOrCommand} labeled alternative in {@link
      * RubyParser# expressionOrCommand}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitExpressionExpressionOrCommand(ctx: RubyParser.ExpressionExpressionOrCommandContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by the {@code notExpressionOrCommand} labeled alternative in {@link RubyParser#
      * expressionOrCommand}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitNotExpressionOrCommand(ctx: RubyParser.NotExpressionOrCommandContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by the {@code invocationExpressionOrCommand} labeled alternative in {@link
      * RubyParser# expressionOrCommand}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitInvocationExpressionOrCommand(ctx: RubyParser.InvocationExpressionOrCommandContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by the {@code orAndExpressionOrCommand} labeled alternative in {@link RubyParser#
      * expressionOrCommand}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitOrAndExpressionOrCommand(ctx: RubyParser.OrAndExpressionOrCommandContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by the {@code primaryExpression} labeled alternative in {@link RubyParser#
      * expression}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitPrimaryExpression(ctx: RubyParser.PrimaryExpressionContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by the {@code operatorAndExpression} labeled alternative in {@link RubyParser#
      * expression}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitOperatorAndExpression(ctx: RubyParser.OperatorAndExpressionContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by the {@code additiveExpression} labeled alternative in {@link RubyParser#
      * expression}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitAdditiveExpression(ctx: RubyParser.AdditiveExpressionContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by the {@code relationalExpression} labeled alternative in {@link RubyParser#
      * expression}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitRelationalExpression(ctx: RubyParser.RelationalExpressionContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by the {@code singleAssignmentExpression} labeled alternative in {@link RubyParser#
      * expression}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitSingleAssignmentExpression(ctx: RubyParser.SingleAssignmentExpressionContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by the {@code operatorOrExpression} labeled alternative in {@link RubyParser#
      * expression}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitOperatorOrExpression(ctx: RubyParser.OperatorOrExpressionContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by the {@code multiplicativeExpression} labeled alternative in {@link RubyParser#
      * expression}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitMultiplicativeExpression(ctx: RubyParser.MultiplicativeExpressionContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by the {@code bitwiseShiftExpression} labeled alternative in {@link RubyParser#
      * expression}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitBitwiseShiftExpression(ctx: RubyParser.BitwiseShiftExpressionContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by the {@code conditionalOperatorExpression} labeled alternative in {@link
      * RubyParser# expression}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitConditionalOperatorExpression(ctx: RubyParser.ConditionalOperatorExpressionContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by the {@code unaryMinusExpression} labeled alternative in {@link RubyParser#
      * expression}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitUnaryMinusExpression(ctx: RubyParser.UnaryMinusExpressionContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by the {@code powerExpression} labeled alternative in {@link RubyParser#
      * expression}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitPowerExpression(ctx: RubyParser.PowerExpressionContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by the {@code bitwiseOrExpression} labeled alternative in {@link RubyParser#
      * expression}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitBitwiseOrExpression(ctx: RubyParser.BitwiseOrExpressionContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by the {@code isDefinedExpression} labeled alternative in {@link RubyParser#
      * expression}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitIsDefinedExpression(ctx: RubyParser.IsDefinedExpressionContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by the {@code bitwiseAndExpression} labeled alternative in {@link RubyParser#
      * expression}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitBitwiseAndExpression(ctx: RubyParser.BitwiseAndExpressionContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by the {@code equalityExpression} labeled alternative in {@link RubyParser#
      * expression}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitEqualityExpression(ctx: RubyParser.EqualityExpressionContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by the {@code rangeExpression} labeled alternative in {@link RubyParser#
      * expression}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitRangeExpression(ctx: RubyParser.RangeExpressionContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by the {@code unaryExpression} labeled alternative in {@link RubyParser#
      * expression}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitUnaryExpression(ctx: RubyParser.UnaryExpressionContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by the {@code multipleAssignmentExpression} labeled alternative in {@link
      * RubyParser# expression}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitMultipleAssignmentExpression(ctx: RubyParser.MultipleAssignmentExpressionContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by the {@code caseExpressionPrimary} labeled alternative in {@link RubyParser#
      * primary}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitCaseExpressionPrimary(ctx: RubyParser.CaseExpressionPrimaryContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by the {@code beginExpressionPrimary} labeled alternative in {@link RubyParser#
      * primary}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitBeginExpressionPrimary(ctx: RubyParser.BeginExpressionPrimaryContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by the {@code unlessExpressionPrimary} labeled alternative in {@link RubyParser#
      * primary}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitUnlessExpressionPrimary(ctx: RubyParser.UnlessExpressionPrimaryContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by the {@code ifExpressionPrimary} labeled alternative in {@link RubyParser#
      * primary}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitIfExpressionPrimary(ctx: RubyParser.IfExpressionPrimaryContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by the {@code chainedScopedConstantReferencePrimary} labeled alternative in {@link
      * RubyParser# primary}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitChainedScopedConstantReferencePrimary(
      ctx: RubyParser.ChainedScopedConstantReferencePrimaryContext
    ): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by the {@code literalPrimary} labeled alternative in {@link RubyParser# primary}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitLiteralPrimary(ctx: RubyParser.LiteralPrimaryContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by the {@code isDefinedPrimary} labeled alternative in {@link RubyParser# primary}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitIsDefinedPrimary(ctx: RubyParser.IsDefinedPrimaryContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by the {@code untilExpressionPrimary} labeled alternative in {@link RubyParser#
      * primary}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitUntilExpressionPrimary(ctx: RubyParser.UntilExpressionPrimaryContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by the {@code classDefinitionPrimary} labeled alternative in {@link RubyParser#
      * primary}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitClassDefinitionPrimary(ctx: RubyParser.ClassDefinitionPrimaryContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by the {@code indexingExpressionPrimary} labeled alternative in {@link RubyParser#
      * primary}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitIndexingExpressionPrimary(ctx: RubyParser.IndexingExpressionPrimaryContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by the {@code groupingExpressionPrimary} labeled alternative in {@link RubyParser#
      * primary}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitGroupingExpressionPrimary(ctx: RubyParser.GroupingExpressionPrimaryContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by the {@code invocationWithBlockOnlyPrimary} labeled alternative in {@link
      * RubyParser# primary}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitInvocationWithBlockOnlyPrimary(ctx: RubyParser.InvocationWithBlockOnlyPrimaryContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by the {@code yieldWithOptionalArgumentPrimary} labeled alternative in {@link
      * RubyParser# primary}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitYieldWithOptionalArgumentPrimary(ctx: RubyParser.YieldWithOptionalArgumentPrimaryContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by the {@code whileExpressionPrimary} labeled alternative in {@link RubyParser#
      * primary}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitWhileExpressionPrimary(ctx: RubyParser.WhileExpressionPrimaryContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by the {@code simpleScopedConstantReferencePrimary} labeled alternative in {@link
      * RubyParser# primary}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitSimpleScopedConstantReferencePrimary(
      ctx: RubyParser.SimpleScopedConstantReferencePrimaryContext
    ): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by the {@code invocationWithParenthesesPrimary} labeled alternative in {@link
      * RubyParser# primary}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitInvocationWithParenthesesPrimary(ctx: RubyParser.InvocationWithParenthesesPrimaryContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by the {@code chainedInvocationPrimary} labeled alternative in {@link RubyParser#
      * primary}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitChainedInvocationPrimary(ctx: RubyParser.ChainedInvocationPrimaryContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by the {@code methodDefinitionPrimary} labeled alternative in {@link RubyParser#
      * primary}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitMethodDefinitionPrimary(ctx: RubyParser.MethodDefinitionPrimaryContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by the {@code hashConstructorPrimary} labeled alternative in {@link RubyParser#
      * primary}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitHashConstructorPrimary(ctx: RubyParser.HashConstructorPrimaryContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by the {@code chainedInvocationWithoutArgumentsPrimary} labeled alternative in
      * {@link RubyParser# primary}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitChainedInvocationWithoutArgumentsPrimary(
      ctx: RubyParser.ChainedInvocationWithoutArgumentsPrimaryContext
    ): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by the {@code arrayConstructorPrimary} labeled alternative in {@link RubyParser#
      * primary}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitArrayConstructorPrimary(ctx: RubyParser.ArrayConstructorPrimaryContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by the {@code jumpExpressionPrimary} labeled alternative in {@link RubyParser#
      * primary}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitJumpExpressionPrimary(ctx: RubyParser.JumpExpressionPrimaryContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by the {@code superExpressionPrimary} labeled alternative in {@link RubyParser#
      * primary}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitSuperExpressionPrimary(ctx: RubyParser.SuperExpressionPrimaryContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by the {@code variableReferencePrimary} labeled alternative in {@link RubyParser#
      * primary}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitVariableReferencePrimary(ctx: RubyParser.VariableReferencePrimaryContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by the {@code forExpressionPrimary} labeled alternative in {@link RubyParser#
      * primary}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitForExpressionPrimary(ctx: RubyParser.ForExpressionPrimaryContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by the {@code methodOnlyIdentifierPrimary} labeled alternative in {@link RubyParser#
      * primary}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitMethodOnlyIdentifierPrimary(ctx: RubyParser.MethodOnlyIdentifierPrimaryContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by the {@code moduleDefinitionPrimary} labeled alternative in {@link RubyParser#
      * primary}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitModuleDefinitionPrimary(ctx: RubyParser.ModuleDefinitionPrimaryContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by {@link RubyParser# singleLeftHandSide}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitSingleLeftHandSide(ctx: RubyParser.SingleLeftHandSideContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# multipleLeftHandSide}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitMultipleLeftHandSide(ctx: RubyParser.MultipleLeftHandSideContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by {@link RubyParser# multipleLeftHandSideItem}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitMultipleLeftHandSideItem(ctx: RubyParser.MultipleLeftHandSideItemContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by {@link RubyParser# packingLeftHandSide}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitPackingLeftHandSide(ctx: RubyParser.PackingLeftHandSideContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# groupedLeftHandSide}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitGroupedLeftHandSide(ctx: RubyParser.GroupedLeftHandSideContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# multipleRightHandSide}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitMultipleRightHandSide(ctx: RubyParser.MultipleRightHandSideContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by {@link RubyParser# expressionOrCommands}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitExpressionOrCommands(ctx: RubyParser.ExpressionOrCommandsContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by {@link RubyParser# invocationWithoutParentheses}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitInvocationWithoutParentheses(ctx: RubyParser.InvocationWithoutParenthesesContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by {@link RubyParser# command}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitCommand(ctx: RubyParser.CommandContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# chainedCommandWithDoBlock}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitChainedCommandWithDoBlock(ctx: RubyParser.ChainedCommandWithDoBlockContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by {@link RubyParser# commandWithDoBlock}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitCommandWithDoBlock(ctx: RubyParser.CommandWithDoBlockContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# argumentsWithoutParentheses}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitArgumentsWithoutParentheses(ctx: RubyParser.ArgumentsWithoutParenthesesContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by {@link RubyParser# arguments}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitArguments(ctx: RubyParser.ArgumentsContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# blockArgument}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitBlockArgument(ctx: RubyParser.BlockArgumentContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# splattingArgument}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitSplattingArgument(ctx: RubyParser.SplattingArgumentContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# indexingArguments}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitIndexingArguments(ctx: RubyParser.IndexingArgumentsContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# argumentsWithParentheses}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitArgumentsWithParentheses(ctx: RubyParser.ArgumentsWithParenthesesContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by {@link RubyParser# expressions}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitExpressions(ctx: RubyParser.ExpressionsContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# block}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitBlock(ctx: RubyParser.BlockContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# braceBlock}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitBraceBlock(ctx: RubyParser.BraceBlockContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# doBlock}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitDoBlock(ctx: RubyParser.DoBlockContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# blockParameter}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitBlockParameter(ctx: RubyParser.BlockParameterContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# blockParameters}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitBlockParameters(ctx: RubyParser.BlockParametersContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# arrayConstructor}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitArrayConstructor(ctx: RubyParser.ArrayConstructorContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# hashConstructor}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitHashConstructor(ctx: RubyParser.HashConstructorContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# associations}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitAssociations(ctx: RubyParser.AssociationsContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# association}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitAssociation(ctx: RubyParser.AssociationContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# methodDefinition}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitMethodDefinition(ctx: RubyParser.MethodDefinitionContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# singletonObject}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitSingletonObject(ctx: RubyParser.SingletonObjectContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# definedMethodName}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitDefinedMethodName(ctx: RubyParser.DefinedMethodNameContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# assignmentLikeMethodIdentifier}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitAssignmentLikeMethodIdentifier(ctx: RubyParser.AssignmentLikeMethodIdentifierContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by {@link RubyParser# methodName}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitMethodName(ctx: RubyParser.MethodNameContext): Any = {
      println(s"Method name: $ctx.toString()")
    }

    /** Visit a parse tree produced by {@link RubyParser# methodIdentifier}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitMethodIdentifier(ctx: RubyParser.MethodIdentifierContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# methodOnlyIdentifier}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitMethodOnlyIdentifier(ctx: RubyParser.MethodOnlyIdentifierContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by {@link RubyParser# methodParameterPart}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitMethodParameterPart(ctx: RubyParser.MethodParameterPartContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# parameters}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitParameters(ctx: RubyParser.ParametersContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# mandatoryParameters}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitMandatoryParameters(ctx: RubyParser.MandatoryParametersContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# optionalParameters}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitOptionalParameters(ctx: RubyParser.OptionalParametersContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# optionalParameter}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitOptionalParameter(ctx: RubyParser.OptionalParameterContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# arrayParameter}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitArrayParameter(ctx: RubyParser.ArrayParameterContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# procParameter}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitProcParameter(ctx: RubyParser.ProcParameterContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# ifExpression}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitIfExpression(ctx: RubyParser.IfExpressionContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# thenClause}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitThenClause(ctx: RubyParser.ThenClauseContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# elsifClause}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitElsifClause(ctx: RubyParser.ElsifClauseContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# elseClause}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitElseClause(ctx: RubyParser.ElseClauseContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# unlessExpression}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitUnlessExpression(ctx: RubyParser.UnlessExpressionContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# caseExpression}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitCaseExpression(ctx: RubyParser.CaseExpressionContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# whenClause}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitWhenClause(ctx: RubyParser.WhenClauseContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# whenArgument}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitWhenArgument(ctx: RubyParser.WhenArgumentContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# whileExpression}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitWhileExpression(ctx: RubyParser.WhileExpressionContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# doClause}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitDoClause(ctx: RubyParser.DoClauseContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# untilExpression}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitUntilExpression(ctx: RubyParser.UntilExpressionContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# forExpression}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitForExpression(ctx: RubyParser.ForExpressionContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# forVariable}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitForVariable(ctx: RubyParser.ForVariableContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# beginExpression}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitBeginExpression(ctx: RubyParser.BeginExpressionContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# bodyStatement}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitBodyStatement(ctx: RubyParser.BodyStatementContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# rescueClause}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitRescueClause(ctx: RubyParser.RescueClauseContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# exceptionClass}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitExceptionClass(ctx: RubyParser.ExceptionClassContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# exceptionVariableAssignment}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitExceptionVariableAssignment(ctx: RubyParser.ExceptionVariableAssignmentContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by {@link RubyParser# ensureClause}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitEnsureClause(ctx: RubyParser.EnsureClauseContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# classDefinition}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitClassDefinition(ctx: RubyParser.ClassDefinitionContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# classOrModuleReference}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitClassOrModuleReference(ctx: RubyParser.ClassOrModuleReferenceContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by {@link RubyParser# moduleDefinition}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitModuleDefinition(ctx: RubyParser.ModuleDefinitionContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# yieldWithOptionalArgument}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitYieldWithOptionalArgument(ctx: RubyParser.YieldWithOptionalArgumentContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by {@link RubyParser# jumpExpression}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitJumpExpression(ctx: RubyParser.JumpExpressionContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# variableReference}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitVariableReference(ctx: RubyParser.VariableReferenceContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# variableIdentifier}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitVariableIdentifier(ctx: RubyParser.VariableIdentifierContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# pseudoVariableIdentifier}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitPseudoVariableIdentifier(ctx: RubyParser.PseudoVariableIdentifierContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by {@link RubyParser# scopedConstantReference}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitScopedConstantReference(ctx: RubyParser.ScopedConstantReferenceContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by {@link RubyParser# literal}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitLiteral(ctx: RubyParser.LiteralContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# symbol}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitSymbol(ctx: RubyParser.SymbolContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by {@link RubyParser# numericLiteral}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitNumericLiteral(ctx: RubyParser.NumericLiteralContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# unsignedNumericLiteral}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitUnsignedNumericLiteral(ctx: RubyParser.UnsignedNumericLiteralContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by {@link RubyParser# definedMethodNameOrSymbol}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitDefinedMethodNameOrSymbol(ctx: RubyParser.DefinedMethodNameOrSymbolContext): Any = {
      println(ctx.toString())
    }

    /** Visit a parse tree produced by {@link RubyParser# wsOrNl}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitWsOrNl(ctx: RubyParser.WsOrNlContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# keyword}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitKeyword(ctx: RubyParser.KeywordContext): Any = { println(ctx.toString()) }

    /** Visit a parse tree produced by {@link RubyParser# operatorMethodName}.
      *
      * @param ctx
      *   the parse tree
      * @return
      *   the visitor result
      */
    override def visitOperatorMethodName(ctx: RubyParser.OperatorMethodNameContext): Any = { println(ctx.toString()) }

    override def visit(tree: ParseTree): Any = { println(tree.toString()) }

    override def visitChildren(node: RuleNode): Any = { println(node.toString()) }

    override def visitTerminal(node: TerminalNode): Any = { println(node.toString()) }

    override def visitErrorNode(node: ErrorNode): Any = { println(node.toString()) }
  }
  override def createAst(): BatchedUpdate.DiffGraphBuilder = {
    val charStream  = CharStreams.fromFileName(filename)
    val lexer       = new RubyLexer(charStream)
    val tokenStream = new CommonTokenStream(lexer)
    val parser      = new RubyParser(tokenStream)
    val visitor     = new RubyVisitor()
    /*
    val tree        = parser.program() // see other contexts
    visitor.visit(tree)
    val symbols = parser.symbol()
    visitor.visitSymbol(symbols)

    val numericLiterals = parser.numericLiteral()
    visitor.visitNumericLiteral(numericLiterals)
    */

    val methods = parser.methodName()
    visitor.visitMethodName(methods)


      // storeInDiffGraph(ast, diffGraph)
    diffGraph
  }

  private def registerType(typ: String): String = {
    global.usedTypes.putIfAbsent(typ, true)
    typ
  }
}
