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
  override def createAst(): BatchedUpdate.DiffGraphBuilder = {
    val charStream  = CharStreams.fromFileName(filename)
    val lexer       = new RubyLexer(charStream)
    val tokenStream = new CommonTokenStream(lexer)
    val parser      = new RubyParser(tokenStream)
    val programCtx  = parser.program()

    val statementCtx = programCtx.compoundStatement().statements()
    astForStatement(statementCtx)

    /*
    val treeWalker = new ParseTreeWalker()
    val listener = new ParseTreeListener() {
      override def visitTerminal(node: TerminalNode): Unit = {
        println(s"Terminal node: ${node.getText}")
      }

      override def visitErrorNode(node: ErrorNode): Unit = {
        println(s"Error node: ${node.getText}")
      }

      override def enterEveryRule(ctx: ParserRuleContext): Unit = {
        println(s"Enter rule: ${ctx.getText}")
      }

      override def exitEveryRule(ctx: ParserRuleContext): Unit = {
        println(s"Exit rule: ${ctx.getText}")
      }
    }
    treeWalker.walk(listener, programCtx)
     */

    // storeInDiffGraph(ast, diffGraph)
    diffGraph
  }

  def getSubClassType(ctx: RubyParser.ExpressionOrCommandStatementContext): String = {
    if (ctx.isInstanceOf[RubyParser.ProgramContext]) "ProgramContext"
    else if (ctx.isInstanceOf[RubyParser.EndStatementContext]) "EndStatementContext"
    else if (ctx.isInstanceOf[RubyParser.BeginStatementContext]) "BeginStatementContext"
    else if (ctx.isInstanceOf[RubyParser.ModifierStatementContext]) "ModifierStatementContext"
    else if (ctx.isInstanceOf[RubyParser.AliasStatementContext]) "AliasStatementContext"
    else if (ctx.isInstanceOf[RubyParser.UndefStatementContext]) "UndefStatementContext"
    else if (ctx.isInstanceOf[RubyParser.ExpressionOrCommandContext]) "ExpressionOrCommandContext"
    else if (ctx.isInstanceOf[RubyParser.ExpressionExpressionOrCommandContext]) "ExpressionExpressionOrCommandContext"
    else if (ctx.isInstanceOf[RubyParser.NotExpressionOrCommandContext]) "NotExpressionOrCommandContext"
    else if (ctx.isInstanceOf[RubyParser.InvocationExpressionOrCommandContext]) "InvocationExpressionOrCommandContext"
    else if (ctx.isInstanceOf[RubyParser.OrAndExpressionOrCommandContext]) "OrAndExpressionOrCommandContext"
    else if (ctx.isInstanceOf[RubyParser.ExpressionContext]) "ExpressionContext"
    else if (ctx.isInstanceOf[RubyParser.PrimaryExpressionContext]) "PrimaryExpressionContext"
    else if (ctx.isInstanceOf[RubyParser.OperatorAndExpressionContext]) "OperatorAndExpressionContext"
    else if (ctx.isInstanceOf[RubyParser.AdditiveExpressionContext]) "AdditiveExpressionContext"
    else if (ctx.isInstanceOf[RubyParser.RelationalExpressionContext]) "RelationalExpressionContext"
    else if (ctx.isInstanceOf[RubyParser.SingleAssignmentExpressionContext]) "SingleAssignmentExpressionContext"
    else if (ctx.isInstanceOf[RubyParser.OperatorOrExpressionContext]) "OperatorOrExpressionContext"
    else if (ctx.isInstanceOf[RubyParser.MultiplicativeExpressionContext]) "MultiplicativeExpressionContext"
    else if (ctx.isInstanceOf[RubyParser.BitwiseShiftExpressionContext]) "BitwiseShiftExpressionContext"
    else if (ctx.isInstanceOf[RubyParser.ConditionalOperatorExpressionContext]) "ConditionalOperatorExpressionContext"
    else if (ctx.isInstanceOf[RubyParser.UnaryMinusExpressionContext]) "UnaryMinusExpressionContext"
    else if (ctx.isInstanceOf[RubyParser.PowerExpressionContext]) "PowerExpressionContext"
    else if (ctx.isInstanceOf[RubyParser.BitwiseOrExpressionContext]) "BitwiseOrExpressionContext"
    else if (ctx.isInstanceOf[RubyParser.IsDefinedExpressionContext]) "IsDefinedExpressionContext"
    else if (ctx.isInstanceOf[RubyParser.BitwiseAndExpressionContext]) "BitwiseAndExpressionContext"
    else if (ctx.isInstanceOf[RubyParser.EqualityExpressionContext]) "EqualityExpressionContext"
    else if (ctx.isInstanceOf[RubyParser.RangeExpressionContext]) "RangeExpressionContext"
    else if (ctx.isInstanceOf[RubyParser.UnaryExpressionContext]) "UnaryExpressionContext"
    else if (ctx.isInstanceOf[RubyParser.MultipleAssignmentExpressionContext]) "MultipleAssignmentExpressionContext"
    else if (ctx.isInstanceOf[RubyParser.PrimaryContext]) "PrimaryContext"
    else if (ctx.isInstanceOf[RubyParser.CaseExpressionPrimaryContext]) "CaseExpressionPrimaryContext"
    else if (ctx.isInstanceOf[RubyParser.BeginExpressionPrimaryContext]) "BeginExpressionPrimaryContext"
    else if (ctx.isInstanceOf[RubyParser.UnlessExpressionPrimaryContext]) "UnlessExpressionPrimaryContext"
    else if (ctx.isInstanceOf[RubyParser.IfExpressionPrimaryContext]) "IfExpressionPrimaryContext"
    else if (ctx.isInstanceOf[RubyParser.ChainedScopedConstantReferencePrimaryContext])
      "ChainedScopedConstantReferencePrimaryContext"
    else if (ctx.isInstanceOf[RubyParser.LiteralPrimaryContext]) "LiteralPrimaryContext"
    else if (ctx.isInstanceOf[RubyParser.IsDefinedPrimaryContext]) "IsDefinedPrimaryContext"
    else if (ctx.isInstanceOf[RubyParser.UntilExpressionPrimaryContext]) "UntilExpressionPrimaryContext"
    else if (ctx.isInstanceOf[RubyParser.ClassDefinitionPrimaryContext]) "ClassDefinitionPrimaryContext"
    else if (ctx.isInstanceOf[RubyParser.IndexingExpressionPrimaryContext]) "IndexingExpressionPrimaryContext"
    else if (ctx.isInstanceOf[RubyParser.GroupingExpressionPrimaryContext]) "GroupingExpressionPrimaryContext"
    else if (ctx.isInstanceOf[RubyParser.InvocationWithBlockOnlyPrimaryContext]) "InvocationWithBlockOnlyPrimaryContext"
    else if (ctx.isInstanceOf[RubyParser.YieldWithOptionalArgumentPrimaryContext])
      "YieldWithOptionalArgumentPrimaryContext"
    else if (ctx.isInstanceOf[RubyParser.WhileExpressionPrimaryContext]) "WhileExpressionPrimaryContext"
    else if (ctx.isInstanceOf[RubyParser.SimpleScopedConstantReferencePrimaryContext])
      "SimpleScopedConstantReferencePrimaryContext"
    else if (ctx.isInstanceOf[RubyParser.InvocationWithParenthesesPrimaryContext])
      "InvocationWithParenthesesPrimaryContext"
    else if (ctx.isInstanceOf[RubyParser.ChainedInvocationPrimaryContext]) "ChainedInvocationPrimaryContext"
    else if (ctx.isInstanceOf[RubyParser.MethodDefinitionPrimaryContext]) "MethodDefinitionPrimaryContext"
    else if (ctx.isInstanceOf[RubyParser.HashConstructorPrimaryContext]) "HashConstructorPrimaryContext"
    else if (ctx.isInstanceOf[RubyParser.ChainedInvocationWithoutArgumentsPrimaryContext])
      "ChainedInvocationWithoutArgumentsPrimaryContext"
    else if (ctx.isInstanceOf[RubyParser.ArrayConstructorPrimaryContext]) "ArrayConstructorPrimaryContext"
    else if (ctx.isInstanceOf[RubyParser.JumpExpressionPrimaryContext]) "JumpExpressionPrimaryContext"
    else if (ctx.isInstanceOf[RubyParser.SuperExpressionPrimaryContext]) "SuperExpressionPrimaryContext"
    else if (ctx.isInstanceOf[RubyParser.VariableReferencePrimaryContext]) "VariableReferencePrimaryContext"
    else if (ctx.isInstanceOf[RubyParser.ForExpressionPrimaryContext]) "ForExpressionPrimaryContext"
    else if (ctx.isInstanceOf[RubyParser.MethodOnlyIdentifierPrimaryContext]) "MethodOnlyIdentifierPrimaryContext"
    else if (ctx.isInstanceOf[RubyParser.ModuleDefinitionPrimaryContext]) "ModuleDefinitionPrimaryContext"
    else if (ctx.isInstanceOf[RubyParser.SingleLeftHandSideContext]) "SingleLeftHandSideContext"
    else if (ctx.isInstanceOf[RubyParser.MultipleLeftHandSideContext]) "MultipleLeftHandSideContext"
    else if (ctx.isInstanceOf[RubyParser.MultipleLeftHandSideItemContext]) "MultipleLeftHandSideItemContext"
    else if (ctx.isInstanceOf[RubyParser.PackingLeftHandSideContext]) "PackingLeftHandSideContext"
    else if (ctx.isInstanceOf[RubyParser.GroupedLeftHandSideContext]) "GroupedLeftHandSideContext"
    else if (ctx.isInstanceOf[RubyParser.MultipleRightHandSideContext]) "MultipleRightHandSideContext"
    else if (ctx.isInstanceOf[RubyParser.ExpressionOrCommandsContext]) "ExpressionOrCommandsContext"
    else if (ctx.isInstanceOf[RubyParser.InvocationWithoutParenthesesContext]) "InvocationWithoutParenthesesContext"
    else if (ctx.isInstanceOf[RubyParser.CommandContext]) "CommandContext"
    else if (ctx.isInstanceOf[RubyParser.ChainedCommandWithDoBlockContext]) "ChainedCommandWithDoBlockContext"
    else if (ctx.isInstanceOf[RubyParser.CommandWithDoBlockContext]) "CommandWithDoBlockContext"
    else if (ctx.isInstanceOf[RubyParser.ArgumentsWithoutParenthesesContext]) "ArgumentsWithoutParenthesesContext"
    else if (ctx.isInstanceOf[RubyParser.ArgumentsContext]) "ArgumentsContext"
    else if (ctx.isInstanceOf[RubyParser.BlockArgumentContext]) "BlockArgumentContext"
    else if (ctx.isInstanceOf[RubyParser.SplattingArgumentContext]) "SplattingArgumentContext"
    else if (ctx.isInstanceOf[RubyParser.IndexingArgumentsContext]) "IndexingArgumentsContext"
    else if (ctx.isInstanceOf[RubyParser.ArgumentsWithParenthesesContext]) "ArgumentsWithParenthesesContext"
    else if (ctx.isInstanceOf[RubyParser.ExpressionsContext]) "ExpressionsContext"
    else if (ctx.isInstanceOf[RubyParser.BlockContext]) "BlockContext"
    else if (ctx.isInstanceOf[RubyParser.BraceBlockContext]) "BraceBlockContext"
    else if (ctx.isInstanceOf[RubyParser.DoBlockContext]) "DoBlockContext"
    else if (ctx.isInstanceOf[RubyParser.BlockParameterContext]) "BlockParameterContext"
    else if (ctx.isInstanceOf[RubyParser.BlockParametersContext]) "BlockParametersContext"
    else if (ctx.isInstanceOf[RubyParser.ArrayConstructorContext]) "ArrayConstructorContext"
    else if (ctx.isInstanceOf[RubyParser.HashConstructorContext]) "HashConstructorContext"
    else if (ctx.isInstanceOf[RubyParser.AssociationsContext]) "AssociationsContext"
    else if (ctx.isInstanceOf[RubyParser.AssociationContext]) "AssociationContext"
    else if (ctx.isInstanceOf[RubyParser.MethodDefinitionContext]) "MethodDefinitionContext"
    else if (ctx.isInstanceOf[RubyParser.SingletonObjectContext]) "SingletonObjectContext"
    else if (ctx.isInstanceOf[RubyParser.DefinedMethodNameContext]) "DefinedMethodNameContext"
    else if (ctx.isInstanceOf[RubyParser.AssignmentLikeMethodIdentifierContext]) "AssignmentLikeMethodIdentifierContext"
    else if (ctx.isInstanceOf[RubyParser.MethodNameContext]) "MethodNameContext"
    else if (ctx.isInstanceOf[RubyParser.MethodIdentifierContext]) "MethodIdentifierContext"
    else if (ctx.isInstanceOf[RubyParser.MethodOnlyIdentifierContext]) "MethodOnlyIdentifierContext"
    else if (ctx.isInstanceOf[RubyParser.MethodParameterPartContext]) "MethodParameterPartContext"
    else if (ctx.isInstanceOf[RubyParser.ParametersContext]) "ParametersContext"
    else if (ctx.isInstanceOf[RubyParser.MandatoryParametersContext]) "MandatoryParametersContext"
    else if (ctx.isInstanceOf[RubyParser.OptionalParametersContext]) "OptionalParametersContext"
    else if (ctx.isInstanceOf[RubyParser.OptionalParameterContext]) "OptionalParameterContext"
    else if (ctx.isInstanceOf[RubyParser.ArrayParameterContext]) "ArrayParameterContext"
    else if (ctx.isInstanceOf[RubyParser.ProcParameterContext]) "ProcParameterContext"
    else if (ctx.isInstanceOf[RubyParser.IfExpressionContext]) "IfExpressionContext"
    else if (ctx.isInstanceOf[RubyParser.ThenClauseContext]) "ThenClauseContext"
    else if (ctx.isInstanceOf[RubyParser.ElsifClauseContext]) "ElsifClauseContext"
    else if (ctx.isInstanceOf[RubyParser.ElseClauseContext]) "ElseClauseContext"
    else if (ctx.isInstanceOf[RubyParser.UnlessExpressionContext]) "UnlessExpressionContext"
    else if (ctx.isInstanceOf[RubyParser.CaseExpressionContext]) "CaseExpressionContext"
    else if (ctx.isInstanceOf[RubyParser.WhenClauseContext]) "WhenClauseContext"
    else if (ctx.isInstanceOf[RubyParser.WhenArgumentContext]) "WhenArgumentContext"
    else if (ctx.isInstanceOf[RubyParser.WhileExpressionContext]) "WhileExpressionContext"
    else if (ctx.isInstanceOf[RubyParser.DoClauseContext]) "DoClauseContext"
    else if (ctx.isInstanceOf[RubyParser.UntilExpressionContext]) "UntilExpressionContext"
    else if (ctx.isInstanceOf[RubyParser.ForExpressionContext]) "ForExpressionContext"
    else if (ctx.isInstanceOf[RubyParser.ForVariableContext]) "ForVariableContext"
    else if (ctx.isInstanceOf[RubyParser.BeginExpressionContext]) "BeginExpressionContext"
    else if (ctx.isInstanceOf[RubyParser.BodyStatementContext]) "BodyStatementContext"
    else if (ctx.isInstanceOf[RubyParser.RescueClauseContext]) "RescueClauseContext"
    else if (ctx.isInstanceOf[RubyParser.ExceptionClassContext]) "ExceptionClassContext"
    else if (ctx.isInstanceOf[RubyParser.ExceptionVariableAssignmentContext]) "ExceptionVariableAssignmentContext"
    else if (ctx.isInstanceOf[RubyParser.EnsureClauseContext]) "EnsureClauseContext"
    else if (ctx.isInstanceOf[RubyParser.ClassDefinitionContext]) "ClassDefinitionContext"
    else if (ctx.isInstanceOf[RubyParser.ClassOrModuleReferenceContext]) "ClassOrModuleReferenceContext"
    else if (ctx.isInstanceOf[RubyParser.ModuleDefinitionContext]) "ModuleDefinitionContext"
    else if (ctx.isInstanceOf[RubyParser.YieldWithOptionalArgumentContext]) "YieldWithOptionalArgumentContext"
    else if (ctx.isInstanceOf[RubyParser.JumpExpressionContext]) "JumpExpressionContext"
    else if (ctx.isInstanceOf[RubyParser.VariableReferenceContext]) "VariableReferenceContext"
    else if (ctx.isInstanceOf[RubyParser.VariableIdentifierContext]) "VariableIdentifierContext"
    else if (ctx.isInstanceOf[RubyParser.PseudoVariableIdentifierContext]) "PseudoVariableIdentifierContext"
    else if (ctx.isInstanceOf[RubyParser.ScopedConstantReferenceContext]) "ScopedConstantReferenceContext"
    else if (ctx.isInstanceOf[RubyParser.LiteralContext]) "LiteralContext"
    else if (ctx.isInstanceOf[RubyParser.SymbolContext]) "SymbolContext"
    else if (ctx.isInstanceOf[RubyParser.NumericLiteralContext]) "NumericLiteralContext"
    else if (ctx.isInstanceOf[RubyParser.UnsignedNumericLiteralContext]) "UnsignedNumericLiteralContext"
    else if (ctx.isInstanceOf[RubyParser.DefinedMethodNameOrSymbolContext]) "DefinedMethodNameOrSymbolContext"
    else if (ctx.isInstanceOf[RubyParser.WsOrNlContext]) "WsOrNlContext"
    else if (ctx.isInstanceOf[RubyParser.KeywordContext]) "KeywordContext"
    else if (ctx.isInstanceOf[RubyParser.OperatorMethodNameContext]) "OperatorMethodNameContext"
    else if (ctx.isInstanceOf[RubyParser.CompoundStatementContext]) "CompoundStatementContext"
    else if (ctx.isInstanceOf[RubyParser.SeparatorsContext]) "SeparatorsContext"
    else if (ctx.isInstanceOf[RubyParser.SeparatorContext]) "SeparatorContext"
    else if (ctx.isInstanceOf[RubyParser.StatementsContext]) "StatementsContext"
    else if (ctx.isInstanceOf[RubyParser.StatementContext]) "StatementContext"
    else if (ctx.isInstanceOf[RubyParser.ExpressionOrCommandStatementContext]) "ExpressionOrCommandStatementContext"
    else "Indeterminate context"
  }

  def astForExpressionOrCommand(ctx: RubyParser.ExpressionOrCommandStatementContext) = {
    if (ctx.isInstanceOf[RubyParser.InvocationExpressionOrCommandContext]) {
      val ctxSubclass = ctx.asInstanceOf[RubyParser.InvocationExpressionOrCommandContext]
      println(s"RubyParser.InvocationExpressionOrCommandContext ${ctxSubclass.getText}")
    } else if (ctx.isInstanceOf[RubyParser.NotExpressionOrCommandContext]) {
      val ctxSubclass = ctx.asInstanceOf[RubyParser.NotExpressionOrCommandContext]
      println(s"RubyParser.NotExpressionOrCommandContext ${ctxSubclass.getText}")
    } else if (ctx.isInstanceOf[RubyParser.OrAndExpressionOrCommandContext]) {
      val ctxSubclass = ctx.asInstanceOf[RubyParser.OrAndExpressionOrCommandContext]
      println(s"RubyParser.OrAndExpressionOrCommandContext ${ctxSubclass.getText}")
    } else if (ctx.isInstanceOf[RubyParser.ExpressionExpressionOrCommandContext]) {
      val ctxSubclass = ctx.asInstanceOf[RubyParser.ExpressionExpressionOrCommandContext]
      println(s"RubyParser.ExpressionExpressionOrCommandContext ${ctxSubclass.getText}")
    } else {
      println(s"Unhandled context type: ${getSubClassType(ctx)}. Text: ${ctx.getText}")
    }
  }

  def astForStatement(ctx: RubyParser.StatementsContext): Unit = {
    ctx
      .statement()
      .forEach(st => {
        println(
          s"Statement: ${st.getText}. Rule: ${st.getRuleContext().getText}. Alt: ${st.getRuleContext().getAltNumber()}"
        )

        if (st.isInstanceOf[RubyParser.AliasStatementContext]) {
          val ctx = st.asInstanceOf[RubyParser.AliasStatementContext]
          println(s"RubyParser.AliasStatementContext ${ctx.getText}")
        } else if (st.isInstanceOf[RubyParser.UndefStatementContext]) {
          val ctx = st.asInstanceOf[RubyParser.UndefStatementContext]
          println(s"RubyParser.UndefStatementContext ${ctx.getText}")
        } else if (st.isInstanceOf[RubyParser.BeginStatementContext]) {
          val ctx = st.asInstanceOf[RubyParser.BeginStatementContext]
          println(s"RubyParser.BeginStatementContext ${ctx.getText}")
        } else if (st.isInstanceOf[RubyParser.EndStatementContext]) {
          val ctx = st.asInstanceOf[RubyParser.EndStatementContext]
          println(s"RubyParser.EndStatementContext ${ctx.getText}")
        } else if (st.isInstanceOf[RubyParser.ModifierStatementContext]) {
          val ctx = st.asInstanceOf[RubyParser.ModifierStatementContext]
          println(s"RubyParser.ModifierStatementContext ${ctx.getText}")
        } else if (st.isInstanceOf[RubyParser.ExpressionOrCommandStatementContext]) {
          val ctx1 = st.asInstanceOf[RubyParser.ExpressionOrCommandStatementContext]
          println(s"RubyParser.ExpressionOrCommandStatementContext ${ctx1.getText}")
          astForExpressionOrCommand(ctx1)
        }
      })
  }

}
