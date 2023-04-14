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
    diffGraph
  }

  def astForVariable(variableCtx: RubyParser.VariableIdentifierContext): Unit = {
    val varText = if (variableCtx.LOCAL_VARIABLE_IDENTIFIER() != null) {
      variableCtx.LOCAL_VARIABLE_IDENTIFIER().getSymbol.getText
    } else if (variableCtx.GLOBAL_VARIABLE_IDENTIFIER() != null) {
      variableCtx.GLOBAL_VARIABLE_IDENTIFIER().getSymbol.getText
    } else if (variableCtx.INSTANCE_VARIABLE_IDENTIFIER() != null) {
      variableCtx.INSTANCE_VARIABLE_IDENTIFIER().getSymbol.getText
    } else if (variableCtx.CLASS_VARIABLE_IDENTIFIER() != null) {
      variableCtx.CLASS_VARIABLE_IDENTIFIER().getSymbol.getText
    } else if (variableCtx.CONSTANT_IDENTIFIER() != null) {
      variableCtx.CONSTANT_IDENTIFIER().getSymbol.getText
    }
    println(s"Variable text: $varText")
    val node = NewLocal()
    node.name(varText.toString)
    diffGraph.addNode(node)
  }

  def astForSingleLHS(ctx: RubyParser.SingleLeftHandSideContext): Unit = {
    val variableCtx = ctx.variableIdentifier()
    astForVariable(variableCtx)
  }

  def astForExpressionOrCommands(ctx: RubyParser.ExpressionOrCommandsContext): Unit = {
    ctx
      .expressionOrCommand()
      .forEach(ec => {
        astForExpressionOrCommand(ec)
      })
  }

  def astForSplattingArgument(ctx: RubyParser.SplattingArgumentContext): Unit = {
    if (ctx == null) {
      return
    }
    astForExpressionOrCommand(ctx.expressionOrCommand())
  }

  def astForMultipleRHS(ctx: RubyParser.MultipleRightHandSideContext): Unit = {
    astForExpressionOrCommands(ctx.expressionOrCommands())
    astForSplattingArgument(ctx.splattingArgument())
  }

  def astForSingleAssignmentExpression(ctxSubclass: RubyParser.SingleAssignmentExpressionContext): Unit = {
    astForSingleLHS(ctxSubclass.singleLeftHandSide())
    astForMultipleRHS(ctxSubclass.multipleRightHandSide())
  }

  def astForPrimaryExpressionContext(ctx: RubyParser.PrimaryContext): Unit = {
    if (ctx.isInstanceOf[RubyParser.ClassDefinitionPrimaryContext]) {
      val ctxSubclass = ctx.asInstanceOf[RubyParser.ClassDefinitionPrimaryContext]
      println(s"RubyParser.ClassDefinitionPrimaryContext ${ctxSubclass.getText}")
    } else if (ctx.isInstanceOf[RubyParser.ModuleDefinitionPrimaryContext]) {
      val ctxSubclass = ctx.asInstanceOf[RubyParser.ModuleDefinitionPrimaryContext]
      println(s"RubyParser.ModuleDefinitionPrimaryContext ${ctxSubclass.getText}")
    } else if (ctx.isInstanceOf[RubyParser.MethodDefinitionPrimaryContext]) {
      val ctxSubclass = ctx.asInstanceOf[RubyParser.MethodDefinitionPrimaryContext]
      println(s"RubyParser.MethodDefinitionPrimaryContext ${ctxSubclass.getText}")
    } else if (ctx.isInstanceOf[RubyParser.YieldWithOptionalArgumentPrimaryContext]) {
      val ctxSubclass = ctx.asInstanceOf[RubyParser.YieldWithOptionalArgumentPrimaryContext]
      println(s"RubyParser.YieldWithOptionalArgumentPrimaryContext ${ctxSubclass.getText}")
    } else if (ctx.isInstanceOf[RubyParser.IfExpressionPrimaryContext]) {
      val ctxSubclass = ctx.asInstanceOf[RubyParser.IfExpressionPrimaryContext]
      println(s"RubyParser.IfExpressionPrimaryContext ${ctxSubclass.getText}")
    } else if (ctx.isInstanceOf[RubyParser.UnlessExpressionPrimaryContext]) {
      val ctxSubclass = ctx.asInstanceOf[RubyParser.UnlessExpressionPrimaryContext]
      println(s"RubyParser.UnlessExpressionPrimaryContext ${ctxSubclass.getText}")
    } else if (ctx.isInstanceOf[RubyParser.CaseExpressionPrimaryContext]) {
      val ctxSubclass = ctx.asInstanceOf[RubyParser.CaseExpressionPrimaryContext]
      println(s"RubyParser.CaseExpressionPrimaryContext ${ctxSubclass.getText}")
    } else if (ctx.isInstanceOf[RubyParser.WhileExpressionPrimaryContext]) {
      val ctxSubclass = ctx.asInstanceOf[RubyParser.WhileExpressionPrimaryContext]
      println(s"RubyParser.WhileExpressionPrimaryContext ${ctxSubclass.getText}")
    } else if (ctx.isInstanceOf[RubyParser.UntilExpressionPrimaryContext]) {
      val ctxSubclass = ctx.asInstanceOf[RubyParser.UntilExpressionPrimaryContext]
      println(s"RubyParser.UntilExpressionPrimaryContext ${ctxSubclass.getText}")
    } else if (ctx.isInstanceOf[RubyParser.ForExpressionPrimaryContext]) {
      val ctxSubclass = ctx.asInstanceOf[RubyParser.ForExpressionPrimaryContext]
      println(s"RubyParser.ForExpressionPrimaryContext ${ctxSubclass.getText}")
    } else if (ctx.isInstanceOf[RubyParser.JumpExpressionPrimaryContext]) {
      val ctxSubclass = ctx.asInstanceOf[RubyParser.JumpExpressionPrimaryContext]
      println(s"RubyParser.JumpExpressionPrimaryContext ${ctxSubclass.getText}")
    } else if (ctx.isInstanceOf[RubyParser.BeginExpressionPrimaryContext]) {
      val ctxSubclass = ctx.asInstanceOf[RubyParser.BeginExpressionPrimaryContext]
      println(s"RubyParser.BeginExpressionPrimaryContext ${ctxSubclass.getText}")
    } else if (ctx.isInstanceOf[RubyParser.GroupingExpressionPrimaryContext]) {
      val ctxSubclass = ctx.asInstanceOf[RubyParser.GroupingExpressionPrimaryContext]
      println(s"RubyParser.GroupingExpressionPrimaryContext ${ctxSubclass.getText}")
    } else if (ctx.isInstanceOf[RubyParser.VariableReferencePrimaryContext]) {
      val ctxSubclass = ctx.asInstanceOf[RubyParser.VariableReferencePrimaryContext]
      println(s"RubyParser.VariableReferencePrimaryContext ${ctxSubclass.getText}")
    } else if (ctx.isInstanceOf[RubyParser.SimpleScopedConstantReferencePrimaryContext]) {
      val ctxSubclass = ctx.asInstanceOf[RubyParser.SimpleScopedConstantReferencePrimaryContext]
      println(s"RubyParser.SimpleScopedConstantReferencePrimaryContext ${ctxSubclass.getText}")
    } else if (ctx.isInstanceOf[RubyParser.ChainedScopedConstantReferencePrimaryContext]) {
      val ctxSubclass = ctx.asInstanceOf[RubyParser.ChainedScopedConstantReferencePrimaryContext]
      println(s"RubyParser.ChainedScopedConstantReferencePrimaryContext ${ctxSubclass.getText}")
    } else if (ctx.isInstanceOf[RubyParser.ArrayConstructorPrimaryContext]) {
      val ctxSubclass = ctx.asInstanceOf[RubyParser.ArrayConstructorPrimaryContext]
      println(s"RubyParser.ArrayConstructorPrimaryContext ${ctxSubclass.getText}")
    } else if (ctx.isInstanceOf[RubyParser.HashConstructorPrimaryContext]) {
      val ctxSubclass = ctx.asInstanceOf[RubyParser.HashConstructorPrimaryContext]
      println(s"RubyParser.HashConstructorPrimaryContext ${ctxSubclass.getText}")
    } else if (ctx.isInstanceOf[RubyParser.LiteralPrimaryContext]) {
      val ctxSubclass = ctx.asInstanceOf[RubyParser.LiteralPrimaryContext]
      println(s"RubyParser.LiteralPrimaryContext ${ctxSubclass.getText}")
    } else if (ctx.isInstanceOf[RubyParser.IsDefinedPrimaryContext]) {
      val ctxSubclass = ctx.asInstanceOf[RubyParser.IsDefinedPrimaryContext]
      println(s"RubyParser.IsDefinedPrimaryContext ${ctxSubclass.getText}")
    } else if (ctx.isInstanceOf[RubyParser.SuperExpressionPrimaryContext]) {
      val ctxSubclass = ctx.asInstanceOf[RubyParser.SuperExpressionPrimaryContext]
      println(s"RubyParser.SuperExpressionPrimaryContext ${ctxSubclass.getText}")
    } else if (ctx.isInstanceOf[RubyParser.IndexingExpressionPrimaryContext]) {
      val ctxSubclass = ctx.asInstanceOf[RubyParser.IndexingExpressionPrimaryContext]
      println(s"RubyParser.IndexingExpressionPrimaryContext ${ctxSubclass.getText}")
    } else if (ctx.isInstanceOf[RubyParser.MethodOnlyIdentifierPrimaryContext]) {
      val ctxSubclass = ctx.asInstanceOf[RubyParser.MethodOnlyIdentifierPrimaryContext]
      println(s"RubyParser.MethodOnlyIdentifierPrimaryContext ${ctxSubclass.getText}")
    } else if (ctx.isInstanceOf[RubyParser.InvocationWithBlockOnlyPrimaryContext]) {
      val ctxSubclass = ctx.asInstanceOf[RubyParser.InvocationWithBlockOnlyPrimaryContext]
      println(s"RubyParser.InvocationWithBlockOnlyPrimaryContext ${ctxSubclass.getText}")
    } else if (ctx.isInstanceOf[RubyParser.InvocationWithParenthesesPrimaryContext]) {
      val ctxSubclass = ctx.asInstanceOf[RubyParser.InvocationWithParenthesesPrimaryContext]
      println(s"RubyParser.InvocationWithParenthesesPrimaryContext ${ctxSubclass.getText}")
    } else if (ctx.isInstanceOf[RubyParser.ChainedInvocationPrimaryContext]) {
      val ctxSubclass = ctx.asInstanceOf[RubyParser.ChainedInvocationPrimaryContext]
      println(s"RubyParser.ChainedInvocationPrimaryContext ${ctxSubclass.getText}")
    } else if (ctx.isInstanceOf[RubyParser.ChainedInvocationWithoutArgumentsPrimaryContext]) {
      val ctxSubclass = ctx.asInstanceOf[RubyParser.ChainedInvocationWithoutArgumentsPrimaryContext]
      println(s"RubyParser.ChainedInvocationWithoutArgumentsPrimaryContext ${ctxSubclass.getText}")
    } else {
      println(s"RubyParser.astForPrimaryExpressionContext(). No subclass found")
    }
  }

  def astForExpression(ctx: RubyParser.ExpressionContext): Unit = {
    if (ctx.isInstanceOf[RubyParser.PrimaryExpressionContext]) {
      val ctxSubclass = ctx.asInstanceOf[RubyParser.PrimaryExpressionContext]
      println(s"RubyParser.PrimaryExpressionContext ${ctxSubclass.getText}")
      astForPrimaryExpressionContext(ctxSubclass.primary())
    } else if (ctx.isInstanceOf[RubyParser.UnaryExpressionContext]) {
      val ctxSubclass = ctx.asInstanceOf[RubyParser.UnaryExpressionContext]
      println(s"RubyParser.UnaryExpressionContext ${ctxSubclass.getText}")
    } else if (ctx.isInstanceOf[RubyParser.PowerExpressionContext]) {
      val ctxSubclass = ctx.asInstanceOf[RubyParser.PowerExpressionContext]
      println(s"RubyParser.PowerExpressionContext ${ctxSubclass.getText}")
    } else if (ctx.isInstanceOf[RubyParser.UnaryMinusExpressionContext]) {
      val ctxSubclass = ctx.asInstanceOf[RubyParser.UnaryMinusExpressionContext]
      println(s"RubyParser.UnaryMinusExpressionContext ${ctxSubclass.getText}")
    } else if (ctx.isInstanceOf[RubyParser.MultiplicativeExpressionContext]) {
      val ctxSubclass = ctx.asInstanceOf[RubyParser.MultiplicativeExpressionContext]
      println(s"RubyParser.MultiplicativeExpressionContext ${ctxSubclass.getText}")
    } else if (ctx.isInstanceOf[RubyParser.AdditiveExpressionContext]) {
      val ctxSubclass = ctx.asInstanceOf[RubyParser.AdditiveExpressionContext]
      println(s"RubyParser.AdditiveExpressionContext ${ctxSubclass.getText}")
    } else if (ctx.isInstanceOf[RubyParser.BitwiseShiftExpressionContext]) {
      val ctxSubclass = ctx.asInstanceOf[RubyParser.BitwiseShiftExpressionContext]
      println(s"RubyParser.BitwiseShiftExpressionContext ${ctxSubclass.getText}")
    } else if (ctx.isInstanceOf[RubyParser.BitwiseAndExpressionContext]) {
      val ctxSubclass = ctx.asInstanceOf[RubyParser.BitwiseAndExpressionContext]
      println(s"RubyParser.BitwiseAndExpressionContext ${ctxSubclass.getText}")
    } else if (ctx.isInstanceOf[RubyParser.BitwiseOrExpressionContext]) {
      val ctxSubclass = ctx.asInstanceOf[RubyParser.BitwiseOrExpressionContext]
      println(s"RubyParser.BitwiseOrExpressionContext ${ctxSubclass.getText}")
    } else if (ctx.isInstanceOf[RubyParser.RelationalExpressionContext]) {
      val ctxSubclass = ctx.asInstanceOf[RubyParser.RelationalExpressionContext]
      println(s"RubyParser.RelationalExpressionContext ${ctxSubclass.getText}")
    } else if (ctx.isInstanceOf[RubyParser.EqualityExpressionContext]) {
      val ctxSubclass = ctx.asInstanceOf[RubyParser.EqualityExpressionContext]
      println(s"RubyParser.EqualityExpressionContext ${ctxSubclass.getText}")
    } else if (ctx.isInstanceOf[RubyParser.OperatorAndExpressionContext]) {
      val ctxSubclass = ctx.asInstanceOf[RubyParser.OperatorAndExpressionContext]
      println(s"RubyParser.OperatorAndExpressionContext ${ctxSubclass.getText}")
    } else if (ctx.isInstanceOf[RubyParser.OperatorOrExpressionContext]) {
      val ctxSubclass = ctx.asInstanceOf[RubyParser.OperatorOrExpressionContext]
      println(s"RubyParser.OperatorOrExpressionContext ${ctxSubclass.getText}")
    } else if (ctx.isInstanceOf[RubyParser.RangeExpressionContext]) {
      val ctxSubclass = ctx.asInstanceOf[RubyParser.RangeExpressionContext]
      println(s"RubyParser.RangeExpressionContext ${ctxSubclass.getText}")
    } else if (ctx.isInstanceOf[RubyParser.ConditionalOperatorExpressionContext]) {
      val ctxSubclass = ctx.asInstanceOf[RubyParser.ConditionalOperatorExpressionContext]
      println(s"RubyParser.ConditionalOperatorExpressionContext ${ctxSubclass.getText}")
    } else if (ctx.isInstanceOf[RubyParser.SingleAssignmentExpressionContext]) {
      val ctxSubclass = ctx.asInstanceOf[RubyParser.SingleAssignmentExpressionContext]
      println(s"RubyParser.SingleAssignmentExpressionContext ${ctxSubclass.getText}")
      astForSingleAssignmentExpression(ctxSubclass)
    } else if (ctx.isInstanceOf[RubyParser.MultipleAssignmentExpressionContext]) {
      val ctxSubclass = ctx.asInstanceOf[RubyParser.MultipleAssignmentExpressionContext]
      println(s"RubyParser.MultipleAssignmentExpressionContext ${ctxSubclass.getText}")
    } else if (ctx.isInstanceOf[RubyParser.IsDefinedExpressionContext]) {
      val ctxSubclass = ctx.asInstanceOf[RubyParser.IsDefinedExpressionContext]
      println(s"RubyParser.IsDefinedExpressionContext ${ctxSubclass.getText}")
    } else {
      println(s"Unhandled context type: Text: ${ctx.getText}")
    }
  }

  def astForExpressionOrCommand(ctx: RubyParser.ExpressionOrCommandContext) = {
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
      astForExpression(ctxSubclass.expression())
    } else {
      println(s"Unhandled context type: Text: ${ctx.getText}")
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
          astForExpressionOrCommand(ctx1.expressionOrCommand())
        }
      })
  }

}
