package io.joern.rubysrc2cpg.astcreation

import io.joern.rubysrc2cpg.parser.RubyParser.ModifierStatementContext
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
import org.apache.commons.lang.mutable.Mutable
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate

import scala.collection.convert.ImplicitConversions.`collection AsScalaIterable`

class AstCreator(filename: String, global: Global) extends AstCreatorBase(filename) {

  object Defines {
    val Any: String               = "ANY"
    val Number: String            = "__ecma.Number"
    val String: String            = "__ecma.String"
    val Boolean: String           = "__ecma.Boolean"
    val Null: String              = "__ecma.Null"
    val Math: String              = "__ecma.Math"
    val Symbol: String            = "__ecma.Symbol"
    val Console: String           = "__whatwg.console"
    val Object: String            = "object"
    val NodeModulesFolder: String = "node_modules"
    val GlobalNamespace: String   = NamespaceTraversal.globalNamespaceName
  }
  override def createAst(): BatchedUpdate.DiffGraphBuilder = {
    val charStream  = CharStreams.fromFileName(filename)
    val lexer       = new RubyLexer(charStream)
    val tokenStream = new CommonTokenStream(lexer)
    val parser      = new RubyParser(tokenStream)
    val programCtx  = parser.program()

    val statementCtx   = programCtx.compoundStatement().statements()
    val statementAst   = astForStatement(statementCtx)
    val fileNode       = NewFile().name(filename).order(1)
    val namespaceBlock = globalNamespaceBlock()
    val ast            = Ast(fileNode).withChild(Ast(namespaceBlock).withChild(statementAst))
    storeInDiffGraph(ast, diffGraph)
    diffGraph
  }

  protected def createIdentifierNode(
    name: String,
    dynamicTypeOption: Option[String],
    line: Option[Integer],
    column: Option[Integer]
  ): NewIdentifier = NewIdentifier()
    .name(name)
    .code(name)
    .lineNumber(line)
    .columnNumber(column)
    .typeFullName(Defines.Any)
    .dynamicTypeHintFullName(dynamicTypeOption.toList)

  def astForVariableIdentifierContext(variableCtx: RubyParser.VariableIdentifierContext): Ast = {
    val varText =
      if (variableCtx.LOCAL_VARIABLE_IDENTIFIER() != null) {
        variableCtx.LOCAL_VARIABLE_IDENTIFIER().getSymbol.getText()
      } else if (variableCtx.GLOBAL_VARIABLE_IDENTIFIER() != null) {
        variableCtx.GLOBAL_VARIABLE_IDENTIFIER().getSymbol.getText()
      } else if (variableCtx.INSTANCE_VARIABLE_IDENTIFIER() != null) {
        variableCtx.INSTANCE_VARIABLE_IDENTIFIER().getSymbol.getText()
      } else if (variableCtx.CLASS_VARIABLE_IDENTIFIER() != null) {
        variableCtx.CLASS_VARIABLE_IDENTIFIER().getSymbol.getText()
      } else if (variableCtx.CONSTANT_IDENTIFIER() != null) {
        variableCtx.CONSTANT_IDENTIFIER().getSymbol.getText()
      }

    val node = createIdentifierNode(varText.toString, None, None, None)
    Ast(node)
  }

  def astForSingleLeftHandSide(ctx: RubyParser.SingleLeftHandSideContext): Ast = {
    val variableCtx = ctx.variableIdentifier()
    astForVariableIdentifierContext(variableCtx)
  }

  def astForExpressionOrCommandsContext(ctx: RubyParser.ExpressionOrCommandsContext): Ast = {
    val seqAst = {
      ctx
        .expressionOrCommand()
        .map(ec => {
          val ast = astForExpressionOrCommandContext(ec)
          ast
        })
    }.toSeq
    Ast().withChildren(seqAst)
  }

  def astForSplattingArgument(ctx: RubyParser.SplattingArgumentContext): Ast = {
    if (ctx == null) {
      return Ast()
    }
    astForExpressionOrCommandContext(ctx.expressionOrCommand())
  }

  def astForMultipleRightHandSide(ctx: RubyParser.MultipleRightHandSideContext): Ast = {
    val exprAst      = astForExpressionOrCommandsContext(ctx.expressionOrCommands())
    val splattingAst = astForSplattingArgument(ctx.splattingArgument())
    val seqAsts      = Seq[Ast](exprAst, splattingAst)
    Ast().withChildren(seqAsts)
  }

  def astForSingleAssignmentExpression(ctxSubclass: RubyParser.SingleAssignmentExpressionContext): Ast = {
    val leftAst  = astForSingleLeftHandSide(ctxSubclass.singleLeftHandSide())
    val rightAst = astForMultipleRightHandSide(ctxSubclass.multipleRightHandSide())
    val seqAsts  = Seq[Ast](leftAst, rightAst)
    Ast().withChildren(seqAsts)
  }

  def astForPrimaryExpressionContext(ctx: RubyParser.PrimaryContext): Ast = {
    if (ctx.isInstanceOf[RubyParser.ClassDefinitionPrimaryContext]) {
      astForClassDefinitionPrimaryContext(ctx.asInstanceOf[RubyParser.ClassDefinitionPrimaryContext])
    } else if (ctx.isInstanceOf[RubyParser.ModuleDefinitionPrimaryContext]) {
      astForModuleDefinitionPrimaryContext(ctx.asInstanceOf[RubyParser.ModuleDefinitionPrimaryContext])
    } else if (ctx.isInstanceOf[RubyParser.MethodDefinitionPrimaryContext]) {
      astForMethodDefinitionPrimaryContext(ctx.asInstanceOf[RubyParser.MethodDefinitionPrimaryContext])
    } else if (ctx.isInstanceOf[RubyParser.YieldWithOptionalArgumentPrimaryContext]) {
      astForYieldWithOptionalArgumentPrimaryContext(
        ctx.asInstanceOf[RubyParser.YieldWithOptionalArgumentPrimaryContext]
      )
    } else if (ctx.isInstanceOf[RubyParser.IfExpressionPrimaryContext]) {
      astForIfExpressionPrimaryContext(ctx.asInstanceOf[RubyParser.IfExpressionPrimaryContext])
    } else if (ctx.isInstanceOf[RubyParser.UnlessExpressionPrimaryContext]) {
      astForUnlessExpressionPrimaryContext(ctx.asInstanceOf[RubyParser.UnlessExpressionPrimaryContext])
    } else if (ctx.isInstanceOf[RubyParser.CaseExpressionPrimaryContext]) {
      astForCaseExpressionPrimaryContext(ctx.asInstanceOf[RubyParser.CaseExpressionPrimaryContext])
    } else if (ctx.isInstanceOf[RubyParser.WhileExpressionPrimaryContext]) {
      astForWhileExpressionPrimaryContext(ctx.asInstanceOf[RubyParser.WhileExpressionPrimaryContext])
    } else if (ctx.isInstanceOf[RubyParser.UntilExpressionPrimaryContext]) {
      astForUntilExpressionPrimaryContext(ctx.asInstanceOf[RubyParser.UntilExpressionPrimaryContext])
    } else if (ctx.isInstanceOf[RubyParser.ForExpressionPrimaryContext]) {
      astForForExpressionPrimaryContext(ctx.asInstanceOf[RubyParser.ForExpressionPrimaryContext])
    } else if (ctx.isInstanceOf[RubyParser.JumpExpressionPrimaryContext]) {
      astForJumpExpressionPrimaryContext(ctx.asInstanceOf[RubyParser.JumpExpressionPrimaryContext])
    } else if (ctx.isInstanceOf[RubyParser.BeginExpressionPrimaryContext]) {
      astForBeginExpressionPrimaryContext(ctx.asInstanceOf[RubyParser.BeginExpressionPrimaryContext])
    } else if (ctx.isInstanceOf[RubyParser.GroupingExpressionPrimaryContext]) {
      astForGroupingExpressionPrimaryContext(ctx.asInstanceOf[RubyParser.GroupingExpressionPrimaryContext])
    } else if (ctx.isInstanceOf[RubyParser.VariableReferencePrimaryContext]) {
      astForVariableReferencePrimaryContext(ctx.asInstanceOf[RubyParser.VariableReferencePrimaryContext])
    } else if (ctx.isInstanceOf[RubyParser.SimpleScopedConstantReferencePrimaryContext]) {
      astForSimpleScopedConstantReferencePrimaryContext(
        ctx.asInstanceOf[RubyParser.SimpleScopedConstantReferencePrimaryContext]
      )
    } else if (ctx.isInstanceOf[RubyParser.ChainedScopedConstantReferencePrimaryContext]) {
      astForChainedScopedConstantReferencePrimaryContext(
        ctx.asInstanceOf[RubyParser.ChainedScopedConstantReferencePrimaryContext]
      )
    } else if (ctx.isInstanceOf[RubyParser.ArrayConstructorPrimaryContext]) {
      astForArrayConstructorPrimaryContext(ctx.asInstanceOf[RubyParser.ArrayConstructorPrimaryContext])
    } else if (ctx.isInstanceOf[RubyParser.HashConstructorPrimaryContext]) {
      astForHashConstructorPrimaryContext(ctx.asInstanceOf[RubyParser.HashConstructorPrimaryContext])
    } else if (ctx.isInstanceOf[RubyParser.LiteralPrimaryContext]) {
      astForLiteralPrimaryContext(ctx.asInstanceOf[RubyParser.LiteralPrimaryContext])
    } else if (ctx.isInstanceOf[RubyParser.IsDefinedPrimaryContext]) {
      astForIsDefinedPrimaryContext(ctx.asInstanceOf[RubyParser.IsDefinedPrimaryContext])
    } else if (ctx.isInstanceOf[RubyParser.SuperExpressionPrimaryContext]) {
      astForSuperExpressionPrimaryContext(ctx.asInstanceOf[RubyParser.SuperExpressionPrimaryContext])
    } else if (ctx.isInstanceOf[RubyParser.IndexingExpressionPrimaryContext]) {
      astForIndexingExpressionPrimaryContext(ctx.asInstanceOf[RubyParser.IndexingExpressionPrimaryContext])
    } else if (ctx.isInstanceOf[RubyParser.MethodOnlyIdentifierPrimaryContext]) {
      astForMethodOnlyIdentifierPrimaryContext(ctx.asInstanceOf[RubyParser.MethodOnlyIdentifierPrimaryContext])
    } else if (ctx.isInstanceOf[RubyParser.InvocationWithBlockOnlyPrimaryContext]) {
      astForInvocationWithBlockOnlyPrimaryContext(ctx.asInstanceOf[RubyParser.InvocationWithBlockOnlyPrimaryContext])
    } else if (ctx.isInstanceOf[RubyParser.InvocationWithParenthesesPrimaryContext]) {
      astForInvocationWithParenthesesPrimaryContext(
        ctx.asInstanceOf[RubyParser.InvocationWithParenthesesPrimaryContext]
      )
    } else if (ctx.isInstanceOf[RubyParser.ChainedInvocationPrimaryContext]) {
      astForChainedInvocationPrimaryContext(ctx.asInstanceOf[RubyParser.ChainedInvocationPrimaryContext])
    } else if (ctx.isInstanceOf[RubyParser.ChainedInvocationWithoutArgumentsPrimaryContext]) {
      astForChainedInvocationWithoutArgumentsPrimaryContext(
        ctx.asInstanceOf[RubyParser.ChainedInvocationWithoutArgumentsPrimaryContext]
      )
    } else {
      println("astForPrimaryExpressionContext(): Unknown context")
      Ast()
    }
  }

  def astForExpressionContext(ctx: RubyParser.ExpressionContext): Ast = {
    if (ctx.isInstanceOf[RubyParser.PrimaryExpressionContext]) {
      astForPrimaryExpressionContext(ctx.asInstanceOf[RubyParser.PrimaryExpressionContext].primary())
    } else if (ctx.isInstanceOf[RubyParser.UnaryExpressionContext]) {
      astForUnaryExpressionContext(ctx.asInstanceOf[RubyParser.UnaryExpressionContext])
    } else if (ctx.isInstanceOf[RubyParser.PowerExpressionContext]) {
      astForPowerExpressionContext(ctx.asInstanceOf[RubyParser.PowerExpressionContext])
    } else if (ctx.isInstanceOf[RubyParser.UnaryMinusExpressionContext]) {
      astForUnaryMinusExpressionContext(ctx.asInstanceOf[RubyParser.UnaryMinusExpressionContext])
    } else if (ctx.isInstanceOf[RubyParser.MultiplicativeExpressionContext]) {
      astForMultiplicativeExpressionContext(ctx.asInstanceOf[RubyParser.MultiplicativeExpressionContext])
    } else if (ctx.isInstanceOf[RubyParser.AdditiveExpressionContext]) {
      astForAdditiveExpressionContext(ctx.asInstanceOf[RubyParser.AdditiveExpressionContext])
    } else if (ctx.isInstanceOf[RubyParser.BitwiseShiftExpressionContext]) {
      astForBitwiseShiftExpressionContext(ctx.asInstanceOf[RubyParser.BitwiseShiftExpressionContext])
    } else if (ctx.isInstanceOf[RubyParser.BitwiseAndExpressionContext]) {
      astForBitwiseAndExpressionContext(ctx.asInstanceOf[RubyParser.BitwiseAndExpressionContext])
    } else if (ctx.isInstanceOf[RubyParser.BitwiseOrExpressionContext]) {
      astForBitwiseOrExpressionContext(ctx.asInstanceOf[RubyParser.BitwiseOrExpressionContext])
    } else if (ctx.isInstanceOf[RubyParser.RelationalExpressionContext]) {
      astForRelationalExpressionContext(ctx.asInstanceOf[RubyParser.RelationalExpressionContext])
    } else if (ctx.isInstanceOf[RubyParser.EqualityExpressionContext]) {
      astForEqualityExpressionContext(ctx.asInstanceOf[RubyParser.EqualityExpressionContext])
    } else if (ctx.isInstanceOf[RubyParser.OperatorAndExpressionContext]) {
      astForOperatorAndExpressionContext(ctx.asInstanceOf[RubyParser.OperatorAndExpressionContext])
    } else if (ctx.isInstanceOf[RubyParser.OperatorOrExpressionContext]) {
      astForOperatorOrExpressionContext(ctx.asInstanceOf[RubyParser.OperatorOrExpressionContext])
    } else if (ctx.isInstanceOf[RubyParser.RangeExpressionContext]) {
      astForRangeExpressionContext(ctx.asInstanceOf[RubyParser.RangeExpressionContext])
    } else if (ctx.isInstanceOf[RubyParser.ConditionalOperatorExpressionContext]) {
      astForConditionalOperatorExpressionContext(ctx.asInstanceOf[RubyParser.ConditionalOperatorExpressionContext])
    } else if (ctx.isInstanceOf[RubyParser.SingleAssignmentExpressionContext]) {
      astForSingleAssignmentExpression(ctx.asInstanceOf[RubyParser.SingleAssignmentExpressionContext])
    } else if (ctx.isInstanceOf[RubyParser.MultipleAssignmentExpressionContext]) {
      astForMultipleAssignmentExpressionContext(ctx.asInstanceOf[RubyParser.MultipleAssignmentExpressionContext])
    } else if (ctx.isInstanceOf[RubyParser.IsDefinedExpressionContext]) {
      astForIsDefinedExpressionContext(ctx.asInstanceOf[RubyParser.IsDefinedExpressionContext])
    } else {
      println("astForExpressionContext(): Unknown context")
      Ast()
    }
  }

  def astForExpressionOrCommandContext(ctx: RubyParser.ExpressionOrCommandContext): Ast = {
    if (ctx.isInstanceOf[RubyParser.InvocationExpressionOrCommandContext]) {
      astForInvocationExpressionOrCommandContext(ctx.asInstanceOf[RubyParser.InvocationExpressionOrCommandContext])
    } else if (ctx.isInstanceOf[RubyParser.NotExpressionOrCommandContext]) {
      astForNotExpressionOrCommandContext(ctx.asInstanceOf[RubyParser.NotExpressionOrCommandContext])
    } else if (ctx.isInstanceOf[RubyParser.OrAndExpressionOrCommandContext]) {
      astForOrAndExpressionOrCommandContext(ctx.asInstanceOf[RubyParser.OrAndExpressionOrCommandContext])
    } else if (ctx.isInstanceOf[RubyParser.ExpressionExpressionOrCommandContext]) {
      astForExpressionContext(ctx.asInstanceOf[RubyParser.ExpressionExpressionOrCommandContext].expression())
    } else {
      println("astForExpressionOrCommandContext(): Unknown context")
      Ast()
    }
  }

  def astForAliasStatementContext(ctx: RubyParser.AliasStatementContext): Ast = {
    Ast()
  }

  def astForUndefStatementContext(ctx: RubyParser.UndefStatementContext): Ast = {
    Ast()
  }

  def astForBeginStatementContext(ctx: RubyParser.BeginStatementContext): Ast = {
    Ast()
  }

  def astForEndStatementContext(ctx: RubyParser.EndStatementContext): Ast = {
    Ast()
  }

  def astForModifierStatementContext(ctx: ModifierStatementContext): Ast = {
    Ast()
  }

  def astForStatement(ctx: RubyParser.StatementsContext): Ast = {

    val blockNode = NewBlock().typeFullName(Defines.Any)
    val seqAst = {
      ctx
        .statement()
        .map(st => {
          val ast =
            if (st.isInstanceOf[RubyParser.AliasStatementContext]) {
              astForAliasStatementContext(st.asInstanceOf[RubyParser.AliasStatementContext])
            } else if (st.isInstanceOf[RubyParser.UndefStatementContext]) {
              astForUndefStatementContext(st.asInstanceOf[RubyParser.UndefStatementContext])
            } else if (st.isInstanceOf[RubyParser.BeginStatementContext]) {
              astForBeginStatementContext(st.asInstanceOf[RubyParser.BeginStatementContext])
            } else if (st.isInstanceOf[RubyParser.EndStatementContext]) {
              astForEndStatementContext(st.asInstanceOf[RubyParser.EndStatementContext])
            } else if (st.isInstanceOf[RubyParser.ModifierStatementContext]) {
              astForModifierStatementContext(st.asInstanceOf[RubyParser.ModifierStatementContext])
            } else if (st.isInstanceOf[RubyParser.ExpressionOrCommandStatementContext]) {
              astForExpressionOrCommandContext(
                st.asInstanceOf[RubyParser.ExpressionOrCommandStatementContext].expressionOrCommand()
              )
            } else {
              println("astForExpressionContext(): Unknown context")
              Ast()
            }
          ast
        })
        .toSeq
    }

    Ast(blockNode).withChildren(seqAst)
  }

  def astForAdditiveExpressionContext(ctx: RubyParser.AdditiveExpressionContext): Ast = {
    Ast()
  }

  def astForArrayConstructorPrimaryContext(ctx: RubyParser.ArrayConstructorPrimaryContext): Ast = {
    Ast()
  }

  def astForBeginExpressionPrimaryContext(ctx: RubyParser.BeginExpressionPrimaryContext): Ast = {
    Ast()
  }

  def astForBitwiseAndExpressionContext(ctx: RubyParser.BitwiseAndExpressionContext): Ast = {
    Ast()
  }

  def astForBitwiseOrExpressionContext(ctx: RubyParser.BitwiseOrExpressionContext): Ast = {
    Ast()
  }

  def astForBitwiseShiftExpressionContext(ctx: RubyParser.BitwiseShiftExpressionContext): Ast = {
    Ast()
  }

  def astForCaseExpressionPrimaryContext(ctx: RubyParser.CaseExpressionPrimaryContext): Ast = {
    Ast()
  }

  def astForChainedInvocationPrimaryContext(ctx: RubyParser.ChainedInvocationPrimaryContext): Ast = {
    Ast()
  }

  def astForChainedInvocationWithoutArgumentsPrimaryContext(
    ctx: RubyParser.ChainedInvocationWithoutArgumentsPrimaryContext
  ): Ast = {
    Ast()
  }

  def astForChainedScopedConstantReferencePrimaryContext(
    ctx: RubyParser.ChainedScopedConstantReferencePrimaryContext
  ): Ast = {
    Ast()
  }

  def astForClassDefinitionPrimaryContext(ctx: RubyParser.ClassDefinitionPrimaryContext): Ast = {
    Ast()
  }

  def astForConditionalOperatorExpressionContext(ctx: RubyParser.ConditionalOperatorExpressionContext): Ast = {
    Ast()
  }

  def astForEqualityExpressionContext(ctx: RubyParser.EqualityExpressionContext): Ast = {
    Ast()
  }

  def astForExpressionExpressionOrCommandContext(ctx: RubyParser.ExpressionExpressionOrCommandContext): Ast = {
    Ast()
  }

  def astForForExpressionPrimaryContext(ctx: RubyParser.ForExpressionPrimaryContext): Ast = {
    Ast()
  }

  def astForGroupingExpressionPrimaryContext(ctx: RubyParser.GroupingExpressionPrimaryContext): Ast = {
    Ast()
  }

  def astForHashConstructorPrimaryContext(ctx: RubyParser.HashConstructorPrimaryContext): Ast = {
    Ast()
  }

  def astForIfExpressionPrimaryContext(ctx: RubyParser.IfExpressionPrimaryContext): Ast = {
    Ast()
  }

  def astForIndexingExpressionPrimaryContext(ctx: RubyParser.IndexingExpressionPrimaryContext): Ast = {
    Ast()
  }

  def astForInvocationExpressionOrCommandContext(ctx: RubyParser.InvocationExpressionOrCommandContext): Ast = {
    Ast()
  }

  def astForInvocationWithBlockOnlyPrimaryContext(ctx: RubyParser.InvocationWithBlockOnlyPrimaryContext): Ast = {
    Ast()
  }

  def astForInvocationWithParenthesesPrimaryContext(ctx: RubyParser.InvocationWithParenthesesPrimaryContext): Ast = {
    Ast()
  }

  def astForIsDefinedExpressionContext(ctx: RubyParser.IsDefinedExpressionContext): Ast = {
    Ast()
  }

  def astForIsDefinedPrimaryContext(ctx: RubyParser.IsDefinedPrimaryContext): Ast = {
    Ast()
  }

  def astForJumpExpressionPrimaryContext(ctx: RubyParser.JumpExpressionPrimaryContext): Ast = {
    Ast()
  }

  def astForLiteralPrimaryContext(ctx: RubyParser.LiteralPrimaryContext): Ast = {
    Ast()
  }

  def astForMethodDefinitionPrimaryContext(ctx: RubyParser.MethodDefinitionPrimaryContext): Ast = {
    Ast()
  }

  def astForMethodOnlyIdentifierPrimaryContext(ctx: RubyParser.MethodOnlyIdentifierPrimaryContext): Ast = {
    Ast()
  }

  def astForModuleDefinitionPrimaryContext(ctx: RubyParser.ModuleDefinitionPrimaryContext): Ast = {
    Ast()
  }

  def astForMultipleAssignmentExpressionContext(ctx: RubyParser.MultipleAssignmentExpressionContext): Ast = {
    Ast()
  }

  def astForMultiplicativeExpressionContext(ctx: RubyParser.MultiplicativeExpressionContext): Ast = {
    Ast()
  }

  def astForNotExpressionOrCommandContext(ctx: RubyParser.NotExpressionOrCommandContext): Ast = {
    Ast()
  }

  def astForOperatorAndExpressionContext(ctx: RubyParser.OperatorAndExpressionContext): Ast = {
    Ast()
  }

  def astForOperatorOrExpressionContext(ctx: RubyParser.OperatorOrExpressionContext): Ast = {
    Ast()
  }

  def astForOrAndExpressionOrCommandContext(ctx: RubyParser.OrAndExpressionOrCommandContext): Ast = {
    Ast()
  }

  def astForPowerExpressionContext(ctx: RubyParser.PowerExpressionContext): Ast = {
    Ast()
  }

  def astForPrimaryExpressionContext(ctx: RubyParser.PrimaryExpressionContext): Ast = {
    Ast()
  }

  def astForRangeExpressionContext(ctx: RubyParser.RangeExpressionContext): Ast = {
    Ast()
  }

  def astForRelationalExpressionContext(ctx: RubyParser.RelationalExpressionContext): Ast = {
    Ast()
  }

  def astForSimpleScopedConstantReferencePrimaryContext(
    ctx: RubyParser.SimpleScopedConstantReferencePrimaryContext
  ): Ast = {
    Ast()
  }

  def astForSingleAssignmentExpressionContext(ctx: RubyParser.SingleAssignmentExpressionContext): Ast = {
    Ast()
  }

  def astForSuperExpressionPrimaryContext(ctx: RubyParser.SuperExpressionPrimaryContext): Ast = {
    Ast()
  }

  def astForUnaryExpressionContext(ctx: RubyParser.UnaryExpressionContext): Ast = {
    Ast()
  }

  def astForUnaryMinusExpressionContext(ctx: RubyParser.UnaryMinusExpressionContext): Ast = {
    Ast()
  }

  def astForUnlessExpressionPrimaryContext(ctx: RubyParser.UnlessExpressionPrimaryContext): Ast = {
    Ast()
  }

  def astForUntilExpressionPrimaryContext(ctx: RubyParser.UntilExpressionPrimaryContext): Ast = {
    Ast()
  }

  def astForVariableReferencePrimaryContext(ctx: RubyParser.VariableReferencePrimaryContext): Ast = {
    Ast()
  }

  def astForWhileExpressionPrimaryContext(ctx: RubyParser.WhileExpressionPrimaryContext): Ast = {
    Ast()
  }

  def astForYieldWithOptionalArgumentPrimaryContext(ctx: RubyParser.YieldWithOptionalArgumentPrimaryContext): Ast = {
    Ast()
  }
}
