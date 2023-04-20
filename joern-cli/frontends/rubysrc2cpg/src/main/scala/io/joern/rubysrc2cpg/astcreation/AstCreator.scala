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

import scala.collection.mutable

class AstCreator(filename: String, global: Global) extends AstCreatorBase(filename) {

  object Defines {
    val Any: String     = "ANY"
    val Number: String  = "number"
    val String: String  = "string"
    val Boolean: String = "boolean"
    val Hash: String    = "hash"
    val Array: String   = "array"
    val Symbol: String  = "symbol"
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

  def astForVariableIdentifierContext(variableCtx: RubyParser.VariableIdentifierContext, varType: String): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    if (variableCtx == null) {
      return Ast()
    }

    val varSymbol =
      if (variableCtx.LOCAL_VARIABLE_IDENTIFIER() != null) {
        variableCtx.LOCAL_VARIABLE_IDENTIFIER().getSymbol()
      } else if (variableCtx.GLOBAL_VARIABLE_IDENTIFIER() != null) {
        variableCtx.GLOBAL_VARIABLE_IDENTIFIER().getSymbol()
      } else if (variableCtx.INSTANCE_VARIABLE_IDENTIFIER() != null) {
        variableCtx.INSTANCE_VARIABLE_IDENTIFIER().getSymbol()
      } else if (variableCtx.CLASS_VARIABLE_IDENTIFIER() != null) {
        variableCtx.CLASS_VARIABLE_IDENTIFIER().getSymbol()
      } else if (variableCtx.CONSTANT_IDENTIFIER() != null) {
        variableCtx.CONSTANT_IDENTIFIER().getSymbol()
      } else {
        return Ast()
      }

    val node = identifierNode(
      varSymbol.getText,
      None,
      Some(varSymbol.getLine()),
      Some(varSymbol.getCharPositionInLine()),
      List(varType)
    ).typeFullName(varType)
    Ast(node)
  }

  def astForSingleLeftHandSide(ctx: RubyParser.SingleLeftHandSideContext, rhsRetType: String): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    val variableCtx = ctx.variableIdentifier()
    astForVariableIdentifierContext(variableCtx, rhsRetType)
  }

  def astForExpressionOrCommandsContext(ctx: RubyParser.ExpressionOrCommandsContext): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    val asts = mutable.ArrayBuffer.empty[Ast]
    ctx
      .expressionOrCommand()
      .forEach(ec => {
        val ast = astForExpressionOrCommandContext(ec)
        asts.addOne(ast)
      })
    Ast().withChildren(asts.toSeq)
  }

  def astForSplattingArgument(ctx: RubyParser.SplattingArgumentContext): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    if (ctx == null) {
      return Ast()
    }
    astForExpressionOrCommandContext(ctx.expressionOrCommand())
  }

  def astForMultipleRightHandSide(ctx: RubyParser.MultipleRightHandSideContext): (Ast, String) = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    val exprAst      = astForExpressionOrCommandsContext(ctx.expressionOrCommands())
    val splattingAst = astForSplattingArgument(ctx.splattingArgument())
    val seqAsts      = Seq[Ast](exprAst, splattingAst)
    (Ast().withChildren(seqAsts), Defines.Any)
  }

  def astForSingleAssignmentExpression(ctxSubclass: RubyParser.SingleAssignmentExpressionContext): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    val (rightAst, rhsRetType) = astForMultipleRightHandSide(ctxSubclass.multipleRightHandSide())
    val leftAst                = astForSingleLeftHandSide(ctxSubclass.singleLeftHandSide(), rhsRetType)
    val seqAsts                = Seq[Ast](leftAst, rightAst)
    val blockNode              = NewBlock().typeFullName(Defines.Any)
    Ast(blockNode).withChildren(seqAsts)
  }

  def astForPrimaryContext(ctx: RubyParser.PrimaryContext): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

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
      println("astForPrimaryContext(): Unknown context")
      Ast()
    }
  }

  def astForExpressionContext(ctx: RubyParser.ExpressionContext): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    if (ctx.isInstanceOf[RubyParser.PrimaryExpressionContext]) {
      astForPrimaryContext(ctx.asInstanceOf[RubyParser.PrimaryExpressionContext].primary())
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
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

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
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    if (ctx == null) return Ast()

    val blockNode = NewBlock().typeFullName(Defines.Any)
    val asts      = mutable.ArrayBuffer.empty[Ast]
    ctx
      .statement()
      .forEach(st => {
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
        asts.addOne(ast)
      })

    Ast(blockNode).withChildren(asts.toSeq)
  }

  def astForAdditiveExpressionContext(ctx: RubyParser.AdditiveExpressionContext): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    Ast()
  }

  def astForArrayConstructorPrimaryContext(ctx: RubyParser.ArrayConstructorPrimaryContext): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    Ast()
  }

  def astForBeginExpressionPrimaryContext(ctx: RubyParser.BeginExpressionPrimaryContext): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    Ast()
  }

  def astForBitwiseAndExpressionContext(ctx: RubyParser.BitwiseAndExpressionContext): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    Ast()
  }

  def astForBitwiseOrExpressionContext(ctx: RubyParser.BitwiseOrExpressionContext): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    Ast()
  }

  def astForBitwiseShiftExpressionContext(ctx: RubyParser.BitwiseShiftExpressionContext): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    Ast()
  }

  def astForCaseExpressionPrimaryContext(ctx: RubyParser.CaseExpressionPrimaryContext): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    Ast()
  }

  def astForChainedInvocationPrimaryContext(ctx: RubyParser.ChainedInvocationPrimaryContext): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    Ast()
  }

  def astForChainedInvocationWithoutArgumentsPrimaryContext(
    ctx: RubyParser.ChainedInvocationWithoutArgumentsPrimaryContext
  ): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    Ast()
  }

  def astForChainedScopedConstantReferencePrimaryContext(
    ctx: RubyParser.ChainedScopedConstantReferencePrimaryContext
  ): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    Ast()
  }

  def astForClassDefinitionPrimaryContext(ctx: RubyParser.ClassDefinitionPrimaryContext): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    Ast()
  }

  def astForConditionalOperatorExpressionContext(ctx: RubyParser.ConditionalOperatorExpressionContext): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    Ast()
  }

  def astForEqualityExpressionContext(ctx: RubyParser.EqualityExpressionContext): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    Ast()
  }

  def astForForExpressionPrimaryContext(ctx: RubyParser.ForExpressionPrimaryContext): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    Ast()
  }

  def astForGroupingExpressionPrimaryContext(ctx: RubyParser.GroupingExpressionPrimaryContext): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    Ast()
  }

  def astForHashConstructorPrimaryContext(ctx: RubyParser.HashConstructorPrimaryContext): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    Ast()
  }

  def astForIfExpressionPrimaryContext(ctx: RubyParser.IfExpressionPrimaryContext): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    Ast()
  }

  def astForIndexingExpressionPrimaryContext(ctx: RubyParser.IndexingExpressionPrimaryContext): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    Ast()
  }

  def astForInvocationExpressionOrCommandContext(ctx: RubyParser.InvocationExpressionOrCommandContext): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    Ast()
  }

  def astForInvocationWithBlockOnlyPrimaryContext(ctx: RubyParser.InvocationWithBlockOnlyPrimaryContext): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    Ast()
  }

  def astForInvocationWithParenthesesPrimaryContext(ctx: RubyParser.InvocationWithParenthesesPrimaryContext): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    Ast()
  }

  def astForIsDefinedExpressionContext(ctx: RubyParser.IsDefinedExpressionContext): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    Ast()
  }

  def astForIsDefinedPrimaryContext(ctx: RubyParser.IsDefinedPrimaryContext): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    Ast()
  }

  def astForJumpExpressionPrimaryContext(ctx: RubyParser.JumpExpressionPrimaryContext): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    Ast()
  }

  def astForLiteralPrimaryContext(ctx: RubyParser.LiteralPrimaryContext): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    if (ctx.literal().symbol() != null) {
      Ast()

    } else if (ctx.literal().numericLiteral() != null) {
      val text = ctx.literal().numericLiteral().getText
      val node = NewLiteral()
        .code(text)
        .typeFullName(Defines.Number)
        .dynamicTypeHintFullName(List(Defines.Number))
      Ast(node)
    } else if (ctx.literal().SINGLE_QUOTED_STRING_LITERAL() != null) {
      val text = ctx.literal().SINGLE_QUOTED_STRING_LITERAL().getText
      val node = NewLiteral()
        .code(text)
        .typeFullName(Defines.String)
        .dynamicTypeHintFullName(List(Defines.String))
      Ast(node)
    } else {
      // double quoted string literal
      Ast()
    }
  }

  def astForMethodDefinitionPrimaryContext(ctx: RubyParser.MethodDefinitionPrimaryContext): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    Ast()
  }

  def astForMethodOnlyIdentifierPrimaryContext(ctx: RubyParser.MethodOnlyIdentifierPrimaryContext): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    Ast()
  }

  def astForModuleDefinitionPrimaryContext(ctx: RubyParser.ModuleDefinitionPrimaryContext): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    Ast()
  }

  def astForMultipleAssignmentExpressionContext(ctx: RubyParser.MultipleAssignmentExpressionContext): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    Ast()
  }

  def astForMultiplicativeExpressionContext(ctx: RubyParser.MultiplicativeExpressionContext): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    Ast()
  }

  def astForNotExpressionOrCommandContext(ctx: RubyParser.NotExpressionOrCommandContext): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    Ast()
  }

  def astForOperatorAndExpressionContext(ctx: RubyParser.OperatorAndExpressionContext): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    Ast()
  }

  def astForOperatorOrExpressionContext(ctx: RubyParser.OperatorOrExpressionContext): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    Ast()
  }

  def astForOrAndExpressionOrCommandContext(ctx: RubyParser.OrAndExpressionOrCommandContext): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    Ast()
  }

  def astForPowerExpressionContext(ctx: RubyParser.PowerExpressionContext): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    Ast()
  }

  def astForRangeExpressionContext(ctx: RubyParser.RangeExpressionContext): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    Ast()
  }

  def astForRelationalExpressionContext(ctx: RubyParser.RelationalExpressionContext): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    Ast()
  }

  def astForSimpleScopedConstantReferencePrimaryContext(
    ctx: RubyParser.SimpleScopedConstantReferencePrimaryContext
  ): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    Ast()
  }

  def astForSuperExpressionPrimaryContext(ctx: RubyParser.SuperExpressionPrimaryContext): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    Ast()
  }

  def astForUnaryExpressionContext(ctx: RubyParser.UnaryExpressionContext): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    Ast()
  }

  def astForUnaryMinusExpressionContext(ctx: RubyParser.UnaryMinusExpressionContext): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    Ast()
  }

  def astForUnlessExpressionPrimaryContext(ctx: RubyParser.UnlessExpressionPrimaryContext): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    Ast()
  }

  def astForUntilExpressionPrimaryContext(ctx: RubyParser.UntilExpressionPrimaryContext): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    Ast()
  }

  def astForVariableReferencePrimaryContext(ctx: RubyParser.VariableReferencePrimaryContext): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    Ast()
  }

  def astForWhileExpressionPrimaryContext(ctx: RubyParser.WhileExpressionPrimaryContext): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )
    Ast()
  }

  def astForYieldWithOptionalArgumentPrimaryContext(ctx: RubyParser.YieldWithOptionalArgumentPrimaryContext): Ast = {
    println(
      s"${Thread.currentThread.getStackTrace()(1).getMethodName}() invoked. Stack size: ${Thread.currentThread.getStackTrace().size}"
    )

    Ast()
  }
}
