package io.joern.rubysrc2cpg.astcreation

import io.joern.rubysrc2cpg.parser.RubyParser.{IF, IS_DEFINED, ModifierStatementContext, RESCUE, UNLESS, UNTIL, WHILE}
import io.joern.rubysrc2cpg.parser.{RubyLexer, RubyParser, RubyParserVisitor}
import org.antlr.v4.runtime.tree.{ErrorNode, ParseTree, ParseTreeListener, ParseTreeWalker, RuleNode, TerminalNode}
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream, ParserRuleContext, Token}
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
import io.shiftleft.codepropertygraph.cpgloading.ProtoToCpg.logger
import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes.Call.PropertyDefaults
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.passes.IntervalKeyPool
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import org.apache.commons.lang.mutable.Mutable
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate

import java.util
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

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

  private val logger = LoggerFactory.getLogger(this.getClass)

  override def createAst(): BatchedUpdate.DiffGraphBuilder = {
    val charStream  = CharStreams.fromFileName(filename)
    val lexer       = new RubyLexer(charStream)
    val tokenStream = new CommonTokenStream(lexer)
    val parser      = new RubyParser(tokenStream)
    val programCtx  = parser.program()

    val statementCtx   = programCtx.compoundStatement().statements()
    val statementAst   = astForStatementsContext(statementCtx)
    val fileNode       = NewFile().name(filename).order(1)
    val namespaceBlock = globalNamespaceBlock()
    val ast            = Ast(fileNode).withChild(Ast(namespaceBlock).withChild(statementAst))
    storeInDiffGraph(ast, diffGraph)
    diffGraph
  }

  def astForVariableIdentifierContext(ctx: RubyParser.VariableIdentifierContext, varType: String): Ast = {
    if (ctx == null) return Ast()

    val varSymbol =
      if (ctx.LOCAL_VARIABLE_IDENTIFIER() != null) {
        ctx.LOCAL_VARIABLE_IDENTIFIER().getSymbol()
      } else if (ctx.GLOBAL_VARIABLE_IDENTIFIER() != null) {
        ctx.GLOBAL_VARIABLE_IDENTIFIER().getSymbol()
      } else if (ctx.INSTANCE_VARIABLE_IDENTIFIER() != null) {
        ctx.INSTANCE_VARIABLE_IDENTIFIER().getSymbol()
      } else if (ctx.CLASS_VARIABLE_IDENTIFIER() != null) {
        ctx.CLASS_VARIABLE_IDENTIFIER().getSymbol()
      } else if (ctx.CONSTANT_IDENTIFIER() != null) {
        ctx.CONSTANT_IDENTIFIER().getSymbol()
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

  def astForSingleLeftHandSideContext(ctx: RubyParser.SingleLeftHandSideContext, rhsRetType: String): Ast = {
    if (ctx == null) return Ast()
    val variableCtx = ctx.variableIdentifier()
    astForVariableIdentifierContext(variableCtx, rhsRetType)
  }

  def astForExpressionOrCommandsContext(ctx: RubyParser.ExpressionOrCommandsContext): Ast = {
    if (ctx == null) return Ast()

    val asts = mutable.ArrayBuffer.empty[Ast]
    ctx
      .expressionOrCommand()
      .forEach(ec => {
        val ast = astForExpressionOrCommandContext(ec)
        asts.addOne(ast)
      })
    Ast().withChildren(asts.toSeq)
  }

  def astForSplattingArgumentContext(ctx: RubyParser.SplattingArgumentContext): Ast = {
    if (ctx == null) return Ast()
    astForExpressionOrCommandContext(ctx.expressionOrCommand())
  }

  def astForMultipleRightHandSideContext(ctx: RubyParser.MultipleRightHandSideContext): (Ast, String) = {
    if (ctx == null) return (Ast(), Defines.Any)

    val exprAst      = astForExpressionOrCommandsContext(ctx.expressionOrCommands())
    val splattingAst = astForSplattingArgumentContext(ctx.splattingArgument())
    val seqAsts      = Seq[Ast](exprAst, splattingAst)
    (Ast().withChildren(seqAsts), Defines.Any)
  }

  def astForSingleAssignmentExpressionContext(ctx: RubyParser.SingleAssignmentExpressionContext): Ast = {
    if (ctx == null) return Ast()

    val (rightAst, rhsRetType) = astForMultipleRightHandSideContext(ctx.multipleRightHandSide())
    val leftAst                = astForSingleLeftHandSideContext(ctx.singleLeftHandSide(), rhsRetType)
    val seqAsts                = Seq[Ast](leftAst, rightAst)
    val blockNode              = NewBlock().typeFullName(Defines.Any)
    Ast(blockNode).withChildren(seqAsts)
  }

  def astForPrimaryContext(ctx: RubyParser.PrimaryContext): Ast = {
    if (ctx == null) return Ast()

    ctx match {
      case ctx: RubyParser.ClassDefinitionPrimaryContext =>
        astForClassDefinitionPrimaryContext(ctx)
      case ctx: RubyParser.ModuleDefinitionPrimaryContext =>
        astForModuleDefinitionPrimaryContext(ctx)
      case ctx: RubyParser.MethodDefinitionPrimaryContext =>
        astForMethodDefinitionContext(ctx.methodDefinition())
      case ctx: RubyParser.YieldWithOptionalArgumentPrimaryContext =>
        astForYieldWithOptionalArgumentPrimaryContext(ctx)
      case ctx: RubyParser.IfExpressionPrimaryContext =>
        astForIfExpressionPrimaryContext(ctx)
      case ctx: RubyParser.UnlessExpressionPrimaryContext =>
        astForUnlessExpressionPrimaryContext(ctx)
      case ctx: RubyParser.CaseExpressionPrimaryContext =>
        astForCaseExpressionPrimaryContext(ctx)
      case ctx: RubyParser.WhileExpressionPrimaryContext =>
        astForWhileExpressionPrimaryContext(ctx)
      case ctx: RubyParser.UntilExpressionPrimaryContext =>
        astForUntilExpressionPrimaryContext(ctx)
      case ctx: RubyParser.ForExpressionPrimaryContext =>
        astForForExpressionPrimaryContext(ctx)
      case ctx: RubyParser.JumpExpressionPrimaryContext =>
        astForJumpExpressionPrimaryContext(ctx)
      case ctx: RubyParser.BeginExpressionPrimaryContext =>
        astForBeginExpressionPrimaryContext(ctx)
      case ctx: RubyParser.GroupingExpressionPrimaryContext =>
        astForGroupingExpressionPrimaryContext(ctx)
      case ctx: RubyParser.VariableReferencePrimaryContext =>
        astForVariableReferencePrimaryContext(ctx)
      case ctx: RubyParser.SimpleScopedConstantReferencePrimaryContext =>
        astForSimpleScopedConstantReferencePrimaryContext(ctx)
      case ctx: RubyParser.ChainedScopedConstantReferencePrimaryContext =>
        astForChainedScopedConstantReferencePrimaryContext(ctx)
      case ctx: RubyParser.ArrayConstructorPrimaryContext =>
        astForArrayConstructorPrimaryContext(ctx)
      case ctx: RubyParser.HashConstructorPrimaryContext =>
        astForHashConstructorPrimaryContext(ctx)
      case ctx: RubyParser.LiteralPrimaryContext =>
        astForLiteralPrimaryContext(ctx)
      case ctx: RubyParser.IsDefinedPrimaryContext =>
        astForIsDefinedPrimaryContext(ctx)
      case ctx: RubyParser.SuperExpressionPrimaryContext =>
        astForSuperExpressionPrimaryContext(ctx)
      case ctx: RubyParser.IndexingExpressionPrimaryContext =>
        astForIndexingExpressionPrimaryContext(ctx)
      case ctx: RubyParser.MethodOnlyIdentifierPrimaryContext =>
        astForMethodOnlyIdentifierPrimaryContext(ctx)
      case ctx: RubyParser.InvocationWithBlockOnlyPrimaryContext =>
        astForInvocationWithBlockOnlyPrimaryContext(ctx)
      case ctx: RubyParser.InvocationWithParenthesesPrimaryContext =>
        astForInvocationWithParenthesesPrimaryContext(ctx)
      case ctx: RubyParser.ChainedInvocationPrimaryContext =>
        astForChainedInvocationPrimaryContext(ctx)
      case ctx: RubyParser.ChainedInvocationWithoutArgumentsPrimaryContext =>
        astForChainedInvocationWithoutArgumentsPrimaryContext(ctx)
      case _ =>
        logger.error("astForPrimaryContext(): Unknown context")
        Ast()
    }
  }

  def astForExpressionContext(ctx: RubyParser.ExpressionContext): Ast = {
    if (ctx == null) return Ast()
    ctx match {
      case ctx: RubyParser.PrimaryExpressionContext =>
        astForPrimaryContext(ctx.primary())
      case ctx: RubyParser.UnaryExpressionContext =>
        astForUnaryExpressionContext(ctx)
      case ctx: RubyParser.PowerExpressionContext =>
        astForPowerExpressionContext(ctx)
      case ctx: RubyParser.UnaryMinusExpressionContext =>
        astForUnaryMinusExpressionContext(ctx)
      case ctx: RubyParser.MultiplicativeExpressionContext =>
        astForMultiplicativeExpressionContext(ctx)
      case ctx: RubyParser.AdditiveExpressionContext =>
        astForAdditiveExpressionContext(ctx)
      case ctx: RubyParser.BitwiseShiftExpressionContext =>
        astForBitwiseShiftExpressionContext(ctx)
      case ctx: RubyParser.BitwiseAndExpressionContext =>
        astForBitwiseAndExpressionContext(ctx)
      case ctx: RubyParser.BitwiseOrExpressionContext =>
        astForBitwiseOrExpressionContext(ctx)
      case ctx: RubyParser.RelationalExpressionContext =>
        astForRelationalExpressionContext(ctx)
      case ctx: RubyParser.EqualityExpressionContext =>
        astForEqualityExpressionContext(ctx)
      case ctx: RubyParser.OperatorAndExpressionContext =>
        astForOperatorAndExpressionContext(ctx)
      case ctx: RubyParser.OperatorOrExpressionContext =>
        astForOperatorOrExpressionContext(ctx)
      case ctx: RubyParser.RangeExpressionContext =>
        astForRangeExpressionContext(ctx)
      case ctx: RubyParser.ConditionalOperatorExpressionContext =>
        astForConditionalOperatorExpressionContext(ctx)
      case ctx: RubyParser.SingleAssignmentExpressionContext =>
        astForSingleAssignmentExpressionContext(ctx)
      case ctx: RubyParser.MultipleAssignmentExpressionContext =>
        astForMultipleAssignmentExpressionContext(ctx)
      case ctx: RubyParser.IsDefinedExpressionContext =>
        astForIsDefinedExpressionContext(ctx)
      case _ =>
        logger.error("astForExpressionContext(): Unknown context")
        Ast()
    }
  }

  def astForExpressionOrCommandContext(ctx: RubyParser.ExpressionOrCommandContext): Ast = {
    if (ctx == null) return Ast()

    ctx match {
      case ctx: RubyParser.InvocationExpressionOrCommandContext =>
        astForInvocationExpressionOrCommandContext(ctx)
      case ctx: RubyParser.NotExpressionOrCommandContext =>
        astForNotExpressionOrCommandContext(ctx)
      case ctx: RubyParser.OrAndExpressionOrCommandContext =>
        astForOrAndExpressionOrCommandContext(ctx)
      case ctx: RubyParser.ExpressionExpressionOrCommandContext =>
        astForExpressionContext(ctx.expression())
      case _ =>
        logger.error("astForExpressionOrCommandContext(): Unknown context")
        Ast()
    }
  }

  def astForSymbolContext(ctx: RubyParser.SymbolContext): Ast = {
    if (ctx == null) return Ast()

    if (ctx.SYMBOL_LITERAL() != null) {
      val text = ctx.getText
      val node = NewLiteral()
        .code(text)
        .typeFullName(Defines.String)
        .dynamicTypeHintFullName(List(Defines.String))
      Ast(node)
    } else if (ctx.SINGLE_QUOTED_STRING_LITERAL() != null) {
      Ast()
    } else {
      Ast()
    }
  }

  def astForDefinedMethodNameOrSymbolContext(ctx: RubyParser.DefinedMethodNameOrSymbolContext): Ast = {
    if (ctx == null) return Ast()

    val definedMethodNameCtx = ctx.definedMethodName()
    val symbolCtx            = ctx.symbol()

    val asts = ListBuffer[Ast]()
    if (definedMethodNameCtx != null) {
      asts.addOne(astForDefinedMethodNameContext(definedMethodNameCtx))
    }

    if (symbolCtx != null) {
      asts.addOne(astForSymbolContext(symbolCtx))
    }

    Ast().withChildren(asts)
  }

  def astForAliasStatementContext(ctx: RubyParser.AliasStatementContext): Ast = {
    if (ctx == null) return Ast()

    val asts = ListBuffer[Ast]()
    ctx
      .definedMethodNameOrSymbol()
      .forEach(dms => {
        asts.addOne(astForDefinedMethodNameOrSymbolContext(dms))
      })

    Ast().withChildren(asts)
  }

  def astForUndefStatementContext(ctx: RubyParser.UndefStatementContext): Ast = {
    if (ctx == null) return Ast()
    Ast()
  }

  def astForBeginStatementContext(ctx: RubyParser.BeginStatementContext): Ast = {
    if (ctx == null) return Ast()

    val astStmts  = astForStatementsContext(ctx.statements())
    val blockNode = NewBlock().typeFullName(Defines.Any)
    Ast(blockNode).withChild(astStmts)
  }

  def astForEndStatementContext(ctx: RubyParser.EndStatementContext): Ast = {
    if (ctx == null) return Ast()
    val astStmts  = astForStatementsContext(ctx.statements())
    val blockNode = NewBlock().typeFullName(Defines.Any)
    Ast(blockNode).withChild(astStmts)
  }

  def astForModifierStatementContext(ctx: ModifierStatementContext): Ast = {
    if (ctx == null) return Ast()
    Ast()

    if (ctx.statement().size() != 2) {
      // unsupported or invalid modifer statement
      Ast()
    }
    val leftAst        = astForStatementContext(ctx.statement(0))
    val statementRight = ctx.statement(1)

    val modifierToken = ctx.mod

    // Separating the cases so that each could be handled differently if needed
    val rightAst = modifierToken.getType() match {
      case IF     => astForStatementContext(statementRight)
      case UNLESS => astForStatementContext(statementRight)
      case WHILE  => astForStatementContext(statementRight)
      case UNTIL  => astForStatementContext(statementRight)
      case RESCUE => astForStatementContext(statementRight)
    }

    leftAst.withChild(rightAst)
  }

  def astForStatementContext(ctx: RubyParser.StatementContext): Ast = {
    if (ctx == null) return Ast()

    ctx match {
      case ctx: RubyParser.AliasStatementContext =>
        astForAliasStatementContext(ctx)
      case ctx: RubyParser.UndefStatementContext =>
        astForUndefStatementContext(ctx)
      case ctx: RubyParser.BeginStatementContext =>
        astForBeginStatementContext(ctx)
      case ctx: RubyParser.EndStatementContext =>
        astForEndStatementContext(ctx)
      case ctx: RubyParser.ModifierStatementContext =>
        astForModifierStatementContext(ctx)
      case ctx: RubyParser.ExpressionOrCommandStatementContext =>
        astForExpressionOrCommandContext(ctx.expressionOrCommand())
      case _ =>
        logger.error("astForExpressionContext(): Unknown context")
        Ast()
    }
  }

  def astForStatementsContext(ctx: RubyParser.StatementsContext): Ast = {
    if (ctx == null) return Ast()

    val blockNode = NewBlock().typeFullName(Defines.Any)
    val asts      = mutable.ArrayBuffer.empty[Ast]
    ctx
      .statement()
      .forEach(st => {
        asts.addOne(astForStatementContext(st))
      })

    Ast(blockNode).withChildren(asts.toSeq)
  }

  def astForAdditiveExpressionContext(ctx: RubyParser.AdditiveExpressionContext): Ast = {
    if (ctx == null) return Ast()

    astForBinaryExpression(ctx.expression().get(0), ctx.expression().get(1), ctx.op)
  }

  def astForArrayConstructorPrimaryContext(ctx: RubyParser.ArrayConstructorPrimaryContext): Ast = {
    if (ctx == null) return Ast()
    Ast()
  }

  def astForBeginExpressionPrimaryContext(ctx: RubyParser.BeginExpressionPrimaryContext): Ast = {
    if (ctx == null) return Ast()
    Ast()
  }

  def astForBitwiseAndExpressionContext(ctx: RubyParser.BitwiseAndExpressionContext): Ast = {
    if (ctx == null) return Ast()
    astForBinaryExpression(ctx.expression().get(0), ctx.expression().get(1), ctx.op)
  }

  def astForBitwiseOrExpressionContext(ctx: RubyParser.BitwiseOrExpressionContext): Ast = {
    if (ctx == null) return Ast()
    astForBinaryExpression(ctx.expression().get(0), ctx.expression().get(1), ctx.op)
  }

  def astForBitwiseShiftExpressionContext(ctx: RubyParser.BitwiseShiftExpressionContext): Ast = {
    if (ctx == null) return Ast()
    astForBinaryExpression(ctx.expression().get(0), ctx.expression().get(1), ctx.op)
  }

  def astForCaseExpressionPrimaryContext(ctx: RubyParser.CaseExpressionPrimaryContext): Ast = {
    if (ctx == null) return Ast()
    Ast()
  }

  def astForChainedInvocationPrimaryContext(ctx: RubyParser.ChainedInvocationPrimaryContext): Ast = {
    if (ctx == null) return Ast()
    Ast()
  }

  def astForChainedInvocationWithoutArgumentsPrimaryContext(
    ctx: RubyParser.ChainedInvocationWithoutArgumentsPrimaryContext
  ): Ast = {
    if (ctx == null) return Ast()
    Ast()
  }

  def astForChainedScopedConstantReferencePrimaryContext(
    ctx: RubyParser.ChainedScopedConstantReferencePrimaryContext
  ): Ast = {
    if (ctx == null) return Ast()
    Ast()
  }

  def astForClassDefinitionContext(ctx: RubyParser.ClassOrModuleReferenceContext): Ast = {
    if (ctx == null) return Ast()
    Ast()
  }

  def astForClassDefinitionPrimaryContext(ctx: RubyParser.ClassDefinitionPrimaryContext): Ast = {
    if (ctx == null) return Ast()
    val astClassOrModuleRef = astForClassDefinitionContext(ctx.classDefinition().classOrModuleReference())
    val astExprOfCommand    = astForExpressionOrCommandContext(ctx.classDefinition().expressionOrCommand())
    val astBodyStatement    = astForBodyStatementContext(ctx.classDefinition().bodyStatement())

    Ast().withChildren(Seq[Ast](astClassOrModuleRef, astExprOfCommand, astBodyStatement))
  }

  def astForConditionalOperatorExpressionContext(ctx: RubyParser.ConditionalOperatorExpressionContext): Ast = {
    if (ctx == null) return Ast()
    Ast()
  }

  def astForEqualityExpressionContext(ctx: RubyParser.EqualityExpressionContext): Ast = {
    if (ctx == null) return Ast()
    astForBinaryExpression(ctx.expression().get(0), ctx.expression().get(1), ctx.op)
  }

  def astForForExpressionPrimaryContext(ctx: RubyParser.ForExpressionPrimaryContext): Ast = {
    if (ctx == null) return Ast()
    Ast()
  }

  def astForGroupingExpressionPrimaryContext(ctx: RubyParser.GroupingExpressionPrimaryContext): Ast = {
    if (ctx == null) return Ast()
    Ast()
  }

  def astForHashConstructorPrimaryContext(ctx: RubyParser.HashConstructorPrimaryContext): Ast = {
    if (ctx == null) return Ast()
    Ast()
  }

  def astForIfExpressionPrimaryContext(ctx: RubyParser.IfExpressionPrimaryContext): Ast = {
    if (ctx == null) return Ast()
    Ast()
  }

  def astForIndexingExpressionPrimaryContext(ctx: RubyParser.IndexingExpressionPrimaryContext): Ast = {
    if (ctx == null) return Ast()
    Ast()
  }

  def astForInvocationExpressionOrCommandContext(ctx: RubyParser.InvocationExpressionOrCommandContext): Ast = {
    if (ctx == null) return Ast()
    Ast()
  }

  def astForInvocationWithBlockOnlyPrimaryContext(ctx: RubyParser.InvocationWithBlockOnlyPrimaryContext): Ast = {
    if (ctx == null) return Ast()
    Ast()
  }

  def astForInvocationWithParenthesesPrimaryContext(ctx: RubyParser.InvocationWithParenthesesPrimaryContext): Ast = {
    if (ctx == null) return Ast()
    Ast()
  }

  def astForIsDefinedExpressionContext(ctx: RubyParser.IsDefinedExpressionContext): Ast = {
    if (ctx == null) return Ast()
    Ast()
  }

  def astForIsDefinedPrimaryContext(ctx: RubyParser.IsDefinedPrimaryContext): Ast = {
    if (ctx == null) return Ast()
    Ast()
  }

  def astForJumpExpressionPrimaryContext(ctx: RubyParser.JumpExpressionPrimaryContext): Ast = {
    if (ctx == null) return Ast()
    Ast()
  }

  def astForLiteralPrimaryContext(ctx: RubyParser.LiteralPrimaryContext): Ast = {
    if (ctx == null) return Ast()
    if (ctx.literal().symbol() != null) {
      Ast()

    } else if (ctx.literal().numericLiteral() != null) {
      val text = ctx.getText
      val node = NewLiteral()
        .code(text)
        .typeFullName(Defines.Number)
        .dynamicTypeHintFullName(List(Defines.Number))
      Ast(node)
    } else if (ctx.literal().SINGLE_QUOTED_STRING_LITERAL() != null) {
      val text = ctx.getText
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

  def astForSimpleMethodNamePartContext(ctx: RubyParser.SimpleMethodNamePartContext): Ast = {
    if (ctx == null) return Ast()
    astForDefinedMethodNameContext(ctx.definedMethodName())
  }

  def astForMethodOnlyIdentifier(ctx: RubyParser.MethodOnlyIdentifierContext): Ast = {
    if (ctx == null) return Ast()
    if (ctx.LOCAL_VARIABLE_IDENTIFIER() != null) {
      val localVar  = ctx.LOCAL_VARIABLE_IDENTIFIER()
      val varSymbol = localVar.getSymbol()
      val node = identifierNode(
        varSymbol.getText,
        None,
        Some(varSymbol.getLine()),
        Some(varSymbol.getCharPositionInLine()),
        List(Defines.Any)
      ).typeFullName(Defines.Any)
      Ast(node)
    } else if (ctx.CONSTANT_IDENTIFIER() != null) {
      Ast()
    } else {
      Ast()
    }
  }

  def astForMethodIdentifierContext(ctx: RubyParser.MethodIdentifierContext): Ast = {
    if (ctx == null) return Ast()
    if (ctx.LOCAL_VARIABLE_IDENTIFIER() != null) {
      val localVar = ctx.LOCAL_VARIABLE_IDENTIFIER()
      Ast()
    } else if (ctx.CONSTANT_IDENTIFIER() != null) {
      Ast()
    } else if (ctx.methodOnlyIdentifier() != null) {
      astForMethodOnlyIdentifier(ctx.methodOnlyIdentifier())
    } else {
      Ast()
    }
  }

  def astForMethodNameContext(ctx: RubyParser.MethodNameContext): Ast = {
    if (ctx == null) return Ast()

    if (ctx.methodIdentifier() != null) {
      astForMethodIdentifierContext(ctx.methodIdentifier())
    } else if (ctx.operatorMethodName() != null) {
      Ast()

    } else if (ctx.keyword() != null) {
      Ast()
    } else {
      Ast()
    }
  }
  def astForAssignmentLikeMethodIdentifierContext(ctx: RubyParser.AssignmentLikeMethodIdentifierContext): Ast = {
    if (ctx == null) return Ast()

    if (ctx.LOCAL_VARIABLE_IDENTIFIER() != null) {
      val localVar  = ctx.LOCAL_VARIABLE_IDENTIFIER()
      val varSymbol = localVar.getSymbol()
      val node = identifierNode(
        varSymbol.getText,
        None,
        Some(varSymbol.getLine()),
        Some(varSymbol.getCharPositionInLine()),
        List(Defines.Any)
      ).typeFullName(Defines.Any)
      Ast(node)
    } else if (ctx.CONSTANT_IDENTIFIER() != null) {
      Ast()
    } else {
      Ast()
    }
  }

  def astForDefinedMethodNameContext(ctx: RubyParser.DefinedMethodNameContext): Ast = {
    if (ctx == null) return Ast()
    val methodNameAst         = astForMethodNameContext(ctx.methodName())
    val assignLinkedMethodAst = astForAssignmentLikeMethodIdentifierContext(ctx.assignmentLikeMethodIdentifier())
    Ast().withChildren(Seq[Ast](methodNameAst, assignLinkedMethodAst))
  }

  def astForSingletonObjextContext(ctx: RubyParser.SingletonObjectContext): Ast = {
    if (ctx == null) return Ast()
    if (ctx.variableIdentifier() != null) {
      astForVariableIdentifierContext(ctx.variableIdentifier(), Defines.Any)
    } else if (ctx.pseudoVariableIdentifier() != null) {
      Ast()
    } else if (ctx.expressionOrCommand() != null) {
      astForExpressionOrCommandContext(ctx.expressionOrCommand())
    } else {
      Ast()
    }
  }

  def astForSingletonMethodNamePartContext(ctx: RubyParser.SingletonMethodNamePartContext): Ast = {
    if (ctx == null) return Ast()
    val definedMethodNameAst = astForDefinedMethodNameContext(ctx.definedMethodName())
    val singletonObjAst      = astForSingletonObjextContext(ctx.singletonObject())
    Ast().withChildren(Seq[Ast](definedMethodNameAst, singletonObjAst))
  }

  def astForMethodNamePartContext(ctx: RubyParser.MethodNamePartContext): Ast = {
    if (ctx == null) return Ast()

    ctx match {
      case ctx: RubyParser.SimpleMethodNamePartContext =>
        astForSimpleMethodNamePartContext(ctx)
      case ctx: RubyParser.SingletonMethodNamePartContext =>
        astForSingletonMethodNamePartContext(ctx)
      case _ =>
        Ast()
    }
  }
  def astForMethodParameterPartContext(ctx: RubyParser.MethodParameterPartContext): Ast = {
    if (ctx == null || ctx.parameters() == null) return Ast()
    // NOT differentiating between the productions here since either way we get paramaters
    val mandatoryParameters = ctx.parameters().mandatoryParameters()
    val optionalParameters  = ctx.parameters().optionalParameters()
    val arrayParameter      = ctx.parameters().arrayParameter()
    val procParameter       = ctx.parameters().procParameter()

    val localVarList = ListBuffer[TerminalNode]()

    if (mandatoryParameters != null) {
      mandatoryParameters
        .LOCAL_VARIABLE_IDENTIFIER()
        .forEach(localVar => {
          localVarList.addOne(localVar)
        })
    }

    if (optionalParameters != null) {
      val optionalParameterList = optionalParameters.optionalParameter()
      optionalParameterList.forEach(param => {
        localVarList.addOne(param.LOCAL_VARIABLE_IDENTIFIER())
      })
    }

    if (arrayParameter != null) {
      localVarList.addOne(arrayParameter.LOCAL_VARIABLE_IDENTIFIER())
    }

    if (procParameter != null) {
      localVarList.addOne(procParameter.LOCAL_VARIABLE_IDENTIFIER())
    }

    val seqNodes = localVarList
      .map(localVar => {
        val varSymbol = localVar.getSymbol()
        identifierNode(
          varSymbol.getText,
          None,
          Some(varSymbol.getLine()),
          Some(varSymbol.getCharPositionInLine()),
          List(Defines.Any)
        ).typeFullName(Defines.Any)
      })
      .toSeq

    Ast(seqNodes)
  }

  def astForBodyStatementContext(ctx: RubyParser.BodyStatementContext): Ast = {
    if (ctx == null) return Ast()
    astForStatementsContext(ctx.compoundStatement().statements())
  }

  def astForMethodDefinitionContext(ctx: RubyParser.MethodDefinitionContext): Ast = {
    if (ctx == null) return Ast()
    val astMethodName  = astForMethodNamePartContext(ctx.methodNamePart())
    val astMethodParam = astForMethodParameterPartContext(ctx.methodParameterPart())
    val astBody        = astForBodyStatementContext(ctx.bodyStatement())

    val blockNode = NewBlock().typeFullName(Defines.Any)
    Ast(blockNode).withChildren(Seq[Ast](astMethodName, astMethodParam, astBody))
  }

  def astForMethodOnlyIdentifierPrimaryContext(ctx: RubyParser.MethodOnlyIdentifierPrimaryContext): Ast = {
    if (ctx == null) return Ast()
    Ast()
  }

  def astForModuleDefinitionPrimaryContext(ctx: RubyParser.ModuleDefinitionPrimaryContext): Ast = {
    if (ctx == null) return Ast()
    Ast()
  }

  def astForMultipleAssignmentExpressionContext(ctx: RubyParser.MultipleAssignmentExpressionContext): Ast = {
    if (ctx == null) return Ast()
    Ast()
  }

  def astForMultiplicativeExpressionContext(ctx: RubyParser.MultiplicativeExpressionContext): Ast = {
    if (ctx == null) return Ast()
    astForBinaryExpression(ctx.expression().get(0), ctx.expression().get(1), ctx.op)
  }

  def astForNotExpressionOrCommandContext(ctx: RubyParser.NotExpressionOrCommandContext): Ast = {
    if (ctx == null) return Ast()
    Ast()
  }

  def astForOperatorAndExpressionContext(ctx: RubyParser.OperatorAndExpressionContext): Ast = {
    if (ctx == null) return Ast()
    astForBinaryExpression(ctx.expression().get(0), ctx.expression().get(1), ctx.op)
  }

  def astForOperatorOrExpressionContext(ctx: RubyParser.OperatorOrExpressionContext): Ast = {
    if (ctx == null) return Ast()
    astForBinaryExpression(ctx.expression().get(0), ctx.expression().get(1), ctx.op)
  }

  def astForOrAndExpressionOrCommandContext(ctx: RubyParser.OrAndExpressionOrCommandContext): Ast = {
    if (ctx == null) return Ast()
    Ast()
  }

  def astForPowerExpressionContext(ctx: RubyParser.PowerExpressionContext): Ast = {
    if (ctx == null) return Ast()

    val expressions           = ctx.expression()
    val baseExpressionAst     = astForExpressionContext(expressions.get(0))
    val exponentExpressionAst = astForExpressionContext(expressions.get(1))
    // TODO create a method Ast
    val blockNode = NewBlock().typeFullName(Defines.Any)
    Ast(blockNode).withChildren(Seq[Ast](baseExpressionAst, exponentExpressionAst))
  }

  def astForRangeExpressionContext(ctx: RubyParser.RangeExpressionContext): Ast = {
    if (ctx == null) return Ast()
    astForBinaryExpression(ctx.expression().get(0), ctx.expression().get(1), ctx.op)
  }

  def astForRelationalExpressionContext(ctx: RubyParser.RelationalExpressionContext): Ast = {
    if (ctx == null) return Ast()
    astForBinaryExpression(ctx.expression().get(0), ctx.expression().get(1), ctx.op)
  }

  def astForBinaryExpression(
    lhs: RubyParser.ExpressionContext,
    rhs: RubyParser.ExpressionContext,
    operatorToken: Token
  ): Ast = {
    val lhsExpressionAst = astForExpressionContext(lhs)
    val rhsExpressionAst = astForExpressionContext(rhs)
    // TODO create a method Ast
    val blockNode = NewBlock().typeFullName(Defines.Any)
    Ast(blockNode).withChildren(Seq[Ast](lhsExpressionAst, rhsExpressionAst))
  }

  def astForSimpleScopedConstantReferencePrimaryContext(
    ctx: RubyParser.SimpleScopedConstantReferencePrimaryContext
  ): Ast = {
    if (ctx == null) return Ast()
    Ast()
  }

  def astForSuperExpressionPrimaryContext(ctx: RubyParser.SuperExpressionPrimaryContext): Ast = {
    if (ctx == null) return Ast()
    Ast()
  }

  def astForUnaryExpressionContext(ctx: RubyParser.UnaryExpressionContext): Ast = {
    if (ctx == null) return Ast()
    val expressionAst = astForExpressionContext(ctx.expression())
    val operatorToken = ctx.op
    // TODO create a method Ast
    val blockNode = NewBlock().typeFullName(Defines.Any)
    Ast(blockNode).withChild(expressionAst)
  }

  def astForUnaryMinusExpressionContext(ctx: RubyParser.UnaryMinusExpressionContext): Ast = {
    if (ctx == null) return Ast()
    Ast()
  }

  def astForUnlessExpressionPrimaryContext(ctx: RubyParser.UnlessExpressionPrimaryContext): Ast = {
    if (ctx == null) return Ast()
    Ast()
  }

  def astForUntilExpressionPrimaryContext(ctx: RubyParser.UntilExpressionPrimaryContext): Ast = {
    if (ctx == null) return Ast()
    Ast()
  }

  def astForPseudoVariableIdentifierContext(ctx: RubyParser.PseudoVariableIdentifierContext): Ast = {
    if (ctx == null) return Ast()
    Ast()
  }

  def astForVariableRefenceContext(ctx: RubyParser.VariableReferenceContext): Ast = {
    if (ctx == null) return Ast()
    val ast =
      if (ctx.variableIdentifier() != null) {
        astForVariableIdentifierContext(ctx.variableIdentifier(), Defines.Any)
      } else if (ctx.pseudoVariableIdentifier() != null) {
        astForPseudoVariableIdentifierContext(ctx.pseudoVariableIdentifier())
      } else {
        Ast()
      }

    val blockNode = NewBlock().typeFullName(Defines.Any)
    Ast(blockNode).withChild(ast)
  }

  def astForVariableReferencePrimaryContext(ctx: RubyParser.VariableReferencePrimaryContext): Ast = {
    if (ctx == null) return Ast()
    astForVariableRefenceContext(ctx.variableReference())
  }

  def astForWhileExpressionPrimaryContext(ctx: RubyParser.WhileExpressionPrimaryContext): Ast = {
    if (ctx == null) return Ast()
    Ast()
  }

  def astForYieldWithOptionalArgumentPrimaryContext(ctx: RubyParser.YieldWithOptionalArgumentPrimaryContext): Ast = {
    if (ctx == null) return Ast()
    Ast()
  }
}
