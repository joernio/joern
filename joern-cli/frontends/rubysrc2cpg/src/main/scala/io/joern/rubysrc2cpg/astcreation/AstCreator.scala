package io.joern.rubysrc2cpg.astcreation
import io.joern.rubysrc2cpg.parser.RubyParser._
import io.joern.rubysrc2cpg.parser.{RubyLexer, RubyParser}
import io.joern.x2cpg.Ast.storeInDiffGraph
import io.joern.x2cpg.datastructures.Global
import io.joern.x2cpg.utils.NodeBuilders.identifierNode
import io.joern.x2cpg.{Ast, AstCreatorBase}
import io.shiftleft.codepropertygraph.generated.nodes._
import org.antlr.v4.runtime.tree.TerminalNode
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream, Token}
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate

import java.util
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.CollectionHasAsScala

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

  def astForVariableIdentifierContext(ctx: VariableIdentifierContext, varType: String): Ast = {
    val token        = ctx.children.asScala.map(_.asInstanceOf[TerminalNode].getSymbol).head
    val variableName = token.getText
    val line         = token.getLine
    val column       = token.getCharPositionInLine
    val node         = identifierNode(variableName, None, Some(line), Some(column), List(varType)).typeFullName(varType)
    Ast(node)
  }

  def astForSingleLeftHandSideContext(ctx: SingleLeftHandSideContext, rhsRetType: String): Ast = ctx match {
    case ctx: VariableIdentifierOnlyContext => astForVariableIdentifierContext(ctx.variableIdentifier(), rhsRetType)
    case ctx: PrimaryInsideBracketsContext  => astForPrimaryContext(ctx.primary())
    case ctx: XdotyContext =>
      val xAst = astForPrimaryContext(ctx.primary())
      val yAst = {
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
      xAst.withChild(yAst)
    case ctx: ScopedConstantAccessContext =>
      Ast()
    case _ =>
      logger.error("astForSingleLeftHandSideContext() All contexts mismatched.")
      Ast()

  }

  def astForExpressionOrCommandsContext(ctx: ExpressionOrCommandsContext): Ast = {
    val asts = ctx.expressionOrCommand().asScala.map(astForExpressionOrCommandContext)
    Ast().withChildren(asts.toSeq)
  }

  def astForSplattingArgumentContext(ctx: SplattingArgumentContext): Ast = {
    if (ctx == null) return Ast()
    astForExpressionOrCommandContext(ctx.expressionOrCommand())
  }

  def astForMultipleRightHandSideContext(ctx: MultipleRightHandSideContext): (Ast, String) = {
    if (ctx == null) return (Ast(), Defines.Any)

    val exprAst      = astForExpressionOrCommandsContext(ctx.expressionOrCommands())
    val splattingAst = astForSplattingArgumentContext(ctx.splattingArgument())
    val seqAsts      = Seq[Ast](exprAst, splattingAst)
    (Ast().withChildren(seqAsts), Defines.Any)
  }

  def astForSingleAssignmentExpressionContext(ctx: SingleAssignmentExpressionContext): Ast = {
    val (rightAst, rhsRetType) = astForMultipleRightHandSideContext(ctx.multipleRightHandSide())
    val leftAst                = astForSingleLeftHandSideContext(ctx.singleLeftHandSide(), rhsRetType)
    val seqAsts                = Seq[Ast](leftAst, rightAst)
    val blockNode              = NewBlock().typeFullName(Defines.Any)
    Ast(blockNode).withChildren(seqAsts)
  }

  def astForPrimaryContext(ctx: PrimaryContext): Ast = ctx match {
    case ctx: ClassDefinitionPrimaryContext           => astForClassDefinitionPrimaryContext(ctx)
    case ctx: ModuleDefinitionPrimaryContext          => astForModuleDefinitionPrimaryContext(ctx)
    case ctx: MethodDefinitionPrimaryContext          => astForMethodDefinitionContext(ctx.methodDefinition())
    case ctx: YieldWithOptionalArgumentPrimaryContext => astForYieldWithOptionalArgumentPrimaryContext(ctx)
    case ctx: IfExpressionPrimaryContext              => astForIfExpressionPrimaryContext(ctx)
    case ctx: UnlessExpressionPrimaryContext          => astForUnlessExpressionPrimaryContext(ctx)
    case ctx: CaseExpressionPrimaryContext            => astForCaseExpressionPrimaryContext(ctx)
    case ctx: WhileExpressionPrimaryContext           => astForWhileExpressionPrimaryContext(ctx)
    case ctx: UntilExpressionPrimaryContext           => astForUntilExpressionPrimaryContext(ctx)
    case ctx: ForExpressionPrimaryContext             => astForForExpressionPrimaryContext(ctx)
    case ctx: JumpExpressionPrimaryContext            => astForJumpExpressionPrimaryContext(ctx)
    case ctx: BeginExpressionPrimaryContext           => astForBeginExpressionPrimaryContext(ctx)
    case ctx: GroupingExpressionPrimaryContext        => astForGroupingExpressionPrimaryContext(ctx)
    case ctx: VariableReferencePrimaryContext         => astForVariableReferencePrimaryContext(ctx)
    case ctx: SimpleScopedConstantReferencePrimaryContext =>
      astForSimpleScopedConstantReferencePrimaryContext(ctx)
    case ctx: ChainedScopedConstantReferencePrimaryContext =>
      astForChainedScopedConstantReferencePrimaryContext(ctx)
    case ctx: ArrayConstructorPrimaryContext          => astForArrayConstructorPrimaryContext(ctx)
    case ctx: HashConstructorPrimaryContext           => astForHashConstructorPrimaryContext(ctx)
    case ctx: LiteralPrimaryContext                   => astForLiteralPrimaryContext(ctx)
    case ctx: IsDefinedPrimaryContext                 => astForIsDefinedPrimaryContext(ctx)
    case ctx: SuperExpressionPrimaryContext           => astForSuperExpressionPrimaryContext(ctx)
    case ctx: IndexingExpressionPrimaryContext        => astForIndexingExpressionPrimaryContext(ctx)
    case ctx: MethodOnlyIdentifierPrimaryContext      => astForMethodOnlyIdentifierPrimaryContext(ctx)
    case ctx: InvocationWithBlockOnlyPrimaryContext   => astForInvocationWithBlockOnlyPrimaryContext(ctx)
    case ctx: InvocationWithParenthesesPrimaryContext => astForInvocationWithParenthesesPrimaryContext(ctx)
    case ctx: ChainedInvocationPrimaryContext         => astForChainedInvocationPrimaryContext(ctx)
    case ctx: ChainedInvocationWithoutArgumentsPrimaryContext =>
      astForChainedInvocationWithoutArgumentsPrimaryContext(ctx)
    case _ =>
      logger.error("astForPrimaryContext() All contexts mismatched.")
      Ast()
  }

  def astForExpressionContext(ctx: ExpressionContext): Ast = ctx match {
    case ctx: PrimaryExpressionContext             => astForPrimaryContext(ctx.primary())
    case ctx: UnaryExpressionContext               => astForUnaryExpressionContext(ctx)
    case ctx: PowerExpressionContext               => astForPowerExpressionContext(ctx)
    case ctx: UnaryMinusExpressionContext          => astForUnaryMinusExpressionContext(ctx)
    case ctx: MultiplicativeExpressionContext      => astForMultiplicativeExpressionContext(ctx)
    case ctx: AdditiveExpressionContext            => astForAdditiveExpressionContext(ctx)
    case ctx: BitwiseShiftExpressionContext        => astForBitwiseShiftExpressionContext(ctx)
    case ctx: BitwiseAndExpressionContext          => astForBitwiseAndExpressionContext(ctx)
    case ctx: BitwiseOrExpressionContext           => astForBitwiseOrExpressionContext(ctx)
    case ctx: RelationalExpressionContext          => astForRelationalExpressionContext(ctx)
    case ctx: EqualityExpressionContext            => astForEqualityExpressionContext(ctx)
    case ctx: OperatorAndExpressionContext         => astForOperatorAndExpressionContext(ctx)
    case ctx: OperatorOrExpressionContext          => astForOperatorOrExpressionContext(ctx)
    case ctx: RangeExpressionContext               => astForRangeExpressionContext(ctx)
    case ctx: ConditionalOperatorExpressionContext => astForConditionalOperatorExpressionContext(ctx)
    case ctx: SingleAssignmentExpressionContext    => astForSingleAssignmentExpressionContext(ctx)
    case ctx: MultipleAssignmentExpressionContext  => astForMultipleAssignmentExpressionContext(ctx)
    case ctx: IsDefinedExpressionContext           => astForIsDefinedExpressionContext(ctx)
    case _ =>
      logger.error("astForExpressionContext() All contexts mismatched.")
      Ast()
  }

  def astForExpressionOrCommandContext(ctx: ExpressionOrCommandContext): Ast = {
    if (ctx == null) return Ast()

    ctx match {
      case ctx: InvocationExpressionOrCommandContext => astForInvocationExpressionOrCommandContext(ctx)
      case ctx: NotExpressionOrCommandContext        => astForNotExpressionOrCommandContext(ctx)
      case ctx: OrAndExpressionOrCommandContext      => astForOrAndExpressionOrCommandContext(ctx)
      case ctx: ExpressionExpressionOrCommandContext => astForExpressionContext(ctx.expression())
      case _ =>
        logger.error("astForExpressionOrCommandContext() All contexts mismatched.")
        Ast()
    }
  }

  def astForSymbolContext(ctx: SymbolContext): Ast = {
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

  def astForDefinedMethodNameOrSymbolContext(ctx: DefinedMethodNameOrSymbolContext): Ast = {
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

  def astForAliasStatementContext(ctx: AliasStatementContext): Ast = {
    if (ctx == null) return Ast()

    val asts = ListBuffer[Ast]()
    ctx
      .definedMethodNameOrSymbol()
      .forEach(dms => {
        asts.addOne(astForDefinedMethodNameOrSymbolContext(dms))
      })

    Ast().withChildren(asts)
  }

  def astForUndefStatementContext(ctx: UndefStatementContext): Ast = {
    Ast()
  }

  def astForBeginStatementContext(ctx: BeginStatementContext): Ast = {
    val astStmts  = astForStatementsContext(ctx.statements())
    val blockNode = NewBlock().typeFullName(Defines.Any)
    Ast(blockNode).withChild(astStmts)
  }

  def astForEndStatementContext(ctx: EndStatementContext): Ast = {
    val astStmts  = astForStatementsContext(ctx.statements())
    val blockNode = NewBlock().typeFullName(Defines.Any)
    Ast(blockNode).withChild(astStmts)
  }

  def astForModifierStatementContext(ctx: ModifierStatementContext): Ast = {
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

  def astForStatementContext(ctx: StatementContext): Ast = ctx match {
    case ctx: AliasStatementContext               => astForAliasStatementContext(ctx)
    case ctx: UndefStatementContext               => astForUndefStatementContext(ctx)
    case ctx: BeginStatementContext               => astForBeginStatementContext(ctx)
    case ctx: EndStatementContext                 => astForEndStatementContext(ctx)
    case ctx: ModifierStatementContext            => astForModifierStatementContext(ctx)
    case ctx: ExpressionOrCommandStatementContext => astForExpressionOrCommandContext(ctx.expressionOrCommand())
    case _ =>
      logger.error("astForStatementContext() All contexts mismatched.")
      Ast()
  }

  def astForStatementsContext(ctx: StatementsContext): Ast = {
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

  def astForAdditiveExpressionContext(ctx: AdditiveExpressionContext): Ast = {
    astForBinaryExpression(ctx.expression(0), ctx.expression(1), ctx.op)
  }

  def astForArrayConstructorPrimaryContext(ctx: ArrayConstructorPrimaryContext): Ast = {
    Ast()
  }

  def astForBeginExpressionPrimaryContext(ctx: BeginExpressionPrimaryContext): Ast = {
    Ast()
  }

  def astForBitwiseAndExpressionContext(ctx: BitwiseAndExpressionContext): Ast = {
    astForBinaryExpression(ctx.expression(0), ctx.expression(1), ctx.op)
  }

  def astForBitwiseOrExpressionContext(ctx: BitwiseOrExpressionContext): Ast = {
    astForBinaryExpression(ctx.expression(0), ctx.expression(1), ctx.op)
  }

  def astForBitwiseShiftExpressionContext(ctx: BitwiseShiftExpressionContext): Ast = {
    astForBinaryExpression(ctx.expression(0), ctx.expression(1), ctx.op)
  }

  def astForWhenArgumentContext(ctx: WhenArgumentContext): Ast = {
    val expAsts = if (ctx.expressions() != null) {
      ctx
        .expressions()
        .expression()
        .asScala
        .map(exp => {
          astForExpressionContext(exp)
        })
        .toSeq
    } else {
      Seq[Ast]()
    }
    val splatAst = astForSplattingArgumentContext(ctx.splattingArgument())
    Ast().withChildren(expAsts).withChild(splatAst)
  }

  def astForCaseExpressionPrimaryContext(ctx: CaseExpressionPrimaryContext): Ast = {
    val exprCmdAst = astForExpressionOrCommandContext(ctx.caseExpression().expressionOrCommand())
    val caseAsts = ctx
      .caseExpression()
      .whenClause()
      .asScala
      .map(wh => {
        val thenAst = astForThenClauseContext(wh.thenClause())
        val whenAst = astForWhenArgumentContext(wh.whenArgument())
        whenAst.withChild(thenAst)
      })
      .toSeq

    val elseAst = astForElseClauseContext(ctx.caseExpression().elseClause())
    exprCmdAst.withChildren(caseAsts).withChild(elseAst)
  }

  def astForChainedInvocationPrimaryContext(ctx: ChainedInvocationPrimaryContext): Ast = {
    Ast()
  }

  def astForChainedInvocationWithoutArgumentsPrimaryContext(
    ctx: ChainedInvocationWithoutArgumentsPrimaryContext
  ): Ast = {
    Ast()
  }

  def astForChainedScopedConstantReferencePrimaryContext(ctx: ChainedScopedConstantReferencePrimaryContext): Ast = {
    Ast()
  }

  def astForScopedConstantReferenceContext(ctx: ScopedConstantReferenceContext): Ast = {
    val primaryAst = astForPrimaryContext(ctx.primary())
    // TODO handle ctx.CONSTANT_IDENTIFIER()
    primaryAst
  }

  def astForClassOrModuleReferenceContext(ctx: ClassOrModuleReferenceContext): Ast = {
    if (ctx.scopedConstantReference() != null) {
      astForScopedConstantReferenceContext(ctx.scopedConstantReference())
    } else {
      Ast()
    }
  }

  def astForClassDefinitionPrimaryContext(ctx: ClassDefinitionPrimaryContext): Ast = {
    val astClassOrModuleRef = astForClassOrModuleReferenceContext(ctx.classDefinition().classOrModuleReference())
    val astExprOfCommand    = astForExpressionOrCommandContext(ctx.classDefinition().expressionOrCommand())
    val astBodyStatement    = astForBodyStatementContext(ctx.classDefinition().bodyStatement())

    Ast().withChildren(Seq[Ast](astClassOrModuleRef, astExprOfCommand, astBodyStatement))
  }

  def astForConditionalOperatorExpressionContext(ctx: ConditionalOperatorExpressionContext): Ast = {
    val ifConditionAst = astForExpressionContext(ctx.expression().get(0))
    val thenAst        = astForExpressionContext(ctx.expression().get(1))
    val elseAst        = astForExpressionContext(ctx.expression().get(2))

    val blockNode = NewBlock().typeFullName(Defines.Any)
    Ast(blockNode).withChildren(Seq[Ast](ifConditionAst, thenAst, elseAst))
  }

  def astForEqualityExpressionContext(ctx: EqualityExpressionContext): Ast = {
    astForBinaryExpression(ctx.expression(0), ctx.expression(1), ctx.op)
  }

  def astForGroupedLeftHandSideContext(ctx: GroupedLeftHandSideContext): Ast = {
    astForMultipleLeftHandSideContext(ctx.multipleLeftHandSide())
  }

  def astForMultipleLeftHandSideContext(ctx: MultipleLeftHandSideContext): Ast = {
    val asts = ctx
      .multipleLeftHandSideItem()
      .asScala
      .map(item => {
        if (item.singleLeftHandSide() != null) {
          astForSingleLeftHandSideContext(item.singleLeftHandSide(), Defines.Any)
        } else {
          astForGroupedLeftHandSideContext(item.groupedLeftHandSide())
        }
      })
      .toSeq
    Ast().withChildren(asts)
  }

  def astForForVariableContext(ctx: ForVariableContext): Ast = {
    if (ctx.singleLeftHandSide() != null) {
      astForSingleLeftHandSideContext(ctx.singleLeftHandSide(), Defines.Any)
    } else if (ctx.multipleLeftHandSide() != null) {
      astForMultipleLeftHandSideContext(ctx.multipleLeftHandSide())
    } else {
      Ast()
    }
  }

  def astForForExpressionPrimaryContext(ctx: ForExpressionPrimaryContext): Ast = {
    val forVarAst   = astForForVariableContext(ctx.forExpression().forVariable())
    val exprCmdAst  = astForExpressionOrCommandContext(ctx.forExpression().expressionOrCommand())
    val doClauseAst = astForDoClauseContext(ctx.forExpression().doClause())
    val blockNode   = NewBlock().typeFullName(Defines.Any)
    Ast(blockNode).withChildren(Seq[Ast](forVarAst, exprCmdAst)).withChild(doClauseAst)
  }

  def astForGroupingExpressionPrimaryContext(ctx: GroupingExpressionPrimaryContext): Ast = {
    astForStatementsContext(ctx.compoundStatement().statements())
  }

  def astForHashConstructorPrimaryContext(ctx: HashConstructorPrimaryContext): Ast = {
    Ast()
  }

  def astForThenClauseContext(ctx: ThenClauseContext): Ast = {
    astForStatementsContext(ctx.compoundStatement().statements())
  }

  def astForElsifClauseContext(ctx: util.List[ElsifClauseContext]): Seq[Ast] = {
    if (ctx == null) return Seq[Ast]()

    ctx.asScala
      .map(elif => {
        val thenAst   = astForThenClauseContext(elif.thenClause())
        val expCmdAst = astForExpressionOrCommandContext(elif.expressionOrCommand())
        val blockNode = NewBlock().typeFullName(Defines.Any)
        Ast(blockNode).withChildren(Seq[Ast](thenAst, expCmdAst))
      })
      .toSeq
  }

  def astForElseClauseContext(ctx: ElseClauseContext): Ast = {
    if (ctx == null) return Ast()
    astForStatementsContext(ctx.compoundStatement().statements())
  }

  def astForIfExpressionContext(ctx: IfExpressionContext): Ast = {
    val ifAst      = astForExpressionOrCommandContext(ctx.expressionOrCommand())
    val thenAst    = astForThenClauseContext(ctx.thenClause())
    val elseifAsts = astForElsifClauseContext(ctx.elsifClause())
    val elseAst    = astForElseClauseContext(ctx.elseClause())
    ifAst.withChildren(Seq[Ast](thenAst, elseAst)).withChildren(elseifAsts)
  }

  def astForIfExpressionPrimaryContext(ctx: IfExpressionPrimaryContext): Ast = {
    astForIfExpressionContext(ctx.ifExpression())
  }

  def astForIndexingExpressionPrimaryContext(ctx: IndexingExpressionPrimaryContext): Ast = {
    Ast()
  }

  def astForInvocationExpressionOrCommandContext(ctx: InvocationExpressionOrCommandContext): Ast = {
    Ast()
  }

  def astForInvocationWithBlockOnlyPrimaryContext(ctx: InvocationWithBlockOnlyPrimaryContext): Ast = {
    Ast()
  }

  def astForInvocationWithParenthesesPrimaryContext(ctx: InvocationWithParenthesesPrimaryContext): Ast = {
    Ast()
  }

  def astForIsDefinedExpressionContext(ctx: IsDefinedExpressionContext): Ast = {
    Ast()
  }

  def astForIsDefinedPrimaryContext(ctx: IsDefinedPrimaryContext): Ast = {
    Ast()
  }

  def astForJumpExpressionPrimaryContext(ctx: JumpExpressionPrimaryContext): Ast = {
    Ast()
  }

  def astForLiteralPrimaryContext(ctx: LiteralPrimaryContext): Ast = {
    if (ctx.literal().numericLiteral() != null) {
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

  def astForSimpleMethodNamePartContext(ctx: SimpleMethodNamePartContext): Ast = {
    astForDefinedMethodNameContext(ctx.definedMethodName())
  }

  def astForMethodOnlyIdentifier(ctx: MethodOnlyIdentifierContext): Ast = {
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

  def astForMethodIdentifierContext(ctx: MethodIdentifierContext): Ast = {
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

  def astForMethodNameContext(ctx: MethodNameContext): Ast = {
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
  def astForAssignmentLikeMethodIdentifierContext(ctx: AssignmentLikeMethodIdentifierContext): Ast = {
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

  def astForDefinedMethodNameContext(ctx: DefinedMethodNameContext): Ast = {
    val methodNameAst         = astForMethodNameContext(ctx.methodName())
    val assignLinkedMethodAst = astForAssignmentLikeMethodIdentifierContext(ctx.assignmentLikeMethodIdentifier())
    Ast().withChildren(Seq[Ast](methodNameAst, assignLinkedMethodAst))
  }

  def astForSingletonObjextContext(ctx: SingletonObjectContext): Ast = {
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

  def astForSingletonMethodNamePartContext(ctx: SingletonMethodNamePartContext): Ast = {
    val definedMethodNameAst = astForDefinedMethodNameContext(ctx.definedMethodName())
    val singletonObjAst      = astForSingletonObjextContext(ctx.singletonObject())
    Ast().withChildren(Seq[Ast](definedMethodNameAst, singletonObjAst))
  }

  def astForMethodNamePartContext(ctx: MethodNamePartContext): Ast = ctx match {
    case ctx: SimpleMethodNamePartContext    => astForSimpleMethodNamePartContext(ctx)
    case ctx: SingletonMethodNamePartContext => astForSingletonMethodNamePartContext(ctx)
    case _ =>
      logger.error("astForMethodNamePartContext() All contexts mismatched.")
      Ast()
  }

  def astForMethodParameterPartContext(ctx: MethodParameterPartContext): Ast = {
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

  def astForBodyStatementContext(ctx: BodyStatementContext): Ast = {
    astForStatementsContext(ctx.compoundStatement().statements())
  }

  def astForMethodDefinitionContext(ctx: MethodDefinitionContext): Ast = {
    val astMethodName  = astForMethodNamePartContext(ctx.methodNamePart())
    val astMethodParam = astForMethodParameterPartContext(ctx.methodParameterPart())
    val astBody        = astForBodyStatementContext(ctx.bodyStatement())

    val blockNode = NewBlock().typeFullName(Defines.Any)
    Ast(blockNode).withChildren(Seq[Ast](astMethodName, astMethodParam, astBody))
  }

  def astForMethodOnlyIdentifierPrimaryContext(ctx: MethodOnlyIdentifierPrimaryContext): Ast = {
    Ast()
  }

  def astForModuleDefinitionPrimaryContext(ctx: ModuleDefinitionPrimaryContext): Ast = {
    val referenceAst = astForClassOrModuleReferenceContext(ctx.moduleDefinition().classOrModuleReference())
    val bodyStmtAst  = astForBodyStatementContext(ctx.moduleDefinition().bodyStatement())
    referenceAst.withChild(bodyStmtAst)
  }

  def astForMultipleAssignmentExpressionContext(ctx: MultipleAssignmentExpressionContext): Ast = {
    Ast()
  }

  def astForMultiplicativeExpressionContext(ctx: MultiplicativeExpressionContext): Ast = {
    astForBinaryExpression(ctx.expression(0), ctx.expression(1), ctx.op)
  }

  def astForNotExpressionOrCommandContext(ctx: NotExpressionOrCommandContext): Ast = {
    Ast()
  }

  def astForOperatorAndExpressionContext(ctx: OperatorAndExpressionContext): Ast = {
    astForBinaryExpression(ctx.expression(0), ctx.expression(1), ctx.op)
  }

  def astForOperatorOrExpressionContext(ctx: OperatorOrExpressionContext): Ast = {
    astForBinaryExpression(ctx.expression(0), ctx.expression(1), ctx.op)
  }

  def astForOrAndExpressionOrCommandContext(ctx: OrAndExpressionOrCommandContext): Ast = {
    Ast()
  }

  def astForPowerExpressionContext(ctx: PowerExpressionContext): Ast = {
    val expressions           = ctx.expression()
    val baseExpressionAst     = astForExpressionContext(expressions.get(0))
    val exponentExpressionAst = astForExpressionContext(expressions.get(1))
    // TODO create a method Ast
    val blockNode = NewBlock().typeFullName(Defines.Any)
    Ast(blockNode).withChildren(Seq[Ast](baseExpressionAst, exponentExpressionAst))
  }

  def astForRangeExpressionContext(ctx: RangeExpressionContext): Ast = {
    if (ctx.expression().size() == 2) {
      astForBinaryExpression(ctx.expression(0), ctx.expression(1), ctx.op)
    } else {
      Ast()
    }
  }

  def astForRelationalExpressionContext(ctx: RelationalExpressionContext): Ast = {
    astForBinaryExpression(ctx.expression(0), ctx.expression(1), ctx.op)
  }

  def astForBinaryExpression(lhs: ExpressionContext, rhs: ExpressionContext, operatorToken: Token): Ast = {
    val lhsExpressionAst = astForExpressionContext(lhs)
    val rhsExpressionAst = astForExpressionContext(rhs)
    // TODO create a method Ast
    val blockNode = NewBlock().typeFullName(Defines.Any)
    Ast(blockNode).withChildren(Seq[Ast](lhsExpressionAst, rhsExpressionAst))
  }

  def astForSimpleScopedConstantReferencePrimaryContext(ctx: SimpleScopedConstantReferencePrimaryContext): Ast = {
    Ast()
  }

  def astForSuperExpressionPrimaryContext(ctx: SuperExpressionPrimaryContext): Ast = {
    Ast()
  }

  def astForUnaryExpressionContext(ctx: UnaryExpressionContext): Ast = {
    val expressionAst = astForExpressionContext(ctx.expression())
    val operatorToken = ctx.op
    // TODO create a method Ast
    val blockNode = NewBlock().typeFullName(Defines.Any)
    Ast(blockNode).withChild(expressionAst)
  }

  def astForUnaryMinusExpressionContext(ctx: UnaryMinusExpressionContext): Ast = {
    Ast()
  }

  def astForUnlessExpressionPrimaryContext(ctx: UnlessExpressionPrimaryContext): Ast = {
    val unlessAst = astForExpressionOrCommandContext(ctx.unlessExpression().expressionOrCommand())
    val thenAst   = astForThenClauseContext(ctx.unlessExpression().thenClause())
    val elseAst   = astForElseClauseContext(ctx.unlessExpression().elseClause())
    unlessAst.withChildren(Seq[Ast](thenAst, elseAst))
  }

  def astForUntilExpressionPrimaryContext(ctx: UntilExpressionPrimaryContext): Ast = {
    Ast()
  }

  def astForPseudoVariableIdentifierContext(ctx: PseudoVariableIdentifierContext): Ast = {
    Ast()
  }

  def astForVariableRefenceContext(ctx: RubyParser.VariableReferenceContext): Ast = {
    val ast = if (ctx.variableIdentifier() != null) {
      astForVariableIdentifierContext(ctx.variableIdentifier(), Defines.Any)
    } else {
      astForPseudoVariableIdentifierContext(ctx.pseudoVariableIdentifier())
    }

    val blockNode = NewBlock().typeFullName(Defines.Any)
    Ast(blockNode).withChild(ast)
  }

  def astForVariableReferencePrimaryContext(ctx: VariableReferencePrimaryContext): Ast = {
    astForVariableRefenceContext(ctx.variableReference())
  }

  def astForDoClauseContext(ctx: DoClauseContext): Ast = {
    astForStatementsContext(ctx.compoundStatement().statements())
  }

  def astForWhileExpressionPrimaryContext(ctx: WhileExpressionPrimaryContext): Ast = {
    val exprCmdAst  = astForExpressionOrCommandContext(ctx.whileExpression().expressionOrCommand())
    val doClauseAst = astForDoClauseContext(ctx.whileExpression().doClause())
    exprCmdAst.withChild(doClauseAst)
  }

  def astForBlockArgumentContext(ctx: BlockArgumentContext): Ast = {
    astForExpressionContext(ctx.expression())
  }

  def astForBlockArgumentTypeContext(ctx: BlockArgumentTypeContext): Ast = {
    astForBlockArgumentContext(ctx.blockArgument())
  }

  def astForBlockSplattingTypeContext(ctx: BlockSplattingTypeContext): Ast = {
    val blockArgAst = astForBlockArgumentContext(ctx.blockArgument())
    val splatAst    = astForSplattingArgumentContext(ctx.splattingArgument())
    Ast().withChildren(Seq[Ast](blockArgAst, splatAst))
  }

  def astForAssociationsContext(ctx: AssociationsContext) = {
    Ast()
  }

  def astForBlockSplattingExprAssocTypeContext(ctx: BlockSplattingExprAssocTypeContext): Ast = {
    val blockArgAst     = astForBlockArgumentContext(ctx.blockArgument())
    val splatAst        = astForSplattingArgumentContext(ctx.splattingArgument())
    val associationsAst = astForAssociationsContext(ctx.associations())
    val expAsts         = ctx.expressions().expression().asScala.map(exp => astForExpressionContext(exp)).toSeq
    val blockNodeExp    = NewBlock().typeFullName(Defines.Any)
    val expAst          = Ast(blockNodeExp).withChildren(expAsts)

    val blockNode = NewBlock().typeFullName(Defines.Any)
    Ast(blockNode).withChildren(Seq[Ast](blockArgAst, splatAst, associationsAst, expAst))
  }

  def astForBlockExprAssocTypeContext(ctx: BlockExprAssocTypeContext): Ast = {
    val blockArgAst = astForBlockArgumentContext(ctx.blockArgument())
    val assocOrExpAst = if (ctx.associations() != null) {
      astForAssociationsContext(ctx.associations())
    } else {
      val blockNodeExp = NewBlock().typeFullName(Defines.Any)
      val asts         = ctx.expressions().expression().asScala.map(exp => astForExpressionContext(exp)).toSeq
      Ast(blockNodeExp).withChildren(asts)
    }

    val blockNode = NewBlock().typeFullName(Defines.Any)
    Ast(blockNode).withChildren(Seq[Ast](blockArgAst, assocOrExpAst))
  }

  def astForArgumentsWithoutParenthesesContext(ctx: ArgumentsWithoutParenthesesContext): Ast = {
    astForArgumentsContext(ctx.arguments())
  }

  def astForCommandContext(ctx: CommandContext): Ast = {
    val argumentsWithoutParenAst = astForArgumentsWithoutParenthesesContext(ctx.argumentsWithoutParentheses())
    val blockNode                = NewBlock().typeFullName(Defines.Any)

    if (ctx.SUPER() != null) {
      Ast(blockNode).withChild(argumentsWithoutParenAst)
    } else if (ctx.YIELD() != null) {
      Ast(blockNode).withChild(argumentsWithoutParenAst)
    } else if (ctx.methodIdentifier() != null) {
      val methodIdentifierAst = astForMethodIdentifierContext(ctx.methodIdentifier())
      Ast(blockNode).withChildren(Seq[Ast](argumentsWithoutParenAst, methodIdentifierAst))
    } else if (ctx.primary() != null) {
      val primaryAst    = astForPrimaryContext(ctx.primary())
      val methodNameAst = astForMethodNameContext(ctx.methodName())
      Ast(blockNode).withChildren(Seq[Ast](primaryAst, methodNameAst, argumentsWithoutParenAst))
    } else {
      Ast()
    }
  }

  def astForCommandTypeContext(ctx: CommandTypeContext): Ast = {
    astForCommandContext(ctx.command())
  }

  def astForArgumentsContext(ctx: ArgumentsContext): Ast = ctx match {
    case ctx: BlockArgumentTypeContext           => astForBlockArgumentTypeContext(ctx)
    case ctx: BlockSplattingTypeContext          => astForBlockSplattingTypeContext(ctx)
    case ctx: BlockSplattingExprAssocTypeContext => astForBlockSplattingExprAssocTypeContext(ctx)
    case ctx: BlockExprAssocTypeContext          => astForBlockExprAssocTypeContext(ctx)
    case ctx: CommandTypeContext                 => astForCommandTypeContext(ctx)
    case _ =>
      logger.error("astForArgumentsContext() All contexts mismatched.")
      Ast()
  }

  def astForYieldWithOptionalArgumentContext(ctx: YieldWithOptionalArgumentContext): Ast = {
    astForArgumentsContext(ctx.arguments())
  }

  def astForYieldWithOptionalArgumentPrimaryContext(ctx: YieldWithOptionalArgumentPrimaryContext): Ast = {
    astForYieldWithOptionalArgumentContext(ctx.yieldWithOptionalArgument())
  }
}
