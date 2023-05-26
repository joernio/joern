package io.joern.rubysrc2cpg.astcreation
import io.joern.rubysrc2cpg.parser.RubyParser._
import io.joern.rubysrc2cpg.parser.{RubyLexer, RubyParser}
import io.joern.x2cpg.Ast.storeInDiffGraph
import io.joern.x2cpg.datastructures.Global
import io.joern.x2cpg.{Ast, AstCreatorBase, AstNodeBuilder}
import io.shiftleft.codepropertygraph.generated.{
  ControlStructureTypes,
  DispatchTypes,
  EdgeTypes,
  EvaluationStrategies,
  ModifierTypes,
  NodeTypes,
  Operators
}
import io.shiftleft.codepropertygraph.generated.nodes._
import org.antlr.v4.runtime.tree.TerminalNode
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream, Token}
import org.slf4j.LoggerFactory
import overflowdb.{BatchedUpdate, Node}

import java.util
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters._

class AstCreator(filename: String, global: Global)
    extends AstCreatorBase(filename)
    with AstNodeBuilder[TerminalNode, AstCreator] {

  object Defines {
    val Any: String     = "ANY"
    val Number: String  = "number"
    val String: String  = "string"
    val Boolean: String = "boolean"
    val Hash: String    = "hash"
    val Array: String   = "array"
    val Symbol: String  = "symbol"
  }

  object MethodFullNames {
    val UnknownFullName = "<unknownfullname>"
    val OperatorPrefix  = "<operator>."
  }

  private val logger = LoggerFactory.getLogger(this.getClass)

  private val classStack = mutable.Stack[String]()

  // Queue of variable identifiers incorrectly identified as method identifiers
  private val methodIdentiferQ = mutable.Queue[Ast]()

  class ScopeIdentifiers {
    val varToIdentiferMap             = mutable.HashMap[String, NewIdentifier]()
    var parentScope: ScopeIdentifiers = null
  }

  private val scopeStack = mutable.Stack[ScopeIdentifiers]()

  private def pushScope(isParentAccesible: Boolean = false): ScopeIdentifiers = {
    val scope = new ScopeIdentifiers()

    if (isParentAccesible) {
      scope.parentScope = scopeStack.top
    }
    scopeStack.push(scope)
    scope
  }

  private def popScope(): Unit = {
    scopeStack.pop()
  }

  private def setIdentiferInScope(node: NewIdentifier): Unit = {
    scopeStack.top.varToIdentiferMap.getOrElseUpdate(node.name, node)
  }

  private def lookupIdentiferInScope(name: String): Boolean = {
    scopeStack.top.varToIdentiferMap.contains(name)
  }

  private def createIdentiferWithScope(
    node: TerminalNode,
    name: String,
    code: String,
    typeFullName: String,
    dynamicTypeHints: Seq[String]
  ): NewIdentifier = {
    val newNode = identifierNode(node, name, code, typeFullName, dynamicTypeHints)
    setIdentiferInScope(newNode)
    newNode
  }

  override def createAst(): BatchedUpdate.DiffGraphBuilder = {
    val charStream  = CharStreams.fromFileName(filename)
    val lexer       = new RubyLexer(charStream)
    val tokenStream = new CommonTokenStream(lexer)
    val parser      = new RubyParser(tokenStream)
    val programCtx  = parser.program()

    val statementCtx = programCtx.compoundStatement().statements()
    pushScope()
    val statementAst = astForStatementsContext(statementCtx)
    popScope()

    val name = ":program"
    val programMethod =
      NewMethod()
        .order(1)
        .name(name)
        .code(name)
        .fullName(filename)
        .filename(filename)
        .lineNumber(0)
        .lineNumberEnd(-1)
        .columnNumber(0)
        .columnNumberEnd(-1)
        .astParentType(NodeTypes.TYPE_DECL)
        .astParentFullName(filename)

    val thisParam = NewMethodParameterIn()
      .name("this")
      .code("this")
      .lineNumber(0)
      .columnNumber(0)
    val thisParamAst = Ast(thisParam)

    val methodRetNode = NewMethodReturn()
      .lineNumber(None)
      .columnNumber(None)
      .typeFullName(Defines.Any)

    diffGraph.addEdge(programMethod, thisParam, EdgeTypes.AST)

    val blockNode = NewBlock().typeFullName(Defines.Any)
    val programAst =
      methodAst(programMethod, Seq[Ast](thisParamAst), blockAst(blockNode, List[Ast](statementAst)), methodRetNode)

    val fileNode       = NewFile().name(filename).order(1)
    val namespaceBlock = globalNamespaceBlock()
    val ast            = Ast(fileNode).withChild(Ast(namespaceBlock).withChild(programAst))

    storeInDiffGraph(ast, diffGraph)
    diffGraph
  }

  protected def line(node: TerminalNode): Option[Integer]      = Option(node.getSymbol.getLine)
  protected def column(node: TerminalNode): Option[Integer]    = Option(node.getSymbol.getCharPositionInLine)
  protected def lineEnd(node: TerminalNode): Option[Integer]   = None
  protected def columnEnd(node: TerminalNode): Option[Integer] = None

  private def registerType(typ: String): String = {
    if (typ != Defines.Any) {
      global.usedTypes.putIfAbsent(typ, true)
    }
    typ
  }
  def astForVariableIdentifierContext(ctx: VariableIdentifierContext): Ast = {
    val terminalNode = ctx.children.asScala.map(_.asInstanceOf[TerminalNode]).head
    val token        = terminalNode.getSymbol
    val variableName = token.getText
    val node         = createIdentiferWithScope(terminalNode, variableName, variableName, Defines.Any, List[String]())
    setIdentiferInScope(node)
    Ast(node)
  }

  def astForSingleLeftHandSideContext(ctx: SingleLeftHandSideContext): Ast = ctx match {
    case ctx: VariableIdentifierOnlySingleLeftHandSideContext =>
      astForVariableIdentifierContext(ctx.variableIdentifier())
    case ctx: PrimaryInsideBracketsSingleLeftHandSideContext =>
      val primaryAst = astForPrimaryContext(ctx.primary())
      val argsAst    = astForArgumentsContext(ctx.arguments())
      val callNode = NewCall()
        .name(Operators.indexAccess)
        .code(Operators.indexAccess)
        .methodFullName(Operators.indexAccess)
        .signature("")
        .dispatchType(DispatchTypes.STATIC_DISPATCH)
        .typeFullName(Defines.Any)
        .lineNumber(ctx.LBRACK().getSymbol.getLine)
        .columnNumber(ctx.LBRACK().getSymbol.getCharPositionInLine())
      callAst(callNode, Seq[Ast](primaryAst, argsAst))
    case ctx: XdotySingleLeftHandSideContext =>
      val xAst = astForPrimaryContext(ctx.primary())
      val localVar = {
        if (ctx.LOCAL_VARIABLE_IDENTIFIER() != null) {
          ctx.LOCAL_VARIABLE_IDENTIFIER()
        } else if (ctx.CONSTANT_IDENTIFIER() != null) {
          ctx.CONSTANT_IDENTIFIER()
        } else {
          null
        }
      }
      val varSymbol = localVar.getSymbol()
      val node =
        createIdentiferWithScope(localVar, varSymbol.getText, varSymbol.getText, Defines.Any, List(Defines.Any))
      val yAst = Ast(node)

      val callNode = NewCall()
        .name(Operators.fieldAccess)
        .code(Operators.fieldAccess)
        .methodFullName(Operators.fieldAccess)
        .signature("")
        .dispatchType(DispatchTypes.STATIC_DISPATCH)
        .typeFullName(Defines.Any)
        .lineNumber(localVar.getSymbol.getLine)
        .columnNumber(localVar.getSymbol.getCharPositionInLine())
      callAst(callNode, Seq[Ast](xAst, yAst))
    case ctx: ScopedConstantAccessSingleLeftHandSideContext =>
      Ast()
    case _ =>
      logger.error("astForSingleLeftHandSideContext() All contexts mismatched.")
      Ast()

  }

  def astForExpressionOrCommandsContext(ctx: ExpressionOrCommandsContext): Ast = {
    val asts = ctx.expressionOrCommand().asScala.map(astForExpressionOrCommandContext)
    if (asts.size == 1) {
      asts.head
    } else {
      Ast().withChildren(asts.toSeq)
    }
  }

  def astForSplattingArgumentContext(ctx: SplattingArgumentContext): Ast = {
    if (ctx == null) return Ast()
    astForExpressionOrCommandContext(ctx.expressionOrCommand())
  }

  def astForMultipleRightHandSideContext(ctx: MultipleRightHandSideContext): Seq[Ast] = {
    if (ctx == null) return Seq[Ast](Ast())

    val splattingAst = astForSplattingArgumentContext(ctx.splattingArgument())

    if (ctx.expressionOrCommands() != null) {
      val exprAst = astForExpressionOrCommandsContext(ctx.expressionOrCommands())
      Seq[Ast](exprAst) ++ Seq[Ast](splattingAst)
    } else {
      Seq[Ast](splattingAst)
    }
  }

  def astForSingleAssignmentExpressionContext(ctx: SingleAssignmentExpressionContext): Ast = {
    val rightAsts = astForMultipleRightHandSideContext(ctx.multipleRightHandSide())
    val leftAst   = astForSingleLeftHandSideContext(ctx.singleLeftHandSide())
    val callNode = NewCall()
      .name(ctx.op.getText)
      .code(ctx.op.getText)
      .methodFullName(MethodFullNames.OperatorPrefix + ctx.op.getText)
      .signature("")
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .typeFullName(Defines.Any)
      .lineNumber(ctx.op.getLine())
      .columnNumber(ctx.op.getCharPositionInLine())
    callAst(callNode, Seq[Ast](leftAst) ++ rightAsts)
  }

  def astForStringInterpolationPrimaryContext(ctx: StringInterpolationPrimaryContext): Ast = {
    val varAsts = ctx
      .stringInterpolation()
      .interpolation()
      .asScala
      .map(inter => {
        astForStatementsContext(inter.compoundStatement().statements())
      })
      .toSeq

    val nodes = ctx
      .stringInterpolation()
      .DOUBLE_QUOTED_STRING_CHARACTER_SEQUENCE()
      .asScala
      .map { substr =>
        {
          NewLiteral()
            .code(substr.getText)
            .typeFullName(Defines.String)
            .dynamicTypeHintFullName(List(Defines.String))
        }
      }
      .toSeq
    val strAst = Ast(nodes)
    Ast().withChildren(varAsts).merge(strAst)
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
    case ctx: StringInterpolationPrimaryContext       => astForStringInterpolationPrimaryContext(ctx)
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
    val text =
      if (ctx.SYMBOL_LITERAL() != null) {
        ctx.getText
      } else if (ctx.SINGLE_QUOTED_STRING_LITERAL() != null) {
        ctx.getText
      } else {
        return Ast()
      }

    val node = NewLiteral()
      .code(text)
      .typeFullName(Defines.String)
      .dynamicTypeHintFullName(List(Defines.String))
    Ast().withChildren(List[Ast](Ast(node)))
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
    // TODO to be implemented
    Ast()
  }

  def astForBeginStatementContext(ctx: BeginStatementContext): Ast = {
    val astStmts = astForStatementsContext(ctx.statements())
    Ast().withChild(astStmts)
  }

  def astForEndStatementContext(ctx: EndStatementContext): Ast = {
    val astStmts = astForStatementsContext(ctx.statements())
    Ast().withChild(astStmts)
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
    val asts = ctx
      .statement()
      .asScala
      .map(st => {
        astForStatementContext(st)
      })
      .toList

    blockAst(blockNode, asts)
  }

  def astForAdditiveExpressionContext(ctx: AdditiveExpressionContext): Ast = {
    astForBinaryExpression(ctx.expression(0), ctx.expression(1), ctx.op)
  }

  def astForIndexingArgumentsContext(ctx: IndexingArgumentsContext): Ast = ctx match {
    case ctx: RubyParser.CommandOnlyIndexingArgumentsContext =>
      astForCommandContext(ctx.command())
    case ctx: RubyParser.ExpressionsOnlyIndexingArgumentsContext =>
      val expAsts =
        ctx
          .expressions()
          .expression()
          .asScala
          .map(exp => {
            astForExpressionContext(exp)
          })
          .toSeq
      val callNode = NewCall()
        .name(Operators.arrayInitializer)
        .methodFullName(Operators.arrayInitializer)
        .signature(Operators.arrayInitializer)
        .typeFullName(MethodFullNames.UnknownFullName)
        .dispatchType(DispatchTypes.STATIC_DISPATCH)
        .code(ctx.getText)
        .lineNumber(-1)
        .columnNumber(-1)
      callAst(callNode, expAsts)
    case ctx: RubyParser.ExpressionsAndSplattingIndexingArgumentsContext =>
      val expAsts = ctx
        .expressions()
        .expression()
        .asScala
        .map(exp => {
          astForExpressionContext(exp)
        })
        .toSeq
      val splatAst = astForSplattingArgumentContext(ctx.splattingArgument())
      val callNode = NewCall()
        .name(ctx.COMMA().getText)
        .methodFullName(Operators.arrayInitializer)
        .signature(Operators.arrayInitializer)
        .typeFullName(MethodFullNames.UnknownFullName)
        .dispatchType(DispatchTypes.STATIC_DISPATCH)
        .code(ctx.getText)
        .lineNumber(ctx.COMMA().getSymbol.getLine)
        .columnNumber(ctx.COMMA().getSymbol.getCharPositionInLine)
      callAst(callNode, expAsts ++ Seq[Ast](splatAst))
    case ctx: AssociationsOnlyIndexingArgumentsContext =>
      astForAssociationsContext(ctx.associations())
    case ctx: RubyParser.SplattingOnlyIndexingArgumentsContext =>
      astForSplattingArgumentContext(ctx.splattingArgument())
    case _ =>
      logger.error("astForIndexingArgumentsContext() All contexts mismatched.")
      Ast()
  }

  def astForArrayConstructorPrimaryContext(ctx: ArrayConstructorPrimaryContext): Ast = {
    astForIndexingArgumentsContext(ctx.arrayConstructor().indexingArguments())
  }

  def astForBeginExpressionPrimaryContext(ctx: BeginExpressionPrimaryContext): Ast = {
    astForBodyStatementContext(ctx.beginExpression().bodyStatement())
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
    val methodNameAst = astForMethodNameContext(ctx.methodName())

    val argList = ListBuffer[Ast]()
    argList.addOne(astForPrimaryContext(ctx.primary()))

    if (ctx.argumentsWithParentheses() != null) {
      argList.addOne(astForArgumentsWithParenthesesContext(ctx.argumentsWithParentheses()))
    }

    if (ctx.block() != null) {
      argList.addOne(astForBlockContext(ctx.block()))
    }

    val callNode = methodNameAst.nodes.filter(node => node.isInstanceOf[NewCall]).head.asInstanceOf[NewCall]
    callAst(callNode, argList.toSeq)
  }

  def astForChainedInvocationWithoutArgumentsPrimaryContext(
    ctx: ChainedInvocationWithoutArgumentsPrimaryContext
  ): Ast = {
    val primaryAst    = astForPrimaryContext(ctx.primary())
    val methodNameAst = astForMethodNameContext(ctx.methodName())
    val blockAst = if (ctx.block() != null) {
      astForBlockContext(ctx.block())
    } else {
      Ast()
    }

    primaryAst.withChild(methodNameAst).withChild(blockAst)
  }

  def astForChainedScopedConstantReferencePrimaryContext(ctx: ChainedScopedConstantReferencePrimaryContext): Ast = {
    val primaryAst = astForPrimaryContext(ctx.primary())
    val localVar   = ctx.CONSTANT_IDENTIFIER()
    val varSymbol  = localVar.getSymbol()
    val node = createIdentiferWithScope(localVar, varSymbol.getText, varSymbol.getText, Defines.Any, List(Defines.Any))
    val constAst = Ast(node)

    val callNode = NewCall()
      .name(ctx.COLON2().getText)
      .code(ctx.COLON2().getText)
      .methodFullName(MethodFullNames.OperatorPrefix + ctx.COLON2().getText)
      .signature("")
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .typeFullName(Defines.Any)
      .lineNumber(ctx.COLON2().getSymbol().getLine())
      .columnNumber(ctx.COLON2().getSymbol().getCharPositionInLine())
    callAst(callNode, Seq[Ast](primaryAst, constAst))
  }

  def astForScopedConstantReferenceContext(ctx: ScopedConstantReferenceContext): Ast = {
    val localVar  = ctx.CONSTANT_IDENTIFIER()
    val varSymbol = localVar.getSymbol()
    val node = createIdentiferWithScope(localVar, varSymbol.getText, varSymbol.getText, Defines.Any, List(Defines.Any))

    if (ctx.primary() != null) {
      astForPrimaryContext(ctx.primary()).withChild(Ast(node))
    } else {
      Ast(node)
    }
  }

  def astForClassOrModuleReferenceContext(ctx: ClassOrModuleReferenceContext): Ast = {
    if (ctx.scopedConstantReference() != null) {
      astForScopedConstantReferenceContext(ctx.scopedConstantReference())
    } else if (ctx.CONSTANT_IDENTIFIER() != null) {
      classStack.push(ctx.CONSTANT_IDENTIFIER().getText)
      Ast()
    } else {
      Ast()
    }
  }

  def astForClassDefinitionPrimaryContext(ctx: ClassDefinitionPrimaryContext): Ast = {
    val astClassOrModuleRef = astForClassOrModuleReferenceContext(ctx.classDefinition().classOrModuleReference())
    val astExprOfCommand    = astForExpressionOrCommandContext(ctx.classDefinition().expressionOrCommand())
    val astBodyStatement    = astForBodyStatementContext(ctx.classDefinition().bodyStatement())
    if (classStack.size > 0) {
      classStack.pop()
    }
    Ast().withChildren(Seq[Ast](astClassOrModuleRef, astExprOfCommand, astBodyStatement))
  }

  def astForConditionalOperatorExpressionContext(ctx: ConditionalOperatorExpressionContext): Ast = {
    val ifConditionAst = astForExpressionContext(ctx.expression().get(0))
    val thenAst        = astForExpressionContext(ctx.expression().get(1))
    val elseAst        = astForExpressionContext(ctx.expression().get(2))
    Ast().withChildren(List[Ast](ifConditionAst, thenAst, elseAst))
  }

  def astForEqualityExpressionContext(ctx: EqualityExpressionContext): Ast = {
    astForBinaryExpression(ctx.expression(0), ctx.expression(1), ctx.op)
  }

  def astForGroupedLeftHandSideContext(ctx: GroupedLeftHandSideContext): Ast = {
    astForMultipleLeftHandSideContext(ctx.multipleLeftHandSide())
  }

  def astForPackingLeftHandSideContext(ctx: PackingLeftHandSideContext): Ast = {
    astForSingleLeftHandSideContext(ctx.singleLeftHandSide())
  }

  def astForMultipleLeftHandSideContext(ctx: MultipleLeftHandSideContext): Ast = ctx match {
    case ctx: MultipleLeftHandSideAndpackingLeftHandSideMultipleLeftHandSideContext =>
      val multipleLHSAsts = ctx
        .multipleLeftHandSideItem()
        .asScala
        .map(item => {
          if (item.singleLeftHandSide() != null) {
            astForSingleLeftHandSideContext(item.singleLeftHandSide())
          } else {
            astForGroupedLeftHandSideContext(item.groupedLeftHandSide())
          }
        })
        .toList

      val paramAsts =
        if (ctx.packingLeftHandSide() != null) {
          val packingLHSAst = astForPackingLeftHandSideContext(ctx.packingLeftHandSide())
          Seq[Ast](packingLHSAst) ++ multipleLHSAsts
        } else {
          multipleLHSAsts
        }

      val callNode = NewCall()
        .name(Operators.arrayInitializer)
        .code(ctx.getText)
        .methodFullName(Operators.arrayInitializer)
        .signature("")
        .dispatchType(DispatchTypes.STATIC_DISPATCH)
        .typeFullName(Defines.Any)
        .lineNumber(-1)
        .columnNumber(-1)
      callAst(callNode, paramAsts)

    case ctx: PackingLeftHandSideOnlyMultipleLeftHandSideContext =>
      astForPackingLeftHandSideContext(ctx.packingLeftHandSide())
    case ctx: GroupedLeftHandSideOnlyMultipleLeftHandSideContext =>
      astForGroupedLeftHandSideContext(ctx.groupedLeftHandSide())
    case _ =>
      logger.error("astForMultipleLeftHandSideContext() All contexts mismatched.")
      Ast()
  }

  def astForForVariableContext(ctx: ForVariableContext): Ast = {
    if (ctx.singleLeftHandSide() != null) {
      astForSingleLeftHandSideContext(ctx.singleLeftHandSide())
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
    Ast().withChildren(List[Ast](forVarAst, exprCmdAst)).withChild(doClauseAst)
  }

  def astForGroupingExpressionPrimaryContext(ctx: GroupingExpressionPrimaryContext): Ast = {
    astForStatementsContext(ctx.compoundStatement().statements())
  }

  def astForHashConstructorPrimaryContext(ctx: HashConstructorPrimaryContext): Ast = {
    if (ctx.hashConstructor().associations() == null) return Ast()
    astForAssociationsContext(ctx.hashConstructor().associations())
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
        Ast().withChildren(List[Ast](thenAst, expCmdAst))
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
    val lhsExpressionAst = astForPrimaryContext(ctx.primary())
    val rhsExpressionAst = astForIndexingArgumentsContext(ctx.indexingArguments())
    val callNode = NewCall()
      .name(ctx.LBRACK().getText + ctx.RBRACK().getText)
      .code(ctx.getText)
      .methodFullName(MethodFullNames.OperatorPrefix + ctx.LBRACK().getText + ctx.RBRACK().getText)
      .signature("")
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .typeFullName(Defines.Any)
      .lineNumber(ctx.LBRACK().getSymbol.getLine())
      .columnNumber(ctx.LBRACK().getSymbol.getCharPositionInLine())
    callAst(callNode, Seq[Ast](lhsExpressionAst, rhsExpressionAst))

  }

  def astForInvocationExpressionOrCommandContext(ctx: InvocationExpressionOrCommandContext): Ast = {
    if (ctx.EMARK() != null) {
      val invocWOParenAst = astForInvocationWithoutParenthesesContext(ctx.invocationWithoutParentheses())
      val callNode = NewCall()
        .name(ctx.EMARK().getText)
        .code(ctx.EMARK().getText)
        .methodFullName(MethodFullNames.OperatorPrefix + ctx.EMARK().getText)
        .signature("")
        .dispatchType(DispatchTypes.STATIC_DISPATCH)
        .typeFullName(Defines.Any)
        .lineNumber(ctx.EMARK().getSymbol().getLine())
        .columnNumber(ctx.EMARK().getSymbol().getCharPositionInLine())
      callAst(callNode, Seq[Ast](invocWOParenAst))
    } else {
      astForInvocationWithoutParenthesesContext(ctx.invocationWithoutParentheses())
    }
  }

  def astForInvocationWithoutParenthesesContext(ctx: InvocationWithoutParenthesesContext): Ast = ctx match {
    case ctx: SingleCommandOnlyInvocationWithoutParenthesesContext => astForCommandContext(ctx.command())
    case ctx: ChainedCommandDoBlockInvocationWithoutParenthesesContext =>
      astForChainedCommandWithDoBlockContext(ctx.chainedCommandWithDoBlock())
    case ctx: ChainedCommandDoBlockDorCol2mNameArgsInvocationWithoutParenthesesContext =>
      val cmdDoBlockAst  = astForChainedCommandWithDoBlockContext(ctx.chainedCommandWithDoBlock())
      val methodNameAst  = astForMethodNameContext(ctx.methodName())
      val argsWOParenAst = astForArgumentsWithoutParenthesesContext(ctx.argumentsWithoutParentheses())
      cmdDoBlockAst.withChild(methodNameAst).withChild(argsWOParenAst)
    case ctx: ReturnArgsInvocationWithoutParenthesesContext =>
      val retNode = NewReturn()
        .code(ctx.getText)
        .lineNumber(ctx.RETURN().getSymbol().getLine)
        .columnNumber(ctx.RETURN().getSymbol().getCharPositionInLine)
      val argAst = astForArgumentsContext(ctx.arguments())
      returnAst(retNode, Seq[Ast](argAst))
    case ctx: BreakArgsInvocationWithoutParenthesesContext => astForArgumentsContext(ctx.arguments())
    case ctx: NextArgsInvocationWithoutParenthesesContext  => astForArgumentsContext(ctx.arguments())
    case _ =>
      logger.error("astForInvocationWithoutParenthesesContext() All contexts mismatched.")
      Ast()
  }

  def astForInvocationWithBlockOnlyPrimaryContext(ctx: InvocationWithBlockOnlyPrimaryContext): Ast = {
    val methodIdAst = astForMethodIdentifierContext(ctx.methodIdentifier())
    val blockAst    = astForBlockContext(ctx.block())
    blockAst.withChild(methodIdAst)
  }

  def astForInvocationWithParenthesesPrimaryContext(ctx: InvocationWithParenthesesPrimaryContext): Ast = {
    val methodIdAst = astForMethodIdentifierContext(ctx.methodIdentifier())
    val parenAst    = astForArgumentsWithParenthesesContext(ctx.argumentsWithParentheses())

    if (ctx.block() != null) {
      val blockAst = astForBlockContext(ctx.block())
      methodIdAst.withChild(parenAst).withChild(blockAst)
    } else {
      methodIdAst.withChild(parenAst)
    }
  }

  def astForIsDefinedExpressionContext(ctx: IsDefinedExpressionContext): Ast = {
    val exprAst = astForExpressionContext(ctx.expression())
    val callNode = NewCall()
      .name(ctx.IS_DEFINED().getText)
      .code(ctx.IS_DEFINED().getText)
      .methodFullName(MethodFullNames.OperatorPrefix + ctx.IS_DEFINED().getText)
      .signature("")
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .typeFullName(Defines.Any)
      .lineNumber(ctx.IS_DEFINED().getSymbol().getLine())
      .columnNumber(ctx.IS_DEFINED().getSymbol().getCharPositionInLine())
    callAst(callNode, Seq[Ast](exprAst))
  }

  def astForIsDefinedPrimaryContext(ctx: IsDefinedPrimaryContext): Ast = {
    astForExpressionOrCommandContext(ctx.expressionOrCommand())
  }

  def astForJumpExpressionPrimaryContext(ctx: JumpExpressionPrimaryContext): Ast = {
    if (ctx.jumpExpression().RETURN() != null) {
      val retNode = NewReturn()
        .code(ctx.getText)
        .lineNumber(ctx.jumpExpression().RETURN().getSymbol().getLine)
        .columnNumber(ctx.jumpExpression().RETURN().getSymbol().getCharPositionInLine)
      returnAst(retNode, Seq[Ast]())
    } else if (ctx.jumpExpression().BREAK() != null) {
      Ast() // TODO implement this
    } else if (ctx.jumpExpression().NEXT() != null) {
      Ast() // TODO implement this
    } else if (ctx.jumpExpression().REDO() != null) {
      Ast() // TODO implement this
    } else if (ctx.jumpExpression().RETRY() != null) {
      Ast() // TODO implement this
    } else {
      Ast()
    }
  }

  def astForLiteralPrimaryContext(ctx: LiteralPrimaryContext): Ast = {
    if (ctx.literal().numericLiteral() != null) {
      val text = ctx.getText
      val node = NewLiteral()
        .code(text)
        .typeFullName(Defines.Number)
        .dynamicTypeHintFullName(List(Defines.Number))
      registerType(Defines.Number)
      Ast(node)
    } else if (ctx.literal().SINGLE_QUOTED_STRING_LITERAL() != null) {
      val text = ctx.getText
      val node = NewLiteral()
        .code(text)
        .typeFullName(Defines.String)
        .dynamicTypeHintFullName(List(Defines.String))
      Ast(node)
    } else if (ctx.literal().DOUBLE_QUOTED_STRING_CHARACTER_SEQUENCE() != null) {
      val text = ctx.literal().DOUBLE_QUOTED_STRING_CHARACTER_SEQUENCE().getText
      val node = NewLiteral()
        .code(text)
        .typeFullName(Defines.String)
        .dynamicTypeHintFullName(List(Defines.String))
      registerType(Defines.String)
      Ast(node)
    } else if (ctx.literal().symbol() != null) {
      astForSymbolContext(ctx.literal().symbol())
    } else {
      Ast()
    }
  }

  def astForSimpleMethodNamePartContext(ctx: SimpleMethodNamePartContext): Ast = {
    astForDefinedMethodNameContext(ctx.definedMethodName())
  }

  def astForCallNode(localIdentifier: TerminalNode): Ast = {
    val column = localIdentifier.getSymbol().getCharPositionInLine()
    val line   = localIdentifier.getSymbol().getLine()
    val callNode = NewCall()
      .name(localIdentifier.getText())
      .methodFullName(MethodFullNames.UnknownFullName)
      .signature(localIdentifier.getText())
      .typeFullName(MethodFullNames.UnknownFullName)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .code(localIdentifier.getText())
      .lineNumber(line)
      .columnNumber(column)
    callAst(callNode)
  }

  def astForMethodOnlyIdentifier(ctx: MethodOnlyIdentifierContext): Ast = {
    if (ctx.LOCAL_VARIABLE_IDENTIFIER() != null) {
      astForCallNode(ctx.LOCAL_VARIABLE_IDENTIFIER())
    } else if (ctx.CONSTANT_IDENTIFIER() != null) {
      astForCallNode(ctx.CONSTANT_IDENTIFIER())
    } else {
      Ast()
    }
  }

  def astForMethodIdentifierContext(ctx: MethodIdentifierContext): Ast = {
    if (ctx.methodOnlyIdentifier() != null) {
      astForMethodOnlyIdentifier(ctx.methodOnlyIdentifier())
    } else if (ctx.LOCAL_VARIABLE_IDENTIFIER() != null) {
      val localVar  = ctx.LOCAL_VARIABLE_IDENTIFIER()
      val varSymbol = localVar.getSymbol()
      if (lookupIdentiferInScope(varSymbol.getText)) {
        val node =
          createIdentiferWithScope(localVar, varSymbol.getText, varSymbol.getText, Defines.Any, List(Defines.Any))
        Ast(node)
      } else {
        astForCallNode(localVar)
      }
    } else if (ctx.CONSTANT_IDENTIFIER() != null) {
      val localVar  = ctx.CONSTANT_IDENTIFIER()
      val varSymbol = localVar.getSymbol()
      if (lookupIdentiferInScope(varSymbol.getText)) {
        val node =
          createIdentiferWithScope(localVar, varSymbol.getText, varSymbol.getText, Defines.Any, List(Defines.Any))
        Ast(node)
      } else {
        astForCallNode(localVar)
      }
    } else {
      Ast()
    }
  }

  def astForOperatorMethodNameContext(ctx: OperatorMethodNameContext): Ast = {

    val terminalNode =
      if (ctx.CARET() != null) ctx.CARET()
      else if (ctx.AMP() != null) ctx.AMP()
      else if (ctx.BAR() != null) ctx.BAR()
      else if (ctx.LTEQGT() != null) ctx.LTEQGT()
      else if (ctx.EQ2() != null) ctx.EQ2()
      else if (ctx.EQ3() != null) ctx.EQ3()
      else if (ctx.EQTILDE() != null) ctx.EQTILDE()
      else if (ctx.GT() != null) ctx.GT()
      else if (ctx.GTEQ() != null) ctx.GTEQ()
      else if (ctx.LT() != null) ctx.LT()
      else if (ctx.LTEQ() != null) ctx.LTEQ()
      else if (ctx.LT2() != null) ctx.LT2()
      else if (ctx.GT2() != null) ctx.GT2()
      else if (ctx.PLUS() != null) ctx.PLUS()
      else if (ctx.MINUS() != null) ctx.MINUS()
      else if (ctx.STAR() != null) ctx.STAR()
      else if (ctx.SLASH() != null) ctx.SLASH()
      else if (ctx.PERCENT() != null) ctx.PERCENT()
      else if (ctx.STAR2() != null) ctx.STAR2()
      else if (ctx.TILDE() != null) ctx.TILDE()
      else if (ctx.PLUSAT() != null) ctx.PLUSAT()
      else if (ctx.MINUSAT() != null) ctx.MINUSAT()
      else if (ctx.LBRACK() != null && ctx.RBRACK() != null) ctx.LBRACK()
      else if (ctx.LBRACK() != null && ctx.RBRACK() != null && ctx.EQ() != null) ctx.LBRACK()
      else return Ast()

    val callNode = NewCall()
      .name(ctx.getText)
      .code(ctx.getText)
      .methodFullName(MethodFullNames.OperatorPrefix + ctx.getText)
      .signature("")
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .typeFullName(Defines.Any)
      .lineNumber(terminalNode.getSymbol().getLine())
      .columnNumber(terminalNode.getSymbol().getCharPositionInLine())
    callAst(callNode)
  }

  def astForMethodNameContext(ctx: MethodNameContext): Ast = {
    if (ctx.methodIdentifier() != null) {
      astForMethodIdentifierContext(ctx.methodIdentifier())
    } else if (ctx.operatorMethodName() != null) {
      astForOperatorMethodNameContext(ctx.operatorMethodName())
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
      val node =
        createIdentiferWithScope(localVar, varSymbol.getText, varSymbol.getText, Defines.Any, List(Defines.Any))
      Ast(node)
    } else if (ctx.CONSTANT_IDENTIFIER() != null) {
      val localVar  = ctx.CONSTANT_IDENTIFIER()
      val varSymbol = localVar.getSymbol()
      val node =
        createIdentiferWithScope(localVar, varSymbol.getText, varSymbol.getText, Defines.Any, List(Defines.Any))
      Ast(node)
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
      astForVariableIdentifierContext(ctx.variableIdentifier())
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

    val seqMethodPar = localVarList
      .map(localVar => {
        val varSymbol = localVar.getSymbol()
        createIdentiferWithScope(localVar, varSymbol.getText, varSymbol.getText, Defines.Any, Seq[String](Defines.Any))
        NewMethodParameterIn()
          .name(varSymbol.getText)
          .code(varSymbol.getText)
          .lineNumber(varSymbol.getLine)
          .columnNumber(varSymbol.getCharPositionInLine)
      })
      .toSeq

    val ast = Ast(seqMethodPar)
    Ast().withChildren(List[Ast](ast))
  }

  def astForBodyStatementContext(ctx: BodyStatementContext): Ast = {
    astForStatementsContext(ctx.compoundStatement().statements())
    // TODO rescue else and ensure to be implemented
  }

  def astForMethodDefinitionContext(ctx: MethodDefinitionContext): Ast = {
    pushScope()
    val astMethodParam = astForMethodParameterPartContext(ctx.methodParameterPart())
    val astMethodName  = astForMethodNamePartContext(ctx.methodNamePart())
    val callNode       = astMethodName.nodes.filter(node => node.isInstanceOf[NewCall]).head.asInstanceOf[NewCall]
    // there can be only one call node
    val astBody = astForBodyStatementContext(ctx.bodyStatement())
    popScope()

    val classPath = classStack.toList.mkString(".") + "."
    val methodNode = NewMethod()
      .code(callNode.code)
      .name(callNode.name)
      .fullName(classPath + callNode.name)
      .columnNumber(callNode.columnNumber)
      .lineNumber(callNode.lineNumber)
      .filename(filename)
    callNode.methodFullName(classPath + callNode.name)

    val methodRetNode = NewMethodReturn()
      .lineNumber(None)
      .columnNumber(None)
      .typeFullName(Defines.Any)

    val publicModifier = NewModifier().modifierType(ModifierTypes.PUBLIC)
    /*
     * TODO find out from where the correct modifier could be obtained since the modifier comes
     *  as variableIdentifier in parser o/p.
     * This problem needs to be solved only if it matters
     */

    astMethodParam.nodes.foreach(node => {
      diffGraph.addEdge(methodNode, node, EdgeTypes.AST)
    })
    methodAst(methodNode, Seq[Ast](astMethodParam), astBody, methodRetNode, Seq[NewModifier](publicModifier))
  }

  def astForMethodOnlyIdentifierPrimaryContext(ctx: MethodOnlyIdentifierPrimaryContext): Ast = {
    astForMethodOnlyIdentifier(ctx.methodOnlyIdentifier())
  }

  def astForModuleDefinitionPrimaryContext(ctx: ModuleDefinitionPrimaryContext): Ast = {
    val referenceAst = astForClassOrModuleReferenceContext(ctx.moduleDefinition().classOrModuleReference())
    val bodyStmtAst  = astForBodyStatementContext(ctx.moduleDefinition().bodyStatement())
    referenceAst.withChild(bodyStmtAst)
  }

  def astForMultipleAssignmentExpressionContext(ctx: MultipleAssignmentExpressionContext): Ast = {
    val lhsAst  = astForMultipleLeftHandSideContext(ctx.multipleLeftHandSide())
    val rhsAsts = astForMultipleRightHandSideContext(ctx.multipleRightHandSide())
    // TODO use rhsType
    val callNode = NewCall()
      .name(ctx.EQ().getText)
      .code(ctx.getText)
      .methodFullName(MethodFullNames.OperatorPrefix + ctx.EQ().getText)
      .signature("")
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .typeFullName(Defines.Any)
      .lineNumber(ctx.EQ().getSymbol().getLine())
      .columnNumber(ctx.EQ().getSymbol().getCharPositionInLine())
    callAst(callNode, Seq[Ast](lhsAst) ++ rhsAsts)
  }

  def astForMultiplicativeExpressionContext(ctx: MultiplicativeExpressionContext): Ast = {
    astForBinaryExpression(ctx.expression(0), ctx.expression(1), ctx.op)
  }

  def astForNotExpressionOrCommandContext(ctx: NotExpressionOrCommandContext): Ast = {
    val expAst = astForExpressionOrCommandContext(ctx.expressionOrCommand())
    val callNode = NewCall()
      .name(ctx.NOT().getText)
      .code(ctx.NOT().getText)
      .methodFullName(MethodFullNames.OperatorPrefix + ctx.NOT().getText)
      .signature("")
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .typeFullName(Defines.Any)
      .lineNumber(ctx.NOT().getSymbol().getLine())
      .columnNumber(ctx.NOT().getSymbol().getCharPositionInLine())
    callAst(callNode, Seq[Ast](expAst))
  }

  def astForOperatorAndExpressionContext(ctx: OperatorAndExpressionContext): Ast = {
    astForBinaryExpression(ctx.expression(0), ctx.expression(1), ctx.op)
  }

  def astForOperatorOrExpressionContext(ctx: OperatorOrExpressionContext): Ast = {
    astForBinaryExpression(ctx.expression(0), ctx.expression(1), ctx.op)
  }

  def astForOrAndExpressionOrCommandContext(ctx: OrAndExpressionOrCommandContext): Ast = {
    val lhsAst = astForExpressionOrCommandContext(ctx.expressionOrCommand().get(0))
    val rhsAst = astForExpressionOrCommandContext(ctx.expressionOrCommand().get(1))
    val callNode = NewCall()
      .name(ctx.op.getText)
      .code(ctx.getText)
      .methodFullName(MethodFullNames.OperatorPrefix + ctx.op.getText)
      .signature("")
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .typeFullName(Defines.Any)
      .lineNumber(ctx.op.getLine())
      .columnNumber(ctx.op.getCharPositionInLine())
    callAst(callNode, Seq[Ast](lhsAst, rhsAst))
  }

  def astForPowerExpressionContext(ctx: PowerExpressionContext): Ast = {
    val expressions           = ctx.expression()
    val baseExpressionAst     = astForExpressionContext(expressions.get(0))
    val exponentExpressionAst = astForExpressionContext(expressions.get(1))
    val callNode = NewCall()
      .name(ctx.STAR2().getText)
      .code(ctx.STAR2().getText)
      .methodFullName(MethodFullNames.OperatorPrefix + ctx.STAR2().getText)
      .signature("")
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .typeFullName(Defines.Any)
      .lineNumber(ctx.STAR2().getSymbol().getLine())
      .columnNumber(ctx.STAR2().getSymbol().getCharPositionInLine())
    callAst(callNode, Seq[Ast](baseExpressionAst, exponentExpressionAst))
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
    val callNode = NewCall()
      .name(operatorToken.getText)
      .code(operatorToken.getText)
      .methodFullName(MethodFullNames.OperatorPrefix + operatorToken.getText)
      .signature("")
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .typeFullName(Defines.Any)
      .lineNumber(operatorToken.getLine())
      .columnNumber(operatorToken.getCharPositionInLine())
    callAst(callNode, Seq[Ast](lhsExpressionAst, rhsExpressionAst))
  }

  def astForSimpleScopedConstantReferencePrimaryContext(ctx: SimpleScopedConstantReferencePrimaryContext): Ast = {
    val localVar  = ctx.CONSTANT_IDENTIFIER()
    val varSymbol = localVar.getSymbol()
    val node = createIdentiferWithScope(localVar, varSymbol.getText, varSymbol.getText, Defines.Any, List(Defines.Any))

    val callNode = NewCall()
      .name(ctx.COLON2().getText)
      .code(ctx.getText)
      .methodFullName(MethodFullNames.OperatorPrefix + ctx.COLON2().getText)
      .signature("")
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .typeFullName(Defines.Any)
      .lineNumber(ctx.COLON2().getSymbol.getLine)
      .columnNumber(ctx.COLON2().getSymbol.getCharPositionInLine())

    callAst(callNode, Seq[Ast](Ast(node)))

  }

  def astForSuperExpressionPrimaryContext(ctx: SuperExpressionPrimaryContext): Ast = {
    val argAst = astForArgumentsWithParenthesesContext(ctx.argumentsWithParentheses())
    val blockAst = if (ctx.block() != null) {
      astForBlockContext(ctx.block())
    } else {
      Ast()
    }
    argAst.withChild(blockAst)
  }

  def astForCommandWithDoBlockContext(ctx: CommandWithDoBlockContext): Ast = ctx match {
    case ctx: ArgsAndDoBlockCommandWithDoBlockContext =>
      val argsAst    = astForArgumentsWithoutParenthesesContext(ctx.argumentsWithoutParentheses())
      val doBlockAst = astForDoBlockContext(ctx.doBlock())
      argsAst.withChild(doBlockAst)
    case ctx: RubyParser.ArgsAndDoBlockAndMethodIdCommandWithDoBlockContext =>
      val argsAst     = astForArgumentsWithoutParenthesesContext(ctx.argumentsWithoutParentheses())
      val doBlockAst  = astForDoBlockContext(ctx.doBlock())
      val methodIdAst = astForMethodIdentifierContext(ctx.methodIdentifier())
      methodIdAst.withChild(argsAst).withChild(doBlockAst)
    case ctx: RubyParser.PrimaryMethodArgsDoBlockCommandWithDoBlockContext =>
      val argsAst       = astForArgumentsWithoutParenthesesContext(ctx.argumentsWithoutParentheses())
      val doBlockAst    = astForDoBlockContext(ctx.doBlock())
      val methodNameAst = astForMethodNameContext(ctx.methodName())
      val primaryAst    = astForPrimaryContext(ctx.primary())
      primaryAst.withChild(methodNameAst).withChild(argsAst).withChild(doBlockAst)
    case _ =>
      logger.error("astForCommandWithDoBlockContext() All contexts mismatched.")
      Ast()
  }

  def astForChainedCommandWithDoBlockContext(ctx: ChainedCommandWithDoBlockContext): Ast = {
    val cmdAst    = astForCommandWithDoBlockContext(ctx.commandWithDoBlock())
    val mNameAsts = ctx.methodName().asScala.map(mName => astForMethodNameContext(mName)).toSeq
    val apAsts = ctx
      .argumentsWithParentheses()
      .asScala
      .map(ap => {
        astForArgumentsWithParenthesesContext(ap)
      })
      .toSeq
    cmdAst.withChildren(mNameAsts).withChildren(apAsts)
  }

  def astForArgumentsWithParenthesesContext(ctx: ArgumentsWithParenthesesContext): Ast = ctx match {
    case ctx: BlankArgsArgumentsWithParenthesesContext => Ast()
    case ctx: ArgsOnlyArgumentsWithParenthesesContext  => astForArgumentsContext(ctx.arguments())
    case ctx: ExpressionsAndChainedCommandWithDoBlockArgumentsWithParenthesesContext =>
      val expAsts = ctx
        .expressions()
        .expression
        .asScala
        .map(exp => {
          astForExpressionContext(exp)
        })
        .toSeq
      val ccDoBlock = astForChainedCommandWithDoBlockContext(ctx.chainedCommandWithDoBlock())
      Ast().withChildren(expAsts).merge(ccDoBlock)
    case ctx: ChainedCommandWithDoBlockOnlyArgumentsWithParenthesesContext =>
      astForChainedCommandWithDoBlockContext(ctx.chainedCommandWithDoBlock())
    case _ =>
      logger.error("astForArgumentsWithParenthesesContext() All contexts mismatched.")
      Ast()
  }

  def astForBlockParametersContext(ctx: BlockParametersContext): Ast = {
    if (ctx.singleLeftHandSide() != null) {
      astForSingleLeftHandSideContext(ctx.singleLeftHandSide())
    } else if (ctx.multipleLeftHandSide() != null) {
      astForMultipleLeftHandSideContext(ctx.multipleLeftHandSide())
    } else {
      Ast()
    }
  }

  def astForBlockParameterContext(ctx: BlockParameterContext): Ast = {
    if (ctx.blockParameters() != null) {
      astForBlockParametersContext(ctx.blockParameters())
    } else {
      Ast()
    }
  }

  def astForDoBlockContext(ctx: DoBlockContext): Ast = {
    astForBlock(ctx.compoundStatement().statements(), ctx.blockParameter())
  }

  def astForBraceBlockContext(ctx: BraceBlockContext): Ast = {
    astForBlock(ctx.compoundStatement().statements(), ctx.blockParameter())
  }

  def astForBlock(ctxStmt: StatementsContext, ctxParam: BlockParameterContext): Ast = {
    val stmtAst = astForStatementsContext(ctxStmt)
    if (ctxParam != null) {
      val bpAst     = astForBlockParameterContext(ctxParam)
      val blockNode = NewBlock().typeFullName(Defines.Any)
      blockAst(blockNode, List[Ast](bpAst, stmtAst))
    } else {
      stmtAst
    }
  }

  def astForBlockContext(ctx: BlockContext): Ast = {
    if (ctx.doBlock() != null) {
      astForDoBlockContext(ctx.doBlock())
    } else if (ctx.braceBlock() != null) {
      astForBraceBlockContext(ctx.braceBlock())
    } else {
      Ast()
    }
  }
  def astForUnaryExpressionContext(ctx: UnaryExpressionContext): Ast = {
    val expressionAst = astForExpressionContext(ctx.expression())
    if (ctx.op.getText == "+" && methodIdentiferQ.size > 0) {
      /*
       * This is incorrectly identified as a unary expression since the parser identifies the LHS as methodIdentifier
       * PLUS is to be interpreted as a binary operator
       */
      val callNode = NewCall()
        .name(ctx.op.getText)
        .code(ctx.op.getText)
        .methodFullName(MethodFullNames.OperatorPrefix + ctx.op.getText)
        .signature("")
        .dispatchType(DispatchTypes.STATIC_DISPATCH)
        .typeFullName(Defines.Any)
        .lineNumber(ctx.op.getLine())
        .columnNumber(ctx.op.getCharPositionInLine())
      val lhsAst = methodIdentiferQ.dequeue()
      callAst(callNode, Seq[Ast](lhsAst, expressionAst))
    } else {
      val callNode = NewCall()
        .name(ctx.op.getText)
        .code(ctx.op.getText)
        .methodFullName(MethodFullNames.OperatorPrefix + ctx.op.getText)
        .signature("")
        .dispatchType(DispatchTypes.STATIC_DISPATCH)
        .typeFullName(Defines.Any)
        .lineNumber(ctx.op.getLine())
        .columnNumber(ctx.op.getCharPositionInLine())
      callAst(callNode, Seq[Ast](expressionAst))
    }
  }

  def astForUnaryMinusExpressionContext(ctx: UnaryMinusExpressionContext): Ast = {
    val expressionAst = astForExpressionContext(ctx.expression())
    if (methodIdentiferQ.size > 0) {
      /*
       * This is incorrectly identified as a unary expression since the parser identifies the LHS as methodIdentifier
       * PLUS is to be interpreted as a binary operator
       */
      val callNode = NewCall()
        .name(ctx.MINUS().getText)
        .code(ctx.MINUS().getText)
        .methodFullName(MethodFullNames.OperatorPrefix + ctx.MINUS().getText)
        .signature("")
        .dispatchType(DispatchTypes.STATIC_DISPATCH)
        .typeFullName(Defines.Any)
        .lineNumber(ctx.MINUS().getSymbol.getLine())
        .columnNumber(ctx.MINUS().getSymbol.getCharPositionInLine())
      val lhsAst = methodIdentiferQ.dequeue()
      callAst(callNode, Seq[Ast](lhsAst, expressionAst))
    } else {
      val callNode = NewCall()
        .name(ctx.MINUS().getText)
        .code(ctx.MINUS().getText)
        .methodFullName(MethodFullNames.OperatorPrefix + ctx.MINUS().getText)
        .signature("")
        .dispatchType(DispatchTypes.STATIC_DISPATCH)
        .typeFullName(Defines.Any)
        .lineNumber(ctx.MINUS().getSymbol().getLine())
        .columnNumber(ctx.MINUS().getSymbol().getCharPositionInLine())
      callAst(callNode, Seq[Ast](expressionAst))
    }
  }

  def astForUnlessExpressionPrimaryContext(ctx: UnlessExpressionPrimaryContext): Ast = {
    val unlessAst = astForExpressionOrCommandContext(ctx.unlessExpression().expressionOrCommand())
    val thenAst   = astForThenClauseContext(ctx.unlessExpression().thenClause())
    val elseAst   = astForElseClauseContext(ctx.unlessExpression().elseClause())
    unlessAst.withChildren(Seq[Ast](thenAst, elseAst))
  }

  def astForUntilExpressionPrimaryContext(ctx: UntilExpressionPrimaryContext): Ast = {
    astForExpressionOrCommandContext(ctx.untilExpression().expressionOrCommand())
      .withChild(astForDoClauseContext(ctx.untilExpression().doClause()))
  }

  def astForPseudoVariableIdentifierContext(ctx: PseudoVariableIdentifierContext): Ast = {
    val node = {
      if (ctx.TRUE() != null) { ctx.TRUE() }
      else if (ctx.NIL() != null) { ctx.NIL() }
      else if (ctx.FALSE() != null) { ctx.FALSE() }
      else if (ctx.SELF() != null) { ctx.SELF() }
      else if (ctx.FILE__() != null) { ctx.FILE__() }
      else if (ctx.LINE__() != null) { ctx.LINE__() }
      else if (ctx.ENCODING__() != null) { ctx.ENCODING__() }
      else return Ast()
    }

    val astNode = createIdentiferWithScope(node, ctx.getText, ctx.getText, Defines.Any, List(Defines.Any))
    Ast(astNode)
  }

  def astForVariableRefenceContext(ctx: RubyParser.VariableReferenceContext): Ast = {
    if (ctx.variableIdentifier() != null) {
      astForVariableIdentifierContext(ctx.variableIdentifier())
    } else {
      astForPseudoVariableIdentifierContext(ctx.pseudoVariableIdentifier())
    }
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
    if (ctx == null) return Ast()
    astForExpressionContext(ctx.expression())
  }

  def astForBlockArgumentTypeArgumentsContext(ctx: BlockArgumentTypeArgumentsContext): Ast = {
    astForBlockArgumentContext(ctx.blockArgument())
  }

  def astForBlockSplattingTypeArgumentsContext(ctx: BlockSplattingTypeArgumentsContext): Ast = {
    val blockArgAst = if (ctx.blockArgument() != null) {
      astForBlockArgumentContext(ctx.blockArgument())
    } else {
      Ast()
    }

    val splatAst = astForSplattingArgumentContext(ctx.splattingArgument())
    Ast().withChildren(Seq[Ast](blockArgAst, splatAst))
  }

  def astForAssociationContext(ctx: AssociationContext): Ast = {
    val expr1Ast = astForExpressionContext(ctx.expression().get(0))
    val expr2Ast = astForExpressionContext(ctx.expression().get(1))
    Ast().withChildren(Seq[Ast](expr1Ast, expr2Ast))
  }

  def astForAssociationsContext(ctx: AssociationsContext) = {
    val asts = ctx
      .association()
      .asScala
      .map(assoc => {
        astForAssociationContext(assoc)
      })
      .toSeq
    Ast().withChildren(asts)
  }

  def astForBlockSplattingExprAssocTypeArgumentsContext(ctx: BlockSplattingExprAssocTypeArgumentsContext): Ast = {
    val blockArgAst     = astForBlockArgumentContext(ctx.blockArgument())
    val splatAst        = astForSplattingArgumentContext(ctx.splattingArgument())
    val associationsAst = astForAssociationsContext(ctx.associations())
    val expAsts         = ctx.expressions().expression().asScala.map(exp => astForExpressionContext(exp)).toSeq
    val expAst          = Ast().withChildren(expAsts)

    Ast().withChildren(List[Ast](blockArgAst, splatAst, associationsAst, expAst))
  }

  def astForBlockExprAssocTypeArgumentsContext(ctx: BlockExprAssocTypeArgumentsContext): Ast = {
    val listAsts = ListBuffer[Ast]()

    if (ctx.blockArgument() != null) {
      listAsts.addOne(astForBlockArgumentContext(ctx.blockArgument()))
    }

    if (ctx.associations() != null) {
      listAsts.addOne(astForAssociationsContext(ctx.associations()))
    } else {
      val exprAsts = ctx.expressions().expression().asScala.map(exp => astForExpressionContext(exp)).toList
      listAsts.addAll(exprAsts)
    }

    val blockNode = NewBlock().typeFullName(Defines.Any)
    blockAst(blockNode, listAsts.toList)
  }

  def astForArgumentsWithoutParenthesesContext(ctx: ArgumentsWithoutParenthesesContext): Ast = {
    astForArgumentsContext(ctx.arguments())
  }

  def astForCommandContext(ctx: CommandContext): Ast = {
    if (ctx.SUPER() != null) {
      val argumentsWithoutParenAst = astForArgumentsWithoutParenthesesContext(ctx.argumentsWithoutParentheses())
      Ast().withChild(argumentsWithoutParenAst)
    } else if (ctx.YIELD() != null) {
      val argumentsWithoutParenAst = astForArgumentsWithoutParenthesesContext(ctx.argumentsWithoutParentheses())
      Ast().withChild(argumentsWithoutParenAst)
    } else if (ctx.methodIdentifier() != null) {
      val methodIdentifierAst = astForMethodIdentifierContext(ctx.methodIdentifier())
      methodIdentiferQ.enqueue(methodIdentifierAst)
      val argsAst = astForArgumentsWithoutParenthesesContext(ctx.argumentsWithoutParentheses())

      val callNodes = methodIdentifierAst.nodes.filter(node => node.isInstanceOf[NewCall])
      if (callNodes.size == 1) {
        val callNode = callNodes.head.asInstanceOf[NewCall]
        if (
          callNode.name == "require" ||
          callNode.name == "require_once" ||
          callNode.name == "load"
        ) {
          val importedFile =
            argsAst.nodes.filter(node => node.isInstanceOf[NewLiteral]).head.asInstanceOf[NewLiteral].code
          println(s"Creating AST for imported file ${importedFile}")
          Ast()
        } else {
          callAst(callNode, Seq[Ast](argsAst))
        }
      } else {
        argsAst
      }
    } else if (ctx.primary() != null) {
      val argumentsWithoutParenAst = astForArgumentsWithoutParenthesesContext(ctx.argumentsWithoutParentheses())
      val primaryAst               = astForPrimaryContext(ctx.primary())
      val methodNameAst            = astForMethodNameContext(ctx.methodName())
      Ast().withChildren(List[Ast](primaryAst, methodNameAst, argumentsWithoutParenAst))
    } else {
      Ast()
    }
  }

  def astForCommandTypeArgumentsContext(ctx: CommandTypeArgumentsContext): Ast = {
    astForCommandContext(ctx.command())
  }

  def astForArgumentsContext(ctx: ArgumentsContext): Ast = ctx match {
    case ctx: BlockArgumentTypeArgumentsContext           => astForBlockArgumentTypeArgumentsContext(ctx)
    case ctx: BlockSplattingTypeArgumentsContext          => astForBlockSplattingTypeArgumentsContext(ctx)
    case ctx: BlockSplattingExprAssocTypeArgumentsContext => astForBlockSplattingExprAssocTypeArgumentsContext(ctx)
    case ctx: BlockExprAssocTypeArgumentsContext          => astForBlockExprAssocTypeArgumentsContext(ctx)
    case ctx: CommandTypeArgumentsContext                 => astForCommandTypeArgumentsContext(ctx)
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
