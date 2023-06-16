package io.joern.rubysrc2cpg.astcreation
import io.joern.rubysrc2cpg.parser.RubyParser._
import io.joern.rubysrc2cpg.parser.{RubyLexer, RubyParser}
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.x2cpg.Ast.storeInDiffGraph
import io.joern.x2cpg.Defines.DynamicCallUnknownFullName
import io.joern.x2cpg.datastructures.{Global, Scope}
import io.joern.x2cpg.{Ast, AstCreatorBase, AstNodeBuilder}
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated._
import org.antlr.v4.runtime.tree.TerminalNode
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream, ParserRuleContext, Token}
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate

import java.util
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters._

class AstCreator(filename: String, global: Global)
    extends AstCreatorBase(filename)
    with AstNodeBuilder[ParserRuleContext, AstCreator]
    with AstForPrimitivesCreator
    with AstForStatementsCreator
    with AstForExpressionsCreator {

  protected val scope: Scope[String, NewIdentifier, Unit] = new Scope()

  private val logger = LoggerFactory.getLogger(this.getClass)

  private val classStack = mutable.Stack[String]()

  /*
   * Stack of variable identifiers incorrectly identified as method identifiers
   * Each AST contains exactly one call or identifier node
   */
  protected val methodNameAsIdentifierStack = mutable.Stack[Ast]()

  protected val methodAliases = mutable.HashMap[String, String]()
  protected val methodNames   = mutable.HashSet[String]()

  protected def createIdentifierWithScope(
    ctx: ParserRuleContext,
    name: String,
    code: String,
    typeFullName: String,
    dynamicTypeHints: Seq[String] = Seq()
  ): NewIdentifier = {
    val newNode = identifierNode(ctx, name, code, typeFullName, dynamicTypeHints)
    scope.addToScope(name, newNode)
    newNode
  }

  private def getActualMethodName(name: String): String = {
    methodAliases.getOrElse(name, name)
  }
  override def createAst(): BatchedUpdate.DiffGraphBuilder = {
    val charStream  = CharStreams.fromFileName(filename)
    val lexer       = new RubyLexer(charStream)
    val tokenStream = new CommonTokenStream(lexer)
    val parser      = new RubyParser(tokenStream)
    val programCtx  = parser.program()

    val statementCtx = programCtx.compoundStatement().statements()
    scope.pushNewScope(())
    val statementAsts = if (statementCtx != null) {
      astForStatementsContext(statementCtx)
    } else {
      List[Ast](Ast())
    }
    scope.popScope()

    val name = ":program"
    val programMethod =
      NewMethod()
        .order(1)
        .name(name)
        .code(name)
        .fullName(filename)
        .filename(filename)
        .astParentType(NodeTypes.TYPE_DECL)
        .astParentFullName(filename)

    val thisParam = NewMethodParameterIn()
      .name("this")
      .code("this")
    val thisParamAst = Ast(thisParam)

    val methodRetNode = NewMethodReturn()
      .lineNumber(None)
      .columnNumber(None)
      .typeFullName(Defines.Any)

    val blockNode = NewBlock().typeFullName(Defines.Any)
    val programAst =
      methodAst(programMethod, Seq(thisParamAst), blockAst(blockNode, statementAsts.toList), methodRetNode)

    val fileNode       = NewFile().name(filename).order(1)
    val namespaceBlock = globalNamespaceBlock()
    val ast            = Ast(fileNode).withChild(Ast(namespaceBlock).withChild(programAst))

    storeInDiffGraph(ast, diffGraph)
    diffGraph
  }

  object RubyOperators {
    val none                    = "<operator>.none"
    val patternMatch            = "<operator>.patternMatch"
    val notPatternMatch         = "<operator>.notPatternMatch"
    val scopeResolution         = "<operator>.scopeResolution"
    val defined                 = "<operator>.defined"
    val keyValueAssociation     = "<operator>.keyValueAssociation"
    val activeRecordAssociation = "<operator>.activeRecordAssociation"
    val undef                   = "<operator>.undef"
    val yieldOp                 = "<operator>.yield"
  }
  private def getOperatorName(token: Token): String = token.getType match {
    case AMP                 => Operators.logicalAnd
    case AMP2                => Operators.and
    case ASSIGNMENT_OPERATOR => Operators.assignment
    case BAR                 => Operators.logicalOr
    case BAR2                => Operators.or
    case CARET               => Operators.logicalOr
    case DOT2                => Operators.range
    case DOT3                => Operators.range
    case EMARK               => Operators.not
    case EMARKEQ             => Operators.notEquals
    case EMARKTILDE          => RubyOperators.notPatternMatch
    case EQ                  => Operators.assignment
    case EQ2                 => Operators.equals
    case EQ3                 => Operators.is
    case EQTILDE             => RubyOperators.patternMatch
    case GT                  => Operators.greaterThan
    case GT2                 => Operators.logicalShiftRight
    case GTEQ                => Operators.greaterEqualsThan
    case LT                  => Operators.lessThan
    case LT2                 => Operators.shiftLeft
    case LTEQ                => Operators.lessEqualsThan
    case LTEQGT              => Operators.compare
    case MINUS               => Operators.subtraction
    case PERCENT             => Operators.modulo
    case PLUS                => Operators.addition
    case SLASH               => Operators.division
    case STAR                => Operators.multiplication
    case TILDE               => Operators.not
    case NOT                 => Operators.not
    case STAR2               => Operators.exponentiation
    case COLON2              => RubyOperators.scopeResolution
    case DOT                 => Operators.fieldAccess
    case EQGT                => RubyOperators.keyValueAssociation
    case COLON               => RubyOperators.activeRecordAssociation
    case _                   => RubyOperators.none
  }

  protected def line(ctx: ParserRuleContext): Option[Integer]      = Option(ctx.getStart.getLine)
  protected def column(ctx: ParserRuleContext): Option[Integer]    = Option(ctx.getStart.getCharPositionInLine)
  protected def lineEnd(ctx: ParserRuleContext): Option[Integer]   = Option(ctx.getStop.getLine)
  protected def columnEnd(ctx: ParserRuleContext): Option[Integer] = Option(ctx.getStop.getCharPositionInLine)

  private def registerType(typ: String): String = {
    if (typ != Defines.Any) {
      global.usedTypes.putIfAbsent(typ, true)
    }
    typ
  }
  def astForVariableIdentifierContext(
    ctx: VariableIdentifierContext,
    definitelyIdentifier: Boolean = false
  ): Seq[Ast] = {
    val terminalNode = ctx.children.asScala.map(_.asInstanceOf[TerminalNode]).head
    val token        = terminalNode.getSymbol
    val variableName = token.getText
    /*
     * Preferences
     * 1. If definitelyIdentifier is SET, create a identifier node
     * 2. If an identifier with the variable name exists within the scope, create a identifier node
     * 3. If a method with the variable name exists, create a method node
     * 4. Otherwise default to identifier node creation since there is no reason (point 2) to create a call node
     */

    if (definitelyIdentifier || scope.lookupVariable(variableName).isDefined) {
      val node = createIdentifierWithScope(ctx, variableName, variableName, Defines.Any, List[String]())
      Seq(Ast(node))
    } else if (methodNames.contains(variableName)) {
      astForCallNode(terminalNode, ctx.getText)
    } else {
      val node = createIdentifierWithScope(ctx, variableName, variableName, Defines.Any, List[String]())
      Seq(Ast(node))
    }
  }

  def astForSingleLeftHandSideContext(ctx: SingleLeftHandSideContext): Seq[Ast] = ctx match {
    case ctx: VariableIdentifierOnlySingleLeftHandSideContext =>
      astForVariableIdentifierContext(ctx.variableIdentifier(), true)
    case ctx: PrimaryInsideBracketsSingleLeftHandSideContext =>
      val primaryAsts = astForPrimaryContext(ctx.primary())
      val argsAsts    = astForArgumentsContext(ctx.arguments())
      val callNode = NewCall()
        .name(Operators.indexAccess)
        .code(ctx.getText)
        .methodFullName(Operators.indexAccess)
        .signature("")
        .dispatchType(DispatchTypes.STATIC_DISPATCH)
        .typeFullName(Defines.Any)
        .lineNumber(ctx.LBRACK().getSymbol.getLine)
        .columnNumber(ctx.LBRACK().getSymbol.getCharPositionInLine())
      Seq(callAst(callNode, primaryAsts ++ argsAsts))
    case ctx: XdotySingleLeftHandSideContext =>
      val xAsts = astForPrimaryContext(ctx.primary())
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
        createIdentifierWithScope(ctx, varSymbol.getText, varSymbol.getText, Defines.Any, List(Defines.Any))
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
      Seq(callAst(callNode, xAsts ++ Seq(yAst)))
    case ctx: ScopedConstantAccessSingleLeftHandSideContext =>
      val localVar  = ctx.CONSTANT_IDENTIFIER()
      val varSymbol = localVar.getSymbol()
      val node = createIdentifierWithScope(ctx, varSymbol.getText, varSymbol.getText, Defines.Any, List(Defines.Any))
      val constAst     = Ast(node)
      val operatorName = getOperatorName(ctx.COLON2().getSymbol)
      val callNode = NewCall()
        .name(operatorName)
        .code(ctx.getText)
        .methodFullName(operatorName)
        .signature("")
        .dispatchType(DispatchTypes.STATIC_DISPATCH)
        .typeFullName(Defines.Any)
        .lineNumber(ctx.COLON2().getSymbol().getLine())
        .columnNumber(ctx.COLON2().getSymbol().getCharPositionInLine())
      Seq(callAst(callNode, Seq(constAst)))
    case _ =>
      logger.error("astForSingleLeftHandSideContext() All contexts mismatched.")
      Seq(Ast())

  }

  def astForExpressionOrCommandsContext(ctx: ExpressionOrCommandsContext): Seq[Ast] = {
    ctx
      .expressionOrCommand()
      .asScala
      .flatMap(ec => astForExpressionOrCommand(ec))
      .toSeq
  }

  def astForSplattingArgumentContext(ctx: SplattingArgumentContext): Seq[Ast] = {
    if (ctx == null) return Seq(Ast())
    astForExpressionOrCommand(ctx.expressionOrCommand())
  }

  def astForMultipleRightHandSideContext(ctx: MultipleRightHandSideContext): Seq[Ast] = {
    if (ctx == null) return Seq(Ast())

    val exprAsts = astForExpressionOrCommandsContext(ctx.expressionOrCommands())

    val paramAsts = if (ctx.splattingArgument() != null) {
      val splattingAsts = astForSplattingArgumentContext(ctx.splattingArgument())
      exprAsts ++ splattingAsts
    } else {
      exprAsts
    }

    if (paramAsts.size > 1) {
      val callNode = NewCall()
        .name(Operators.arrayInitializer)
        .code(ctx.getText)
        .methodFullName(Operators.arrayInitializer)
        .signature("")
        .dispatchType(DispatchTypes.STATIC_DISPATCH)
        .typeFullName(Defines.Any)
      Seq(callAst(callNode, paramAsts))
    } else {
      paramAsts
    }
  }

  def astForSingleAssignmentExpressionContext(ctx: SingleAssignmentExpressionContext): Seq[Ast] = {
    val rightAst     = astForMultipleRightHandSideContext(ctx.multipleRightHandSide())
    val leftAst      = astForSingleLeftHandSideContext(ctx.singleLeftHandSide())
    val operatorName = getOperatorName(ctx.op)
    val callNode = NewCall()
      .name(operatorName)
      .code(ctx.op.getText)
      .methodFullName(operatorName)
      .signature("")
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .typeFullName(Defines.Any)
      .lineNumber(ctx.op.getLine())
      .columnNumber(ctx.op.getCharPositionInLine())
    Seq(callAst(callNode, leftAst ++ rightAst))
  }

  def astForStringInterpolationPrimaryContext(ctx: StringInterpolationPrimaryContext): Seq[Ast] = {
    val varAsts = ctx
      .stringInterpolation()
      .interpolatedStringSequence()
      .asScala
      .flatMap(inter => {
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
    varAsts ++ Seq(Ast(nodes))
  }

  def astForPrimaryContext(ctx: PrimaryContext): Seq[Ast] = ctx match {
    case ctx: ClassDefinitionPrimaryContext           => astForClassDefinitionPrimaryContext(ctx)
    case ctx: ModuleDefinitionPrimaryContext          => astForModuleDefinitionPrimaryContext(ctx)
    case ctx: MethodDefinitionPrimaryContext          => astForMethodDefinitionContext(ctx.methodDefinition())
    case ctx: YieldWithOptionalArgumentPrimaryContext => astForYieldWithOptionalArgumentPrimaryContext(ctx)
    case ctx: IfExpressionPrimaryContext              => astForIfExpressionPrimaryContext(ctx)
    case ctx: UnlessExpressionPrimaryContext          => astForUnlessExpressionPrimaryContext(ctx)
    case ctx: CaseExpressionPrimaryContext            => astForCaseExpressionPrimaryContext(ctx)
    case ctx: WhileExpressionPrimaryContext           => astForWhileExpressionContext(ctx.whileExpression())
    case ctx: UntilExpressionPrimaryContext           => astForUntilExpressionContext(ctx.untilExpression())
    case ctx: ForExpressionPrimaryContext             => astForForExpressionContext(ctx.forExpression())
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
    case ctx: LiteralPrimaryContext                   => Seq(astForLiteralPrimaryContext(ctx))
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
      Seq(Ast())
  }

  def astForExpressionContext(ctx: ExpressionContext): Seq[Ast] = ctx match {
    case ctx: PrimaryExpressionContext             => astForPrimaryContext(ctx.primary())
    case ctx: UnaryExpressionContext               => Seq(astForUnaryExpression(ctx))
    case ctx: PowerExpressionContext               => Seq(astForPowerExpression(ctx))
    case ctx: UnaryMinusExpressionContext          => astForUnaryMinusExpressionContext(ctx)
    case ctx: MultiplicativeExpressionContext      => Seq(astForMultiplicativeExpression(ctx))
    case ctx: AdditiveExpressionContext            => Seq(astForAdditiveExpression(ctx))
    case ctx: BitwiseShiftExpressionContext        => astForBitwiseShiftExpressionContext(ctx)
    case ctx: BitwiseAndExpressionContext          => astForBitwiseAndExpressionContext(ctx)
    case ctx: BitwiseOrExpressionContext           => astForBitwiseOrExpressionContext(ctx)
    case ctx: RelationalExpressionContext          => astForRelationalExpressionContext(ctx)
    case ctx: EqualityExpressionContext            => astForEqualityExpressionContext(ctx)
    case ctx: OperatorAndExpressionContext         => Seq(astForAndExpression(ctx))
    case ctx: OperatorOrExpressionContext          => Seq(astForOrExpression(ctx))
    case ctx: RangeExpressionContext               => astForRangeExpressionContext(ctx)
    case ctx: ConditionalOperatorExpressionContext => astForConditionalOperatorExpressionContext(ctx)
    case ctx: SingleAssignmentExpressionContext    => astForSingleAssignmentExpressionContext(ctx)
    case ctx: MultipleAssignmentExpressionContext  => astForMultipleAssignmentExpressionContext(ctx)
    case ctx: IsDefinedExpressionContext           => astForIsDefinedExpressionContext(ctx)
    case _ =>
      logger.error("astForExpressionContext() All contexts mismatched.")
      Seq(Ast())
  }

  def astForSymbolContext(ctx: SymbolContext): Seq[Ast] = {
    val text =
      if (ctx.SYMBOL_LITERAL() != null) {
        ctx.getText
      } else if (ctx.SINGLE_QUOTED_STRING_LITERAL() != null) {
        ctx.getText
      } else {
        return Seq(Ast())
      }

    val node = NewLiteral()
      .code(text)
      .typeFullName(Defines.String)
      .dynamicTypeHintFullName(List(Defines.String))
    Seq(Ast(node))
  }

  def astForDefinedMethodNameOrSymbolContext(ctx: DefinedMethodNameOrSymbolContext): Seq[Ast] = {
    if (ctx == null) return Seq(Ast())

    if (ctx.definedMethodName() != null) {
      astForDefinedMethodNameContext(ctx.definedMethodName())
    } else {
      astForSymbolContext(ctx.symbol())
    }
  }

  def astForStatementsContext(ctx: StatementsContext): Seq[Ast] = {
    if (ctx == null) return Seq(Ast())
    ctx
      .statement()
      .asScala
      .flatMap(st => {
        val asts = astForStatement(st)
        asts
      })
      .toSeq
  }

  def astForIndexingArgumentsContext(ctx: IndexingArgumentsContext): Seq[Ast] = ctx match {
    case ctx: RubyParser.CommandOnlyIndexingArgumentsContext =>
      astForCommandContext(ctx.command())
    case ctx: RubyParser.ExpressionsOnlyIndexingArgumentsContext =>
      val expAsts =
        ctx
          .expressions()
          .expression()
          .asScala
          .flatMap(exp => {
            astForExpressionContext(exp)
          })
          .toSeq
      val callNode = NewCall()
        .name(Operators.arrayInitializer)
        .methodFullName(Operators.arrayInitializer)
        .signature(Operators.arrayInitializer)
        .typeFullName(DynamicCallUnknownFullName)
        .dispatchType(DispatchTypes.STATIC_DISPATCH)
        .code(ctx.getText)
      Seq(callAst(callNode, expAsts))
    case ctx: RubyParser.ExpressionsAndSplattingIndexingArgumentsContext =>
      val expAsts = ctx
        .expressions()
        .expression()
        .asScala
        .flatMap(exp => {
          astForExpressionContext(exp)
        })
        .toSeq
      val splatAsts = astForSplattingArgumentContext(ctx.splattingArgument())
      val callNode = NewCall()
        .name(ctx.COMMA().getText)
        .methodFullName(Operators.arrayInitializer)
        .signature(Operators.arrayInitializer)
        .typeFullName(DynamicCallUnknownFullName)
        .dispatchType(DispatchTypes.STATIC_DISPATCH)
        .code(ctx.getText)
        .lineNumber(ctx.COMMA().getSymbol.getLine)
        .columnNumber(ctx.COMMA().getSymbol.getCharPositionInLine)
      Seq(callAst(callNode, expAsts ++ splatAsts))
    case ctx: AssociationsOnlyIndexingArgumentsContext =>
      astForAssociationsContext(ctx.associations())
    case ctx: RubyParser.SplattingOnlyIndexingArgumentsContext =>
      astForSplattingArgumentContext(ctx.splattingArgument())
    case _ =>
      logger.error("astForIndexingArgumentsContext() All contexts mismatched.")
      Seq(Ast())
  }

  def astForArrayConstructorPrimaryContext(ctx: ArrayConstructorPrimaryContext): Seq[Ast] = {
    astForIndexingArgumentsContext(ctx.arrayConstructor().indexingArguments())
  }

  def astForBeginExpressionPrimaryContext(ctx: BeginExpressionPrimaryContext): Seq[Ast] = {
    astForBodyStatementContext(ctx.beginExpression().bodyStatement())
  }

  def astForBitwiseAndExpressionContext(ctx: BitwiseAndExpressionContext): Seq[Ast] = {
    astForBinaryExpression(ctx.expression(0), ctx.expression(1), ctx.op, ctx.getText)
  }

  def astForBitwiseOrExpressionContext(ctx: BitwiseOrExpressionContext): Seq[Ast] = {
    astForBinaryExpression(ctx.expression(0), ctx.expression(1), ctx.op, ctx.getText)
  }

  def astForBitwiseShiftExpressionContext(ctx: BitwiseShiftExpressionContext): Seq[Ast] = {
    astForBinaryExpression(ctx.expression(0), ctx.expression(1), ctx.op, ctx.getText)
  }

  def astForWhenArgumentContext(ctx: WhenArgumentContext): Seq[Ast] = {
    val expAsts =
      ctx
        .expressions()
        .expression()
        .asScala
        .flatMap(exp => {
          astForExpressionContext(exp)
        })
        .toList

    val asts =
      if (ctx.splattingArgument() != null) {
        expAsts ++ astForSplattingArgumentContext(ctx.splattingArgument())
      } else {
        expAsts
      }

    val blockNode = NewBlock().typeFullName(Defines.Any)
    Seq(blockAst(blockNode, asts))
  }

  def astForCaseExpressionPrimaryContext(ctx: CaseExpressionPrimaryContext): Seq[Ast] = {
    val whenThenAstsList = ctx
      .caseExpression()
      .whenClause()
      .asScala
      .map(wh => {
        val whenNode = NewControlStructure()
          .controlStructureType(ControlStructureTypes.IF)
          .code(wh.getText())
          .lineNumber(wh.WHEN().getSymbol.getLine)
          .columnNumber(wh.WHEN().getSymbol.getCharPositionInLine)

        val whenACondAsts = astForWhenArgumentContext(wh.whenArgument())
        val thenAsts      = astForThenClauseContext(wh.thenClause())
        Ast(whenNode)
          .withChildren(whenACondAsts)
          .withConditionEdge(whenNode, whenACondAsts.head.nodes.head)
          .withChildren(thenAsts)
      })
      .toList

    val caseNode = NewControlStructure()
      .controlStructureType(ControlStructureTypes.SWITCH)
      .code(ctx.getText)
      .lineNumber(ctx.caseExpression().CASE().getSymbol.getLine)
      .columnNumber(ctx.caseExpression().CASE().getSymbol.getCharPositionInLine)

    val condAst = {
      if (ctx.caseExpression().expressionOrCommand() != null) {
        astForExpressionOrCommand(ctx.caseExpression().expressionOrCommand()).headOption
      } else {
        None
      }
    }

    val caseAsts =
      if (ctx.caseExpression().elseClause() != null) {
        val elseAst = astForElseClauseContext(ctx.caseExpression().elseClause())
        whenThenAstsList ++ elseAst
      } else {
        whenThenAstsList
      }

    Seq(controlStructureAst(caseNode, condAst, caseAsts))
  }

  def astForChainedInvocationPrimaryContext(ctx: ChainedInvocationPrimaryContext): Seq[Ast] = {
    val methodNameAst = astForMethodNameContext(ctx.methodName())

    val baseAst = astForPrimaryContext(ctx.primary())

    val terminalNode = if (ctx.COLON2() != null) {
      ctx.COLON2()
    } else {
      ctx.DOT()
    }

    val identifierNodes = methodNameAst.head.nodes
      .filter(node => node.isInstanceOf[NewIdentifier])
    if (identifierNodes.size > 0) {
      /*
       This is a object.member access. baseAst contains the object whose member is being accessed
       methodNameAst is the member
       TODO this does not cover the case in which the member could be correctly recognised as a identifier
       This will be covered once class and method information is made available to this pass with a
       preprocessing pass for imports before this pass
       */

      val operatorName = getOperatorName(terminalNode.getSymbol)
      val callNode = NewCall()
        .name(operatorName)
        .code(ctx.getText)
        .methodFullName(operatorName)
        .signature("")
        .dispatchType(DispatchTypes.STATIC_DISPATCH)
        .typeFullName(Defines.Any)
        .lineNumber(terminalNode.getSymbol().getLine())
        .columnNumber(terminalNode.getSymbol().getCharPositionInLine())
      Seq(callAst(callNode, baseAst ++ methodNameAst))
    } else {
      // this is a object.method(args) access
      // baseAst contains the object whose member is being accessed
      // call node is for the method. arguments are the passed arguments + the object itself
      val argsAst = if (ctx.argumentsWithParentheses() != null) {
        astForArgumentsWithParenthesesContext(ctx.argumentsWithParentheses())
      } else {
        Seq()
      }

      val blocksAst = if (ctx.block() != null) {
        astForBlockContext(ctx.block())
      } else {
        Seq()
      }

      val callNode = methodNameAst.head.nodes.filter(node => node.isInstanceOf[NewCall]).head.asInstanceOf[NewCall]
      callNode
        .code(ctx.getText)
        .lineNumber(terminalNode.getSymbol().getLine())
        .columnNumber(terminalNode.getSymbol().getCharPositionInLine())
      Seq(callAst(callNode, baseAst ++ argsAst ++ blocksAst))
    }
  }

  def astForChainedInvocationWithoutArgumentsPrimaryContext(
    ctx: ChainedInvocationWithoutArgumentsPrimaryContext
  ): Seq[Ast] = {
    val methodNameAst = astForMethodNameContext(ctx.methodName())
    val baseAst       = astForPrimaryContext(ctx.primary())

    val blocksAst = if (ctx.block() != null) {
      astForBlockContext(ctx.block())
    } else {
      Seq()
    }
    val callNode = methodNameAst.head.nodes.filter(node => node.isInstanceOf[NewCall]).head.asInstanceOf[NewCall]
    callNode
      .code(ctx.getText)
      .lineNumber(ctx.COLON2().getSymbol().getLine())
      .columnNumber(ctx.COLON2().getSymbol().getCharPositionInLine())
    Seq(callAst(callNode, baseAst ++ blocksAst))
  }

  def astForChainedScopedConstantReferencePrimaryContext(
    ctx: ChainedScopedConstantReferencePrimaryContext
  ): Seq[Ast] = {
    val primaryAst = astForPrimaryContext(ctx.primary())
    val localVar   = ctx.CONSTANT_IDENTIFIER()
    val varSymbol  = localVar.getSymbol()
    val node     = createIdentifierWithScope(ctx, varSymbol.getText, varSymbol.getText, Defines.Any, List(Defines.Any))
    val constAst = Ast(node)

    val operatorName = getOperatorName(ctx.COLON2().getSymbol)
    val callNode = NewCall()
      .name(operatorName)
      .code(ctx.getText)
      .methodFullName(operatorName)
      .signature("")
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .typeFullName(Defines.Any)
      .lineNumber(ctx.COLON2().getSymbol().getLine())
      .columnNumber(ctx.COLON2().getSymbol().getCharPositionInLine())
    Seq(callAst(callNode, primaryAst ++ Seq(constAst)))
  }

  private def getClassNameScopedConstantReferenceContext(ctx: ScopedConstantReferenceContext): String = {
    val classTerminalNode = ctx.CONSTANT_IDENTIFIER()

    if (ctx.primary() != null) {
      val primaryAst = astForPrimaryContext(ctx.primary())
      val moduleNameNode = primaryAst.head.nodes
        .filter(node => node.isInstanceOf[NewIdentifier])
        .head
        .asInstanceOf[NewIdentifier]
      val moduleName = moduleNameNode.name
      moduleName + "." + classTerminalNode.getText
    } else {
      classTerminalNode.getText
    }
  }

  def astForClassOrModuleReferenceContext(
    ctx: ClassOrModuleReferenceContext,
    baseClassName: Option[String] = None
  ): Seq[Ast] = {
    val className = if (ctx.scopedConstantReference() != null) {
      getClassNameScopedConstantReferenceContext(ctx.scopedConstantReference())
    } else if (ctx.CONSTANT_IDENTIFIER() != null) {
      baseClassName match {
        case Some(value) => value + "." + ctx.CONSTANT_IDENTIFIER().getText
        case None        => ctx.CONSTANT_IDENTIFIER().getText
      }
    } else {
      Defines.Any
    }

    if (className != Defines.Any) {
      classStack.push(className)
    }
    Seq(Ast())
  }

  def astForClassDefinitionPrimaryContext(ctx: ClassDefinitionPrimaryContext): Seq[Ast] = {
    if (ctx.classDefinition().classOrModuleReference() != null) {
      val baseClassName = if (ctx.classDefinition().expressionOrCommand() != null) {
        val parentClassNameAst = astForExpressionOrCommand(ctx.classDefinition().expressionOrCommand())
        val nameNode = parentClassNameAst.head.nodes
          .filter(node => node.isInstanceOf[NewIdentifier])
          .head
          .asInstanceOf[NewIdentifier]
        Some(nameNode.name)
      } else {
        None
      }

      val classOrModuleRefAst =
        astForClassOrModuleReferenceContext(ctx.classDefinition().classOrModuleReference(), baseClassName)
      val bodyAst = astForBodyStatementContext(ctx.classDefinition().bodyStatement())
      val bodyAstSansModifiers = bodyAst
        .filterNot(ast => {
          val nodes = ast.nodes
            .filter(_.isInstanceOf[NewIdentifier])

          if (nodes.size == 1) {
            val varName = nodes
              .map(_.asInstanceOf[NewIdentifier].name)
              .head
            varName == "public" || varName == "protected" || varName == "private"
          } else {
            false
          }
        })

      if (classStack.size > 0) {
        classStack.pop()
      }
      val blockNode = NewBlock()
        .code(ctx.getText)
      val bodyBlockAst = blockAst(blockNode, bodyAstSansModifiers.toList)
      Seq(classOrModuleRefAst.head.withChild(bodyBlockAst))
    } else {
      // TODO test for this is pending due to lack of understanding to generate an example
      val astExprOfCommand = astForExpressionOrCommand(ctx.classDefinition().expressionOrCommand())
      val astBodyStatement = astForBodyStatementContext(ctx.classDefinition().bodyStatement())
      val blockNode = NewBlock()
        .code(ctx.getText)
      val bodyBlockAst = blockAst(blockNode, astBodyStatement.toList)
      astExprOfCommand ++ Seq(bodyBlockAst)
    }
  }

  def astForConditionalOperatorExpressionContext(ctx: ConditionalOperatorExpressionContext): Seq[Ast] = {
    val conditionAst = astForExpressionContext(ctx.expression().get(0))
    val thenAst      = astForExpressionContext(ctx.expression().get(1))
    val elseAst      = astForExpressionContext(ctx.expression().get(2))

    val ifNode = NewControlStructure()
      .controlStructureType(ControlStructureTypes.IF)
      .code(ctx.getText)
      .lineNumber(ctx.QMARK().getSymbol.getLine)
      .columnNumber(ctx.QMARK().getSymbol.getCharPositionInLine)

    Seq(controlStructureAst(ifNode, conditionAst.headOption, List(thenAst ++ elseAst).flatten))
  }

  def astForEqualityExpressionContext(ctx: EqualityExpressionContext): Seq[Ast] = {
    astForBinaryExpression(ctx.expression(0), ctx.expression(1), ctx.op, ctx.getText)
  }

  def astForGroupedLeftHandSideContext(ctx: GroupedLeftHandSideContext): Seq[Ast] = {
    astForMultipleLeftHandSideContext(ctx.multipleLeftHandSide())
  }

  def astForPackingLeftHandSideContext(ctx: PackingLeftHandSideContext): Seq[Ast] = {
    astForSingleLeftHandSideContext(ctx.singleLeftHandSide())
  }

  def astForMultipleLeftHandSideContext(ctx: MultipleLeftHandSideContext): Seq[Ast] = ctx match {
    case ctx: MultipleLeftHandSideAndpackingLeftHandSideMultipleLeftHandSideContext =>
      val multipleLHSAsts = ctx
        .multipleLeftHandSideItem()
        .asScala
        .flatMap(item => {
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
          packingLHSAst ++ multipleLHSAsts
        } else {
          multipleLHSAsts
        }

      if (paramAsts.size > 1) {
        val callNode = NewCall()
          .name(Operators.arrayInitializer)
          .code(ctx.getText)
          .methodFullName(Operators.arrayInitializer)
          .signature("")
          .dispatchType(DispatchTypes.STATIC_DISPATCH)
          .typeFullName(Defines.Any)
        Seq(callAst(callNode, paramAsts))
      } else {
        paramAsts
      }

    case ctx: PackingLeftHandSideOnlyMultipleLeftHandSideContext =>
      astForPackingLeftHandSideContext(ctx.packingLeftHandSide())
    case ctx: GroupedLeftHandSideOnlyMultipleLeftHandSideContext =>
      astForGroupedLeftHandSideContext(ctx.groupedLeftHandSide())
    case _ =>
      logger.error("astForMultipleLeftHandSideContext() All contexts mismatched.")
      Seq(Ast())
  }

  def astForForVariableContext(ctx: ForVariableContext): Seq[Ast] = {
    if (ctx.singleLeftHandSide() != null) {
      astForSingleLeftHandSideContext(ctx.singleLeftHandSide())
    } else if (ctx.multipleLeftHandSide() != null) {
      astForMultipleLeftHandSideContext(ctx.multipleLeftHandSide())
    } else {
      Seq(Ast())
    }
  }

  def astForForExpressionContext(ctx: ForExpressionContext): Seq[Ast] = {
    val initAst    = astForForVariableContext(ctx.forVariable())
    val forCondAst = astForExpressionOrCommand(ctx.expressionOrCommand())
    val bodyAsts   = astForDoClauseContext(ctx.doClause())

    val ast = whileAst(
      Some(forCondAst.head),
      bodyAsts,
      Some(ctx.getText),
      Some(ctx.FOR().getSymbol.getLine),
      Some(ctx.FOR().getSymbol.getCharPositionInLine)
    ).withChild(initAst.head)
    Seq(ast)
  }

  def astForGroupingExpressionPrimaryContext(ctx: GroupingExpressionPrimaryContext): Seq[Ast] = {
    astForStatementsContext(ctx.compoundStatement().statements())
  }

  def astForHashConstructorPrimaryContext(ctx: HashConstructorPrimaryContext): Seq[Ast] = {
    if (ctx.hashConstructor().associations() == null) return Seq(Ast())
    astForAssociationsContext(ctx.hashConstructor().associations())
  }

  def astForThenClauseContext(ctx: ThenClauseContext): Seq[Ast] = {
    astForStatementsContext(ctx.compoundStatement().statements())
  }

  def astForElsifClauseContext(ctx: util.List[ElsifClauseContext]): Seq[Ast] = {
    if (ctx == null) return Seq()

    ctx.asScala
      .map(elif => {
        val elifNode = NewControlStructure()
          .controlStructureType(ControlStructureTypes.IF)
          .code(elif.getText())
          .lineNumber(elif.ELSIF().getSymbol.getLine)
          .columnNumber(elif.ELSIF().getSymbol.getCharPositionInLine)

        val conditionAst = astForExpressionOrCommand(elif.expressionOrCommand())
        val thenAsts     = astForThenClauseContext(elif.thenClause())
        controlStructureAst(elifNode, conditionAst.headOption, thenAsts)
      })
      .toSeq
  }

  def astForElseClauseContext(ctx: ElseClauseContext): Seq[Ast] = {
    if (ctx == null) return Seq(Ast())
    val elseNode = NewControlStructure()
      .controlStructureType(ControlStructureTypes.ELSE)
      .code(ctx.getText())
      .lineNumber(ctx.ELSE().getSymbol.getLine)
      .columnNumber(ctx.ELSE().getSymbol.getCharPositionInLine)
    val stmtsAsts = astForStatementsContext(ctx.compoundStatement().statements())
    Seq(
      Ast(elseNode)
        .withChildren(stmtsAsts)
    )
  }

  def astForIfExpressionContext(ctx: IfExpressionContext): Seq[Ast] = {
    val conditionAsts = astForExpressionOrCommand(ctx.expressionOrCommand())
    val thenAsts      = astForThenClauseContext(ctx.thenClause())
    val elseifAsts    = astForElsifClauseContext(ctx.elsifClause())
    val elseAst       = astForElseClauseContext(ctx.elseClause())

    val ifNode = NewControlStructure()
      .controlStructureType(ControlStructureTypes.IF)
      .code(ctx.getText)
      .lineNumber(ctx.IF().getSymbol.getLine)
      .columnNumber(ctx.IF().getSymbol.getCharPositionInLine)

    Seq(controlStructureAst(ifNode, conditionAsts.headOption, List(thenAsts ++ elseifAsts ++ elseAst).flatten))
  }

  def astForIfExpressionPrimaryContext(ctx: IfExpressionPrimaryContext): Seq[Ast] = {
    astForIfExpressionContext(ctx.ifExpression())
  }

  def astForIndexingExpressionPrimaryContext(ctx: IndexingExpressionPrimaryContext): Seq[Ast] = {
    val lhsExpressionAst = astForPrimaryContext(ctx.primary())
    val rhsExpressionAst = astForIndexingArgumentsContext(ctx.indexingArguments())
    val callNode = NewCall()
      .name(Operators.indexAccess)
      .code(ctx.getText)
      .methodFullName(Operators.indexAccess)
      .signature("")
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .typeFullName(Defines.Any)
      .lineNumber(ctx.LBRACK().getSymbol.getLine())
      .columnNumber(ctx.LBRACK().getSymbol.getCharPositionInLine())
    Seq(callAst(callNode, lhsExpressionAst ++ rhsExpressionAst))

  }

  def astForInvocationExpressionOrCommandContext(ctx: InvocationExpressionOrCommandContext): Seq[Ast] = {
    if (ctx.EMARK() != null) {
      val invocWOParenAsts = astForInvocationWithoutParenthesesContext(ctx.invocationWithoutParentheses())
      val operatorName     = getOperatorName(ctx.EMARK().getSymbol)
      val callNode = NewCall()
        .name(operatorName)
        .code(ctx.getText)
        .methodFullName(operatorName)
        .signature("")
        .dispatchType(DispatchTypes.STATIC_DISPATCH)
        .typeFullName(Defines.Any)
        .lineNumber(ctx.EMARK().getSymbol().getLine())
        .columnNumber(ctx.EMARK().getSymbol().getCharPositionInLine())
      Seq(callAst(callNode, invocWOParenAsts))
    } else {
      astForInvocationWithoutParenthesesContext(ctx.invocationWithoutParentheses())
    }
  }

  def astForInvocationWithoutParenthesesContext(ctx: InvocationWithoutParenthesesContext): Seq[Ast] = ctx match {
    case ctx: SingleCommandOnlyInvocationWithoutParenthesesContext => astForCommandContext(ctx.command())
    case ctx: ChainedCommandDoBlockInvocationWithoutParenthesesContext =>
      astForChainedCommandWithDoBlockContext(ctx.chainedCommandWithDoBlock())
    case ctx: ChainedCommandDoBlockDorCol2mNameArgsInvocationWithoutParenthesesContext =>
      val cmdDoBlockAst  = astForChainedCommandWithDoBlockContext(ctx.chainedCommandWithDoBlock())
      val methodNameAst  = astForMethodNameContext(ctx.methodName())
      val argsWOParenAst = astForArgumentsWithoutParenthesesContext(ctx.argumentsWithoutParentheses())
      cmdDoBlockAst ++ methodNameAst ++ argsWOParenAst
    case ctx: ReturnArgsInvocationWithoutParenthesesContext =>
      val retNode = NewReturn()
        .code(ctx.getText)
        .lineNumber(ctx.RETURN().getSymbol().getLine)
        .columnNumber(ctx.RETURN().getSymbol().getCharPositionInLine)
      val argAst = astForArgumentsContext(ctx.arguments())
      Seq(returnAst(retNode, argAst))
    case ctx: BreakArgsInvocationWithoutParenthesesContext =>
      val node = NewControlStructure()
        .controlStructureType(ControlStructureTypes.BREAK)
        .lineNumber(ctx.BREAK().getSymbol.getLine)
        .columnNumber(ctx.BREAK().getSymbol.getCharPositionInLine)
        .code(ctx.getText)
      Seq(
        Ast(node)
          .withChildren(astForArgumentsContext(ctx.arguments()))
      )
    case ctx: NextArgsInvocationWithoutParenthesesContext =>
      astForArgumentsContext(ctx.arguments())
      val node = NewControlStructure()
        .controlStructureType(ControlStructureTypes.CONTINUE)
        .lineNumber(ctx.NEXT().getSymbol.getLine)
        .columnNumber(ctx.NEXT().getSymbol.getCharPositionInLine)
        .code(Defines.ModifierNext)
      Seq(
        Ast(node)
          .withChildren(astForArgumentsContext(ctx.arguments()))
      )
    case _ =>
      logger.error("astForInvocationWithoutParenthesesContext() All contexts mismatched.")
      Seq(Ast())
  }

  def astForInvocationWithBlockOnlyPrimaryContext(ctx: InvocationWithBlockOnlyPrimaryContext): Seq[Ast] = {
    val methodIdAst = astForMethodIdentifierContext(ctx.methodIdentifier(), ctx.getText, true)
    val blockAst    = astForBlockContext(ctx.block())
    blockAst ++ methodIdAst
  }

  def astForInvocationWithParenthesesPrimaryContext(ctx: InvocationWithParenthesesPrimaryContext): Seq[Ast] = {
    val methodIdAst = astForMethodIdentifierContext(ctx.methodIdentifier(), ctx.getText, true)
    val parenAst    = astForArgumentsWithParenthesesContext(ctx.argumentsWithParentheses())
    val callNode    = methodIdAst.head.nodes.filter(_.isInstanceOf[NewCall]).head.asInstanceOf[NewCall]
    callNode.name(getActualMethodName(callNode.name))

    if (ctx.block() != null) {
      val blockAst = astForBlockContext(ctx.block())
      Seq(callAst(callNode, parenAst ++ blockAst))
    } else {
      Seq(callAst(callNode, parenAst))
    }
  }

  def astForIsDefinedExpressionContext(ctx: IsDefinedExpressionContext): Seq[Ast] = {
    val exprAst = astForExpressionContext(ctx.expression())
    val callNode = NewCall()
      .name(RubyOperators.defined)
      .code(ctx.getText)
      .methodFullName(RubyOperators.defined)
      .signature("")
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .typeFullName(Defines.Any)
      .lineNumber(ctx.IS_DEFINED().getSymbol().getLine())
      .columnNumber(ctx.IS_DEFINED().getSymbol().getCharPositionInLine())
    Seq(callAst(callNode, exprAst))
  }

  def astForIsDefinedPrimaryContext(ctx: IsDefinedPrimaryContext): Seq[Ast] = {
    astForExpressionOrCommand(ctx.expressionOrCommand())
  }

  def astForJumpExpressionPrimaryContext(ctx: JumpExpressionPrimaryContext): Seq[Ast] = {
    if (ctx.jumpExpression().RETURN() != null) {
      val retNode = NewReturn()
        .code(ctx.getText)
        .lineNumber(ctx.jumpExpression().RETURN().getSymbol().getLine)
        .columnNumber(ctx.jumpExpression().RETURN().getSymbol().getCharPositionInLine)
      Seq(returnAst(retNode, Seq[Ast]()))
    } else if (ctx.jumpExpression().BREAK() != null) {
      val node = NewControlStructure()
        .controlStructureType(ControlStructureTypes.BREAK)
        .lineNumber(ctx.jumpExpression().BREAK().getSymbol.getLine)
        .columnNumber(ctx.jumpExpression().BREAK().getSymbol.getCharPositionInLine)
        .code(ctx.getText)
      Seq(Ast(node))
    } else if (ctx.jumpExpression().NEXT() != null) {
      val node = NewControlStructure()
        .controlStructureType(ControlStructureTypes.CONTINUE)
        .lineNumber(ctx.jumpExpression().NEXT().getSymbol.getLine)
        .columnNumber(ctx.jumpExpression().NEXT().getSymbol.getCharPositionInLine)
        .code(Defines.ModifierNext)
      Seq(Ast(node))
    } else if (ctx.jumpExpression().REDO() != null) {
      val node = NewControlStructure()
        .controlStructureType(ControlStructureTypes.CONTINUE)
        .lineNumber(ctx.jumpExpression().REDO().getSymbol.getLine)
        .columnNumber(ctx.jumpExpression().REDO().getSymbol.getCharPositionInLine)
        .code(Defines.ModifierRedo)
      Seq(Ast(node))
    } else if (ctx.jumpExpression().RETRY() != null) {
      val node = NewControlStructure()
        .controlStructureType(ControlStructureTypes.CONTINUE)
        .lineNumber(ctx.jumpExpression().RETRY().getSymbol.getLine)
        .columnNumber(ctx.jumpExpression().RETRY().getSymbol.getCharPositionInLine)
        .code(Defines.ModifierRetry)
      Seq(Ast(node))
    } else {
      Seq(Ast())
    }
  }

  def astForLiteralPrimaryContext(ctx: LiteralPrimaryContext): Ast =
    ctx.literal() match {
      case ctx: NumericLiteralLiteralContext     => astForNumericLiteral(ctx.numericLiteral)
      case ctx: SymbolLiteralContext             => astForSymbolLiteral(ctx.symbol())
      case ctx: SingleQuotedStringLiteralContext => astForSingleQuotedStringLiteral(ctx)
      case ctx: DoubleQuotedStringLiteralContext => astForDoubleQuotedStringLiteral(ctx)
      case ctx: RegularExpressionLiteralContext  => astForRegularExpressionLiteral(ctx)
    }

  def astForSimpleMethodNamePartContext(ctx: SimpleMethodNamePartContext): Seq[Ast] = {
    astForDefinedMethodNameContext(ctx.definedMethodName())
  }

  def astForCallNode(localIdentifier: TerminalNode, code: String): Seq[Ast] = {
    val column         = localIdentifier.getSymbol().getCharPositionInLine()
    val line           = localIdentifier.getSymbol().getLine()
    val name           = getActualMethodName(localIdentifier.getText)
    val methodFullName = s"$filename:$name"

    val callNode = NewCall()
      .name(name)
      .methodFullName(methodFullName)
      .signature(localIdentifier.getText())
      .typeFullName(DynamicCallUnknownFullName)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .code(code)
      .lineNumber(line)
      .columnNumber(column)
      .code(code)
    Seq(callAst(callNode))
  }

  def astForMethodOnlyIdentifier(ctx: MethodOnlyIdentifierContext): Seq[Ast] = {
    if (ctx.LOCAL_VARIABLE_IDENTIFIER() != null) {
      astForCallNode(ctx.LOCAL_VARIABLE_IDENTIFIER(), ctx.getText)
    } else if (ctx.CONSTANT_IDENTIFIER() != null) {
      astForCallNode(ctx.CONSTANT_IDENTIFIER(), ctx.getText)
    } else {
      Seq(Ast())
    }
  }

  def astForMethodIdentifierContext(
    ctx: MethodIdentifierContext,
    code: String,
    definitelyMethod: Boolean = false
  ): Seq[Ast] = {
    if (ctx.methodOnlyIdentifier() != null) {
      astForMethodOnlyIdentifier(ctx.methodOnlyIdentifier())
    } else if (ctx.LOCAL_VARIABLE_IDENTIFIER() != null) {
      val localVar  = ctx.LOCAL_VARIABLE_IDENTIFIER()
      val varSymbol = localVar.getSymbol()

      /*
       * Preferences
       * 1. If definitelyMethod is SET, we are in the context of processing a method or call
       * node wrt the statement being processed. Create a call node
       * 2. If an identifier with the variable name exists within the scope, create a identifier node
       * 3. Otherwise default to call node creation since there is no reason (point 2) to create a identifier node
       */

      if (scope.lookupVariable(varSymbol.getText).isDefined && !definitelyMethod) {
        val node =
          createIdentifierWithScope(ctx, varSymbol.getText, varSymbol.getText, Defines.Any, List(Defines.Any))
        Seq(Ast(node))
      } else {
        astForCallNode(localVar, code)
      }
    } else if (ctx.CONSTANT_IDENTIFIER() != null) {
      val localVar  = ctx.CONSTANT_IDENTIFIER()
      val varSymbol = localVar.getSymbol()
      if (scope.lookupVariable(varSymbol.getText).isDefined) {
        val node =
          createIdentifierWithScope(ctx, varSymbol.getText, varSymbol.getText, Defines.Any, List(Defines.Any))
        Seq(Ast(node))
      } else {
        astForCallNode(localVar, code)
      }
    } else {
      Seq(Ast())
    }
  }

  def astForOperatorMethodNameContext(ctx: OperatorMethodNameContext): Seq[Ast] = {

    /*
     * This is for operator overloading for the class
     */
    val terminalNode = ctx.children.asScala.head
      .asInstanceOf[TerminalNode]

    val name           = ctx.getText
    val methodFullName = s"$filename:$name"

    val callNode = NewCall()
      .name(name)
      .code(ctx.getText)
      .methodFullName(methodFullName)
      .signature("")
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .typeFullName(Defines.Any)
      .lineNumber(terminalNode.getSymbol().getLine())
      .columnNumber(terminalNode.getSymbol().getCharPositionInLine())
    Seq(callAst(callNode))
  }

  def astForMethodNameContext(ctx: MethodNameContext): Seq[Ast] = {
    if (ctx.methodIdentifier() != null) {
      astForMethodIdentifierContext(ctx.methodIdentifier(), ctx.getText, true)
    } else if (ctx.operatorMethodName() != null) {
      astForOperatorMethodNameContext(ctx.operatorMethodName())
    } else if (ctx.keyword() != null) {
      val terminalNode = ctx
        .keyword()
        .children
        .asScala
        .head
        .asInstanceOf[TerminalNode]
      val callNode = NewCall()
        .name(terminalNode.getText)
        .code(ctx.getText)
        .methodFullName(terminalNode.getText)
        .signature("")
        .dispatchType(DispatchTypes.STATIC_DISPATCH)
        .typeFullName(Defines.Any)
        .lineNumber(terminalNode.getSymbol().getLine())
        .columnNumber(terminalNode.getSymbol().getCharPositionInLine())
      Seq(callAst(callNode))
    } else {
      Seq(Ast())
    }
  }
  def astForAssignmentLikeMethodIdentifierContext(ctx: AssignmentLikeMethodIdentifierContext): Seq[Ast] = {
    if (ctx == null) return Seq(Ast())

    if (ctx.LOCAL_VARIABLE_IDENTIFIER() != null) {
      val localVar  = ctx.LOCAL_VARIABLE_IDENTIFIER()
      val varSymbol = localVar.getSymbol()
      val node =
        createIdentifierWithScope(ctx, varSymbol.getText, varSymbol.getText, Defines.Any, List(Defines.Any))
      Seq(Ast(node))
    } else if (ctx.CONSTANT_IDENTIFIER() != null) {
      val localVar  = ctx.CONSTANT_IDENTIFIER()
      val varSymbol = localVar.getSymbol()
      val node =
        createIdentifierWithScope(ctx, varSymbol.getText, varSymbol.getText, Defines.Any, List(Defines.Any))
      Seq(Ast(node))
    } else {
      Seq(Ast())
    }
  }

  def astForDefinedMethodNameContext(ctx: DefinedMethodNameContext): Seq[Ast] = {
    val methodNameAst         = astForMethodNameContext(ctx.methodName())
    val assignLinkedMethodAst = astForAssignmentLikeMethodIdentifierContext(ctx.assignmentLikeMethodIdentifier())
    methodNameAst ++ assignLinkedMethodAst
  }

  def astForSingletonObjextContext(ctx: SingletonObjectContext): Seq[Ast] = {
    if (ctx.variableIdentifier() != null) {
      astForVariableIdentifierContext(ctx.variableIdentifier(), true)
    } else if (ctx.pseudoVariableIdentifier() != null) {
      Seq(Ast())
    } else if (ctx.expressionOrCommand() != null) {
      astForExpressionOrCommand(ctx.expressionOrCommand())
    } else {
      Seq(Ast())
    }
  }

  def astForSingletonMethodNamePartContext(ctx: SingletonMethodNamePartContext): Seq[Ast] = {
    val definedMethodNameAst = astForDefinedMethodNameContext(ctx.definedMethodName())
    val singletonObjAst      = astForSingletonObjextContext(ctx.singletonObject())
    definedMethodNameAst ++ singletonObjAst
  }

  def astForMethodNamePartContext(ctx: MethodNamePartContext): Seq[Ast] = ctx match {
    case ctx: SimpleMethodNamePartContext    => astForSimpleMethodNamePartContext(ctx)
    case ctx: SingletonMethodNamePartContext => astForSingletonMethodNamePartContext(ctx)
    case _ =>
      logger.error("astForMethodNamePartContext() All contexts mismatched.")
      Seq(Ast())
  }

  def astForMethodParameterPartContext(ctx: MethodParameterPartContext): Seq[Ast] = {
    if (ctx == null || ctx.parameters() == null) return Seq(Ast())
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

    localVarList
      .map(localVar => {
        val varSymbol = localVar.getSymbol()
        createIdentifierWithScope(ctx, varSymbol.getText, varSymbol.getText, Defines.Any, Seq[String](Defines.Any))
        val param = NewMethodParameterIn()
          .name(varSymbol.getText)
          .code(varSymbol.getText)
          .lineNumber(varSymbol.getLine)
          .columnNumber(varSymbol.getCharPositionInLine)
        Ast(param)
      })
      .toSeq
  }

  def astForRescueClauseContext(ctx: RescueClauseContext): Ast = {
    val asts = ListBuffer[Ast]()

    if (ctx.exceptionClass() != null) {
      val exceptionClass = ctx.exceptionClass()

      if (exceptionClass.expression() != null) {
        asts.addAll(astForExpressionContext(exceptionClass.expression()))
      } else {
        asts.addAll(astForMultipleRightHandSideContext(exceptionClass.multipleRightHandSide()))
      }
    }

    if (ctx.exceptionVariableAssignment() != null) {
      asts.addAll(astForSingleLeftHandSideContext(ctx.exceptionVariableAssignment().singleLeftHandSide()))
    }

    asts.addAll(astForThenClauseContext(ctx.thenClause()))
    val blockNode = NewBlock()
      .code(ctx.getText)
      .lineNumber(ctx.RESCUE().getSymbol.getLine)
      .columnNumber(ctx.RESCUE().getSymbol.getCharPositionInLine)
    blockAst(blockNode, asts.toList)
  }

  def astForBodyStatementContext(ctx: BodyStatementContext, addReturnNode: Boolean = false): Seq[Ast] = {
    val compoundStatementAsts = astForStatementsContext(ctx.compoundStatement().statements())

    val compoundStatementAstsWithReturn =
      if (addReturnNode && compoundStatementAsts.size > 0) {
        /*
         * Convert the last statement to a return AST if it is not already a return AST.
         * If it is a return AST leave it untouched.
         */
        val lastStmtIsAlreadyReturn = compoundStatementAsts.last.root match {
          case Some(value) => value.isInstanceOf[NewReturn]
          case None        => false
        }

        if (
          !lastStmtIsAlreadyReturn &&
          ctx.compoundStatement().statements() != null
        ) {
          val len  = ctx.compoundStatement().statements().statement().size()
          val code = ctx.compoundStatement().statements().statement().get(len - 1).getText
          val retNode = NewReturn()
            .code(code)
          val returnReplaced = returnAst(retNode, Seq[Ast](compoundStatementAsts.last))
          compoundStatementAsts.updated(compoundStatementAsts.size - 1, returnReplaced)
        } else {
          compoundStatementAsts
        }
      } else {
        compoundStatementAsts
      }

    val mainBodyAsts = if (ctx.ensureClause() != null) {
      val ensureAsts = astForStatementsContext(ctx.ensureClause().compoundStatement().statements())
      compoundStatementAstsWithReturn ++ ensureAsts
    } else {
      compoundStatementAstsWithReturn
    }

    val rescueAsts = ctx
      .rescueClause()
      .asScala
      .map(astForRescueClauseContext(_))
      .toSeq

    if (ctx.elseClause() != null) {
      val elseClauseAsts = astForElseClauseContext(ctx.elseClause())
      mainBodyAsts ++ rescueAsts ++ elseClauseAsts
    } else {
      mainBodyAsts ++ rescueAsts
    }
  }

  def astForMethodDefinitionContext(ctx: MethodDefinitionContext): Seq[Ast] = {
    scope.pushNewScope(())
    val astMethodParam = astForMethodParameterPartContext(ctx.methodParameterPart())
    val astMethodName  = astForMethodNamePartContext(ctx.methodNamePart())
    val callNode       = astMethodName.head.nodes.filter(node => node.isInstanceOf[NewCall]).head.asInstanceOf[NewCall]
    // there can be only one call node
    val astBody = astForBodyStatementContext(ctx.bodyStatement(), true)
    scope.popScope()

    /*
     * The method astForMethodNamePartContext() returns a call node in the AST.
     * This is because it has been called from several places some of which need a call node.
     * We will use fields from the call node to construct the method node. Post that,
     * we will discard the call node since it is of no further use to us
     */

    val classPath = classStack.toList.mkString(".") + "."
    val methodNode = NewMethod()
      .code(ctx.getText)
      .name(callNode.name)
      .fullName(s"$filename:${callNode.name}")
      .columnNumber(callNode.columnNumber)
      .lineNumber(callNode.lineNumber)
      .lineNumberEnd(ctx.END().getSymbol.getLine)
      .filename(filename)
    callNode.methodFullName(classPath + callNode.name)

    val methodRetNode = NewMethodReturn()
      .lineNumber(None)
      .columnNumber(None)
      .typeFullName(Defines.Any)

    val publicModifier = NewModifier().modifierType(ModifierTypes.PUBLIC)
    /*
     * public/private/protected modifiers are in a separate statement
     * TODO find out how they should be used. Need to do this iff it adds any value
     */

    val paramSeq = astMethodParam.head.nodes
      .map(node => {
        Ast(node)
      })
      .toSeq

    methodNames.add(methodNode.name)
    val blockNode = NewBlock().typeFullName(Defines.Any)
    Seq(
      methodAst(
        methodNode,
        paramSeq,
        blockAst(blockNode, astBody.toList),
        methodRetNode,
        Seq[NewModifier](publicModifier)
      )
    )
  }

  def astForMethodOnlyIdentifierPrimaryContext(ctx: MethodOnlyIdentifierPrimaryContext): Seq[Ast] = {
    astForMethodOnlyIdentifier(ctx.methodOnlyIdentifier())
  }

  def astForModuleDefinitionPrimaryContext(ctx: ModuleDefinitionPrimaryContext): Seq[Ast] = {
    val referenceAsts = astForClassOrModuleReferenceContext(ctx.moduleDefinition().classOrModuleReference())
    val bodyStmtAsts  = astForBodyStatementContext(ctx.moduleDefinition().bodyStatement())
    if (classStack.size > 0) {
      classStack.pop()
    }
    val bodyAstSansModifiers = bodyStmtAsts
      .filterNot(ast => {
        val nodes = ast.nodes
          .filter(_.isInstanceOf[NewIdentifier])

        if (nodes.size == 1) {
          val varName = nodes
            .map(_.asInstanceOf[NewIdentifier].name)
            .head
          varName == "public" || varName == "protected" || varName == "private"
        } else {
          false
        }
      })
    Seq(referenceAsts.head.withChildren(bodyAstSansModifiers))
  }

  def astForMultipleAssignmentExpressionContext(ctx: MultipleAssignmentExpressionContext): Seq[Ast] = {
    val lhsAsts      = astForMultipleLeftHandSideContext(ctx.multipleLeftHandSide())
    val rhsAsts      = astForMultipleRightHandSideContext(ctx.multipleRightHandSide())
    val operatorName = getOperatorName(ctx.EQ().getSymbol)
    val callNode = NewCall()
      .name(operatorName)
      .code(ctx.getText)
      .methodFullName(operatorName)
      .signature("")
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .typeFullName(Defines.Any)
      .lineNumber(ctx.EQ().getSymbol().getLine())
      .columnNumber(ctx.EQ().getSymbol().getCharPositionInLine())
    Seq(callAst(callNode, lhsAsts ++ rhsAsts))
  }

  def astForRangeExpressionContext(ctx: RangeExpressionContext): Seq[Ast] = {
    if (ctx.expression().size() == 2) {
      astForBinaryExpression(ctx.expression(0), ctx.expression(1), ctx.op, ctx.getText)
    } else {
      Seq(Ast())
    }
  }

  def astForRelationalExpressionContext(ctx: RelationalExpressionContext): Seq[Ast] = {
    astForBinaryExpression(ctx.expression(0), ctx.expression(1), ctx.op, ctx.getText)
  }

  def astForBinaryExpression(
    lhs: ExpressionContext,
    rhs: ExpressionContext,
    operatorToken: Token,
    code: String
  ): Seq[Ast] = {
    val lhsExpressionAsts = astForExpressionContext(lhs)
    val rhsExpressionAsts = astForExpressionContext(rhs)
    val operatorName      = getOperatorName(operatorToken)
    val callNode = NewCall()
      .name(operatorName)
      .code(code)
      .methodFullName(operatorName)
      .signature("")
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .typeFullName(Defines.Any)
      .lineNumber(operatorToken.getLine())
      .columnNumber(operatorToken.getCharPositionInLine())
    Seq(callAst(callNode, lhsExpressionAsts ++ rhsExpressionAsts))
  }

  def astForSimpleScopedConstantReferencePrimaryContext(ctx: SimpleScopedConstantReferencePrimaryContext): Seq[Ast] = {
    val localVar  = ctx.CONSTANT_IDENTIFIER()
    val varSymbol = localVar.getSymbol()
    val node      = createIdentifierWithScope(ctx, varSymbol.getText, varSymbol.getText, Defines.Any, List(Defines.Any))

    val operatorName = getOperatorName(ctx.COLON2().getSymbol)
    val callNode = NewCall()
      .name(operatorName)
      .code(ctx.getText)
      .methodFullName(operatorName)
      .signature("")
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .typeFullName(Defines.Any)
      .lineNumber(ctx.COLON2().getSymbol.getLine)
      .columnNumber(ctx.COLON2().getSymbol.getCharPositionInLine())

    Seq(callAst(callNode, Seq(Ast(node))))

  }

  def astForSuperExpressionPrimaryContext(ctx: SuperExpressionPrimaryContext): Seq[Ast] = {
    val argAsts = astForArgumentsWithParenthesesContext(ctx.argumentsWithParentheses())
    if (ctx.block() != null) {
      argAsts ++ astForBlockContext(ctx.block())
    } else {
      argAsts
    }
  }

  def astForCommandWithDoBlockContext(ctx: CommandWithDoBlockContext): Seq[Ast] = ctx match {
    case ctx: ArgsAndDoBlockCommandWithDoBlockContext =>
      val argsAsts   = astForArgumentsWithoutParenthesesContext(ctx.argumentsWithoutParentheses())
      val doBlockAst = astForDoBlockContext(ctx.doBlock())
      argsAsts ++ doBlockAst
    case ctx: RubyParser.ArgsAndDoBlockAndMethodIdCommandWithDoBlockContext =>
      val argsAsts     = astForArgumentsWithoutParenthesesContext(ctx.argumentsWithoutParentheses())
      val doBlockAsts  = astForDoBlockContext(ctx.doBlock())
      val methodIdAsts = astForMethodIdentifierContext(ctx.methodIdentifier(), ctx.getText, true)
      methodIdAsts ++ argsAsts ++ doBlockAsts
    case ctx: RubyParser.PrimaryMethodArgsDoBlockCommandWithDoBlockContext =>
      val argsAsts       = astForArgumentsWithoutParenthesesContext(ctx.argumentsWithoutParentheses())
      val doBlockAsts    = astForDoBlockContext(ctx.doBlock())
      val methodNameAsts = astForMethodNameContext(ctx.methodName())
      val primaryAsts    = astForPrimaryContext(ctx.primary())
      primaryAsts ++ methodNameAsts ++ argsAsts ++ doBlockAsts
    case _ =>
      logger.error("astForCommandWithDoBlockContext() All contexts mismatched.")
      Seq(Ast())
  }

  def astForChainedCommandWithDoBlockContext(ctx: ChainedCommandWithDoBlockContext): Seq[Ast] = {
    val cmdAsts   = astForCommandWithDoBlockContext(ctx.commandWithDoBlock())
    val mNameAsts = ctx.methodName().asScala.flatMap(mName => astForMethodNameContext(mName)).toSeq
    val apAsts = ctx
      .argumentsWithParentheses()
      .asScala
      .flatMap(ap => {
        astForArgumentsWithParenthesesContext(ap)
      })
      .toSeq
    cmdAsts ++ mNameAsts ++ apAsts
  }

  def astForArgumentsWithParenthesesContext(ctx: ArgumentsWithParenthesesContext): Seq[Ast] = ctx match {
    case ctx: BlankArgsArgumentsWithParenthesesContext => Seq(Ast())
    case ctx: ArgsOnlyArgumentsWithParenthesesContext  => astForArgumentsContext(ctx.arguments())
    case ctx: ExpressionsAndChainedCommandWithDoBlockArgumentsWithParenthesesContext =>
      val expAsts = ctx
        .expressions()
        .expression
        .asScala
        .flatMap(exp => {
          astForExpressionContext(exp)
        })
        .toSeq
      val ccDoBlock = astForChainedCommandWithDoBlockContext(ctx.chainedCommandWithDoBlock())
      expAsts ++ ccDoBlock
    case ctx: ChainedCommandWithDoBlockOnlyArgumentsWithParenthesesContext =>
      astForChainedCommandWithDoBlockContext(ctx.chainedCommandWithDoBlock())
    case _ =>
      logger.error("astForArgumentsWithParenthesesContext() All contexts mismatched.")
      Seq(Ast())
  }

  def astForBlockParametersContext(ctx: BlockParametersContext): Seq[Ast] = {
    if (ctx.singleLeftHandSide() != null) {
      astForSingleLeftHandSideContext(ctx.singleLeftHandSide())
    } else if (ctx.multipleLeftHandSide() != null) {
      astForMultipleLeftHandSideContext(ctx.multipleLeftHandSide())
    } else {
      Seq(Ast())
    }
  }

  def astForBlockParameterContext(ctx: BlockParameterContext): Seq[Ast] = {
    if (ctx.blockParameters() != null) {
      astForBlockParametersContext(ctx.blockParameters())
    } else {
      Seq(Ast())
    }
  }

  def astForDoBlockContext(ctx: DoBlockContext): Seq[Ast] = {
    astForBlock(ctx.compoundStatement().statements(), ctx.blockParameter())
  }

  def astForBraceBlockContext(ctx: BraceBlockContext): Seq[Ast] = {
    astForBlock(ctx.compoundStatement().statements(), ctx.blockParameter())
  }

  def astForBlock(ctxStmt: StatementsContext, ctxParam: BlockParameterContext): Seq[Ast] = {
    val stmtAsts  = astForStatementsContext(ctxStmt)
    val blockNode = NewBlock().typeFullName(Defines.Any)
    val retAst = if (ctxParam != null) {
      val bpAsts = astForBlockParameterContext(ctxParam)
      blockAst(blockNode, (bpAsts ++ stmtAsts).toList)
    } else {
      blockAst(blockNode, stmtAsts.toList)
    }
    Seq(retAst)
  }

  def astForBlockContext(ctx: BlockContext): Seq[Ast] = {
    if (ctx.doBlock() != null) {
      astForDoBlockContext(ctx.doBlock())
    } else if (ctx.braceBlock() != null) {
      astForBraceBlockContext(ctx.braceBlock())
    } else {
      Seq(Ast())
    }
  }

  def astForUnaryMinusExpressionContext(ctx: UnaryMinusExpressionContext): Seq[Ast] = {
    val expressionAst = astForExpressionContext(ctx.expression())
    if (methodNameAsIdentifierStack.size > 0) {
      /*
       * This is incorrectly identified as a unary expression since the parser identifies the LHS as methodIdentifier
       * MINUS is to be interpreted as a binary operator
       */

      val queuedAst = methodNameAsIdentifierStack.pop()
      val lhsAst =
        queuedAst.nodes
          .filter(node => node.isInstanceOf[NewCall])
          .headOption match {
          case Some(node) =>
            /*
             * IDENTIFIER node incorrectly created as a call node since a binary subtraction operation
             * was identifier as unary - due to parser limitations
             */
            val incorrectCallNode = node.asInstanceOf[NewCall]
            val identifierNode =
              createIdentifierWithScope(ctx, incorrectCallNode.name, incorrectCallNode.name, Defines.Any, Seq())
            Ast(identifierNode)
          case None =>
            queuedAst
        }

      val lhsCode = lhsAst.nodes
        .filter(node => node.isInstanceOf[NewIdentifier])
        .head
        .asInstanceOf[NewIdentifier]
        .code

      val operatorName = Operators.subtraction
      val callNode = NewCall()
        .name(operatorName)
        .code(lhsCode + ctx.getText.filterNot(_.isWhitespace))
        .methodFullName(operatorName)
        .signature("")
        .dispatchType(DispatchTypes.STATIC_DISPATCH)
        .typeFullName(Defines.Any)
        .lineNumber(ctx.MINUS().getSymbol.getLine())
        .columnNumber(ctx.MINUS().getSymbol.getCharPositionInLine())

      Seq(callAst(callNode, Seq(lhsAst) ++ expressionAst))
    } else {
      val operatorName = Operators.minus
      val callNode = NewCall()
        .name(operatorName)
        .code(ctx.getText)
        .methodFullName(operatorName)
        .signature("")
        .dispatchType(DispatchTypes.STATIC_DISPATCH)
        .typeFullName(Defines.Any)
        .lineNumber(ctx.MINUS().getSymbol().getLine())
        .columnNumber(ctx.MINUS().getSymbol().getCharPositionInLine())
      Seq(callAst(callNode, expressionAst))
    }
  }

  def astForUnlessExpressionPrimaryContext(ctx: UnlessExpressionPrimaryContext): Seq[Ast] = {
    val conditionAsts = astForExpressionOrCommand(ctx.unlessExpression().expressionOrCommand())
    val thenAsts      = astForThenClauseContext(ctx.unlessExpression().thenClause())
    val elseAsts      = astForElseClauseContext(ctx.unlessExpression().elseClause())

    // unless will be modelled as IF since there is no difference from a static analysis POV
    val unlessNode = NewControlStructure()
      .controlStructureType(ControlStructureTypes.IF)
      .code(ctx.getText)
      .lineNumber(ctx.unlessExpression().UNLESS().getSymbol.getLine)
      .columnNumber(ctx.unlessExpression().UNLESS().getSymbol.getCharPositionInLine)

    Seq(controlStructureAst(unlessNode, conditionAsts.headOption, List(thenAsts ++ elseAsts).flatten))
  }

  def astForUntilExpressionContext(ctx: UntilExpressionContext): Seq[Ast] = {
    // until will be modelled as a while
    val untilCondAst = astForExpressionOrCommand(ctx.expressionOrCommand()).headOption
    val doClauseAsts = astForDoClauseContext(ctx.doClause())

    val ast = whileAst(
      untilCondAst,
      doClauseAsts,
      Some(ctx.getText),
      Some(ctx.UNTIL().getSymbol.getLine),
      Some(ctx.UNTIL().getSymbol.getCharPositionInLine)
    )
    Seq(ast)
  }

  private def astForPseudoVariableIdentifierContext(ctx: PseudoVariableIdentifierContext): Ast = ctx match {
    case ctx: NilPseudoVariableIdentifierContext      => astForNilLiteral(ctx)
    case ctx: TruePseudoVariableIdentifierContext     => astForTrueLiteral(ctx)
    case ctx: FalsePseudoVariableIdentifierContext    => astForFalseLiteral(ctx)
    case ctx: SelfPseudoVariableIdentifierContext     => astForSelfPseudoIdentifier(ctx)
    case ctx: FilePseudoVariableIdentifierContext     => astForFilePseudoIdentifier(ctx)
    case ctx: LinePseudoVariableIdentifierContext     => astForLinePseudoIdentifier(ctx)
    case ctx: EncodingPseudoVariableIdentifierContext => astForEncodingPseudoIdentifier(ctx)
  }

  def astForVariableRefenceContext(ctx: RubyParser.VariableReferenceContext): Seq[Ast] = {
    if (ctx.variableIdentifier() != null) {
      astForVariableIdentifierContext(ctx.variableIdentifier())
    } else {
      Seq(astForPseudoVariableIdentifierContext(ctx.pseudoVariableIdentifier()))
    }
  }

  def astForVariableReferencePrimaryContext(ctx: VariableReferencePrimaryContext): Seq[Ast] = {
    astForVariableRefenceContext(ctx.variableReference())
  }

  def astForDoClauseContext(ctx: DoClauseContext): Seq[Ast] = {
    astForStatementsContext(ctx.compoundStatement().statements())
  }

  def astForWhileExpressionContext(ctx: WhileExpressionContext): Seq[Ast] = {
    val whileCondAst = astForExpressionOrCommand(ctx.expressionOrCommand()).headOption
    val doClauseAsts = astForDoClauseContext(ctx.doClause())

    val ast = whileAst(
      whileCondAst,
      doClauseAsts,
      Some(ctx.getText),
      Some(ctx.WHILE().getSymbol.getLine),
      Some(ctx.WHILE().getSymbol.getCharPositionInLine)
    )
    Seq(ast)
  }

  def astForBlockArgumentContext(ctx: BlockArgumentContext): Seq[Ast] = {
    if (ctx == null) return Seq(Ast())
    astForExpressionContext(ctx.expression())
  }

  def astForBlockArgumentTypeArgumentsContext(ctx: BlockArgumentTypeArgumentsContext): Seq[Ast] = {
    astForBlockArgumentContext(ctx.blockArgument())
  }

  def astForBlockSplattingTypeArgumentsContext(ctx: BlockSplattingTypeArgumentsContext): Seq[Ast] = {
    val splatAst = astForSplattingArgumentContext(ctx.splattingArgument())
    if (ctx.blockArgument() != null) {
      val blockArgAst = astForBlockArgumentContext(ctx.blockArgument())
      blockArgAst ++ splatAst
    } else {
      splatAst
    }
  }

  def astForAssociationContext(ctx: AssociationContext): Seq[Ast] = {
    val expr1Asts = astForExpressionContext(ctx.expression().get(0))
    val expr2Asts = astForExpressionContext(ctx.expression().get(1))

    val terminalNode =
      if (ctx.COLON() != null) ctx.COLON()
      else ctx.EQGT()

    val operatorText = getOperatorName(terminalNode.getSymbol)
    val callNode = NewCall()
      .name(operatorText)
      .code(ctx.getText)
      .methodFullName(operatorText)
      .signature("")
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .typeFullName(Defines.Any)
      .lineNumber(terminalNode.getSymbol.getLine)
      .columnNumber(terminalNode.getSymbol.getCharPositionInLine)
    Seq(callAst(callNode, expr1Asts ++ expr2Asts))
  }

  def astForAssociationsContext(ctx: AssociationsContext): Seq[Ast] = {
    ctx
      .association()
      .asScala
      .flatMap(assoc => {
        astForAssociationContext(assoc)
      })
      .toSeq
  }

  def astForBlockSplattingExprAssocTypeArgumentsContext(ctx: BlockSplattingExprAssocTypeArgumentsContext): Seq[Ast] = {
    val blockArgAsts     = astForBlockArgumentContext(ctx.blockArgument())
    val splatAsts        = astForSplattingArgumentContext(ctx.splattingArgument())
    val associationsAsts = astForAssociationsContext(ctx.associations())
    val expAsts          = ctx.expressions().expression().asScala.flatMap(exp => astForExpressionContext(exp)).toSeq
    blockArgAsts ++ splatAsts ++ associationsAsts ++ expAsts
  }

  def astForBlockExprAssocTypeArgumentsContext(ctx: BlockExprAssocTypeArgumentsContext): Seq[Ast] = {
    val listAsts = ListBuffer[Ast]()

    if (ctx.blockArgument() != null) {
      listAsts.addAll(astForBlockArgumentContext(ctx.blockArgument()))
    }

    if (ctx.associations() != null) {
      listAsts.addAll(astForAssociationsContext(ctx.associations()))
    } else {
      val exprAsts = ctx.expressions().expression().asScala.flatMap(exp => astForExpressionContext(exp))
      listAsts.addAll(exprAsts)
    }

    listAsts.toSeq
  }

  def astForArgumentsWithoutParenthesesContext(ctx: ArgumentsWithoutParenthesesContext): Seq[Ast] = {
    astForArgumentsContext(ctx.arguments())
  }

  def astForCommandContext(ctx: CommandContext): Seq[Ast] = {
    if (ctx.SUPER() != null) {
      astForArgumentsWithoutParenthesesContext(ctx.argumentsWithoutParentheses())
    } else if (ctx.YIELD() != null) {
      astForArgumentsWithoutParenthesesContext(ctx.argumentsWithoutParentheses())
    } else if (ctx.methodIdentifier() != null) {
      val methodIdentifierAsts = astForMethodIdentifierContext(ctx.methodIdentifier(), ctx.getText)
      methodNameAsIdentifierStack.push(methodIdentifierAsts.head)
      val argsAsts = astForArgumentsWithoutParenthesesContext(ctx.argumentsWithoutParentheses())

      val callNodes = methodIdentifierAsts.head.nodes.filter(node => node.isInstanceOf[NewCall])
      if (callNodes.size == 1) {
        val callNode = callNodes.head.asInstanceOf[NewCall]
        if (
          callNode.name == "require" ||
          callNode.name == "require_once" ||
          callNode.name == "load"
        ) {
          val literalImports = argsAsts.head.nodes
            .filter(node => node.isInstanceOf[NewLiteral])

          if (literalImports.size == 1) {
            val importedFile =
              literalImports.head
                .asInstanceOf[NewLiteral]
                .code
            println(s"AST to be created for imported file ${importedFile}")
          } else {
            println(
              s"Cannot process import since it is determined on the fly. Just creating a call node for later processing"
            )
            Seq(callAst(callNode, argsAsts))
          }
        }
        Seq(callAst(callNode, argsAsts))
      } else {
        argsAsts
      }
    } else if (ctx.primary() != null) {
      val argsAst    = astForArgumentsWithoutParenthesesContext(ctx.argumentsWithoutParentheses())
      val primaryAst = astForPrimaryContext(ctx.primary())
      val methodCallNode = astForMethodNameContext(ctx.methodName()).head.nodes.head
        .asInstanceOf[NewCall]
      val callNode = NewCall()
        .name(getActualMethodName(methodCallNode.name))
        .code(ctx.getText)
        .methodFullName(DynamicCallUnknownFullName)
        .signature("")
        .dispatchType(DispatchTypes.STATIC_DISPATCH)
        .typeFullName(Defines.Any)
        .lineNumber(methodCallNode.lineNumber)
        .columnNumber(methodCallNode.columnNumber)
      Seq(callAst(callNode, primaryAst ++ argsAst))
    } else {
      Seq(Ast())
    }
  }

  def astForCommandTypeArgumentsContext(ctx: CommandTypeArgumentsContext): Seq[Ast] = {
    astForCommandContext(ctx.command())
  }

  def astForArgumentsContext(ctx: ArgumentsContext): Seq[Ast] = ctx match {
    case ctx: BlockArgumentTypeArgumentsContext           => astForBlockArgumentTypeArgumentsContext(ctx)
    case ctx: BlockSplattingTypeArgumentsContext          => astForBlockSplattingTypeArgumentsContext(ctx)
    case ctx: BlockSplattingExprAssocTypeArgumentsContext => astForBlockSplattingExprAssocTypeArgumentsContext(ctx)
    case ctx: BlockExprAssocTypeArgumentsContext          => astForBlockExprAssocTypeArgumentsContext(ctx)
    case ctx: CommandTypeArgumentsContext                 => astForCommandTypeArgumentsContext(ctx)
    case _ =>
      logger.error("astForArgumentsContext() All contexts mismatched.")
      Seq(Ast())
  }

  def astForYieldWithOptionalArgumentContext(ctx: YieldWithOptionalArgumentContext): Seq[Ast] = {
    if (ctx.arguments() == null) return Seq(Ast())
    val argsAst      = astForArgumentsContext(ctx.arguments())
    val operatorName = RubyOperators.yieldOp
    val callNode = NewCall()
      .name(operatorName)
      .code(ctx.getText)
      .methodFullName(operatorName)
      .signature("")
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .typeFullName(Defines.Any)
      .lineNumber(ctx.YIELD().getSymbol().getLine())
      .columnNumber(ctx.YIELD().getSymbol().getCharPositionInLine())
    Seq(callAst(callNode, argsAst))
  }

  def astForYieldWithOptionalArgumentPrimaryContext(ctx: YieldWithOptionalArgumentPrimaryContext): Seq[Ast] = {
    astForYieldWithOptionalArgumentContext(ctx.yieldWithOptionalArgument())
  }
}
