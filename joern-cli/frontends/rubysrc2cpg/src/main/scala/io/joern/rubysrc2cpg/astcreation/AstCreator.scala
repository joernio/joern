package io.joern.rubysrc2cpg.astcreation
import io.joern.rubysrc2cpg.parser.RubyParser
import io.joern.rubysrc2cpg.parser.RubyParser.*
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.rubysrc2cpg.utils.PackageContext
import io.joern.x2cpg.Ast.storeInDiffGraph
import io.joern.x2cpg.Defines.DynamicCallUnknownFullName
import io.joern.x2cpg.datastructures.{Global, Scope}
import io.joern.x2cpg.{Ast, AstCreatorBase, AstNodeBuilder, Defines as XDefines}
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import org.antlr.v4.runtime.tree.TerminalNode
import org.antlr.v4.runtime.{ParserRuleContext, Token}
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate

import java.io.File as JFile
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Using}

class AstCreator(
  protected val filename: String,
  global: Global,
  parser: ResourceManagedParser,
  packageContext: PackageContext,
  projectRoot: Option[String] = None
) extends AstCreatorBase(filename)
    with AstNodeBuilder[ParserRuleContext, AstCreator]
    with AstForPrimitivesCreator
    with AstForStatementsCreator
    with AstForExpressionsCreator
    with AstForDeclarationsCreator
    with AstForTypesCreator
    with AstCreatorHelper {

  protected val scope: Scope[String, NewIdentifier, Unit] = new Scope()

  private val logger = LoggerFactory.getLogger(this.getClass)

  protected val classStack = mutable.Stack[String]()

  protected val packageStack = mutable.Stack[String]()

  /*
   * Stack of variable identifiers incorrectly identified as method identifiers
   * Each AST contains exactly one call or identifier node
   */
  protected val methodNameAsIdentifierStack = mutable.Stack[Ast]()

  protected val methodAliases       = mutable.HashMap[String, String]()
  protected val methodNameToMethod  = mutable.HashMap[String, nodes.NewMethod]()
  protected val methodDefInArgument = mutable.ListBuffer[Ast]()

  protected val typeDeclNameToTypeDecl = mutable.HashMap[String, nodes.NewTypeDecl]()

  protected val methodNamesWithYield = mutable.HashSet[String]()

  /*
   *Fake methods created from yield blocks and their yield calls will have this suffix in their names
   */
  protected val YIELD_SUFFIX = "_yield"

  /*
   * This is used to mark call nodes created due to yield calls. This is set in their names at creation.
   * The appropriate name wrt the names of their actual methods is set later in them.
   */
  protected val UNRESOLVED_YIELD = "unresolved_yield"

  protected val pathSep = "."

  protected val blockMethods = ListBuffer[Ast]()

  protected val relativeFilename: String =
    projectRoot.map(filename.stripPrefix).map(_.stripPrefix(JFile.separator)).getOrElse(filename)

  // The below are for adding implicit return nodes to methods

  // This is true if the last statement of a method is being processed. The last statement could be a if-else as well
  protected var processingLastMethodStatement = false
  // a monotonically increasing block id unique within this file
  protected var blockIdCounter = 1
  // block id of the block currently being processed
  protected var currentBlockId = 0
  /*
   * This is a hash of parent block id ---> child block id. If there are multiple children, any one child can be present.
   * The value of this entry for a block is read AFTER its last statement has been processed. Absence of the the block
   * in this hash implies this is a leaf block.
   */
  protected val blockChildHash = mutable.HashMap[Int, Int]()

  protected val builtInCallNames = mutable.HashSet[String]()
  // Hashmap to store used variable names, to avoid duplicates in case of un-named variables
  protected val usedVariableNames = mutable.HashMap.empty[String, Int]

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

  protected def getActualMethodName(name: String): String = {
    methodAliases.getOrElse(name, name)
  }

  override def createAst(): BatchedUpdate.DiffGraphBuilder = {
    parser.parse(filename) match {
      case Success(programCtx) =>
        createAstForProgramCtx(programCtx)
      case Failure(exc) =>
        logger.warn(s"Could not parse file: $filename, skipping")
        logger.warn(exc.getMessage)
        diffGraph
    }
  }

  private def createAstForProgramCtx(programCtx: RubyParser.ProgramContext) = {
    val name     = ":program"
    val fullName = s"$relativeFilename:$name"
    val programMethod =
      NewMethod()
        .order(1)
        .name(name)
        .code(name)
        .fullName(fullName)
        .filename(filename)
        .astParentType(NodeTypes.TYPE_DECL)
        .astParentFullName(fullName)

    classStack.push(fullName)

    scope.pushNewScope(())
    val statementAsts =
      if (
        programCtx.compoundStatement() != null &&
        programCtx.compoundStatement().statements() != null
      ) {
        astForStatements(programCtx.compoundStatement().statements(), false, false) ++ blockMethods
      } else {
        logger.error(s"File $filename has no compound statement. Needs to be examined")
        List[Ast](Ast())
      }
    scope.popScope()

    val methodRetNode = NewMethodReturn()
      .lineNumber(None)
      .columnNumber(None)
      .typeFullName(Defines.Any)

    // For all the builtIn's encountered create assignment ast
    val lineColNum = 1
    val builtInMethodAst = builtInCallNames.map { builtInCallName =>
      val identifierNode = NewIdentifier()
        .code(builtInCallName)
        .name(builtInCallName)
        .lineNumber(lineColNum)
        .columnNumber(lineColNum)
        .typeFullName(Defines.Any)
      val typeRefNode = NewTypeRef()
        .code(prefixAsBuiltin(builtInCallName))
        .typeFullName(prefixAsBuiltin(builtInCallName))
        .lineNumber(lineColNum)
        .columnNumber(lineColNum)
      astForAssignment(identifierNode, typeRefNode, Some(lineColNum), Some(lineColNum))
    }.toList

    val methodRefAssignmentAsts = methodNameToMethod.values.map { methodNode =>
      // Create a methodRefNode and assign it to the identifier version of the method, which will help in type propogation to resolve calls
      val methodRefNode = NewMethodRef()
        .code("def " + methodNode.name + "(...)")
        .methodFullName(methodNode.fullName)
        .typeFullName(methodNode.fullName)
        .lineNumber(lineColNum)
        .columnNumber(lineColNum)

      val methodNameIdentifier = NewIdentifier()
        .code(methodNode.name)
        .name(methodNode.name)
        .typeFullName(Defines.Any)
        .lineNumber(lineColNum)
        .columnNumber(lineColNum)

      val methodRefAssignmentAst =
        astForAssignment(methodNameIdentifier, methodRefNode, methodNode.lineNumber, methodNode.columnNumber)
      methodRefAssignmentAst
    }.toList

    val typeRefAssignmentAst = typeDeclNameToTypeDecl.values.map { typeDeclNode =>

      val typeRefNode = NewTypeRef()
        .code("class " + typeDeclNode.name + "(...)")
        .typeFullName(typeDeclNode.fullName)
        .lineNumber(typeDeclNode.lineNumber)
        .columnNumber(typeDeclNode.columnNumber)

      val typeDeclNameIdentifier = NewIdentifier()
        .code(typeDeclNode.name)
        .name(typeDeclNode.name)
        .typeFullName(Defines.Any)
        .lineNumber(lineColNum)
        .columnNumber(lineColNum)

      val typeRefAssignmentAst =
        astForAssignment(typeDeclNameIdentifier, typeRefNode, typeDeclNode.lineNumber, typeDeclNode.columnNumber)
      typeRefAssignmentAst
    }

    val methodDefInArgumentAsts = methodDefInArgument.toList

    val blockNode = NewBlock().typeFullName(Defines.Any)
    val programAst =
      methodAst(
        programMethod,
        Seq(Ast()),
        blockAst(
          blockNode,
          statementAsts.toList ++ builtInMethodAst ++ methodRefAssignmentAsts ++ typeRefAssignmentAst ++ methodDefInArgumentAsts
        ),
        methodRetNode
      )

    val fileNode       = NewFile().name(filename).order(1)
    val namespaceBlock = globalNamespaceBlock()
    val ast            = Ast(fileNode).withChild(Ast(namespaceBlock).withChild(programAst))

    classStack.popAll()

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
    val superKeyword            = "<operator>.super"
    val stringConcatenation     = "<operator>.stringConcatenation"
    val formattedString         = "<operator>.formatString"
    val formattedValue          = "<operator>.formatValue"
  }
  private def getOperatorName(token: Token): String = token.getType match {
    case ASSIGNMENT_OPERATOR => Operators.assignment
    case DOT2                => Operators.range
    case DOT3                => Operators.range
    case EMARK               => Operators.not
    case EQ                  => Operators.assignment
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

  def astForSingleLeftHandSideContext(ctx: SingleLeftHandSideContext): Seq[Ast] = ctx match {
    case ctx: VariableIdentifierOnlySingleLeftHandSideContext =>
      Seq(astForVariableIdentifierHelper(ctx.variableIdentifier(), true))
    case ctx: PrimaryInsideBracketsSingleLeftHandSideContext =>
      val primaryAsts = astForPrimaryContext(ctx.primary())
      val argsAsts    = astForArguments(ctx.arguments())
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
      // TODO handle obj.foo=arg being interpreted as obj.foo(arg) here.
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
      val varSymbol = localVar.getSymbol
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
        .columnNumber(localVar.getSymbol.getCharPositionInLine)
      Seq(callAst(callNode, xAsts ++ Seq(yAst)))
    case ctx: ScopedConstantAccessSingleLeftHandSideContext =>
      val localVar  = ctx.CONSTANT_IDENTIFIER()
      val varSymbol = localVar.getSymbol
      val node = createIdentifierWithScope(ctx, varSymbol.getText, varSymbol.getText, Defines.Any, List(Defines.Any))
      Seq(Ast(node))
    case _ =>
      logger.error(s"astForSingleLeftHandSideContext() $filename, ${ctx.getText} All contexts mismatched.")
      Seq(Ast())

  }

  def astForMultipleRightHandSideContext(ctx: MultipleRightHandSideContext): Seq[Ast] = {
    if (ctx == null) return Seq(Ast())

    val expCmd = ctx.expressionOrCommands()
    val exprAsts = Option(expCmd) match
      case Some(expCmd) =>
        expCmd.expressionOrCommand().asScala.flatMap(astForExpressionOrCommand).toSeq
      case None =>
        Seq()

    val paramAsts = if (ctx.splattingArgument() != null) {
      val splattingAsts = astForExpressionOrCommand(ctx.splattingArgument().expressionOrCommand())
      exprAsts ++ splattingAsts
    } else {
      exprAsts
    }

    paramAsts
  }

  def astForSingleAssignmentExpressionContext(ctx: SingleAssignmentExpressionContext): Seq[Ast] = {
    val rightAst = astForMultipleRightHandSideContext(ctx.multipleRightHandSide())
    val leftAst  = astForSingleLeftHandSideContext(ctx.singleLeftHandSide())

    val operatorName = getOperatorName(ctx.op)

    if (leftAst.size == 1 && rightAst.size > 1) {
      /*
       * This is multiple RHS packed into a single LHS. That is, packing left hand side.
       * This is as good as multiple RHS packed into an array and put into a single LHS
       */
      val callNode = NewCall()
        .name(operatorName)
        .code(ctx.getText)
        .methodFullName(operatorName)
        .dispatchType(DispatchTypes.STATIC_DISPATCH)
        .typeFullName(Defines.Any)
        .lineNumber(ctx.op.getLine)
        .columnNumber(ctx.op.getCharPositionInLine)

      val packedRHS = getPackedRHS(rightAst)
      Seq(callAst(callNode, leftAst ++ packedRHS))
    } else {
      val callNode = NewCall()
        .name(operatorName)
        .code(ctx.op.getText)
        .methodFullName(operatorName)
        .signature("")
        .dispatchType(DispatchTypes.STATIC_DISPATCH)
        .typeFullName(Defines.Any)
        .lineNumber(ctx.op.getLine)
        .columnNumber(ctx.op.getCharPositionInLine)
      Seq(callAst(callNode, leftAst ++ rightAst))
    }
  }

  def astForStringInterpolationContext(ctx: InterpolatedStringExpressionContext): Seq[Ast] = {
    val varAsts = ctx
      .stringInterpolation()
      .interpolatedStringSequence()
      .asScala
      .flatMap(inter => {
        Seq(
          Ast(
            NewCall()
              .code(inter.getText)
              .name(RubyOperators.formattedValue)
              .methodFullName(RubyOperators.formattedValue)
              .lineNumber(line(ctx))
              .columnNumber(column(ctx))
              .typeFullName(Defines.Any)
              .dispatchType(DispatchTypes.STATIC_DISPATCH)
          )
        ) ++
          astForStatements(inter.compoundStatement().statements(), false, false)
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
    case ctx: ClassDefinitionPrimaryContext if ctx.hasClassDefinition => astForClassDeclaration(ctx)
    case ctx: ClassDefinitionPrimaryContext                           => astForClassExpression(ctx)
    case ctx: ModuleDefinitionPrimaryContext                          => astForModuleDefinitionPrimaryContext(ctx)
    case ctx: MethodDefinitionPrimaryContext => astForMethodDefinitionContext(ctx.methodDefinition())
    case ctx: ProcDefinitionPrimaryContext   => astForProcDefinitionContext(ctx.procDefinition())
    case ctx: YieldWithOptionalArgumentPrimaryContext =>
      Seq(astForYieldCall(ctx, Option(ctx.yieldWithOptionalArgument().arguments())))
    case ctx: IfExpressionPrimaryContext     => Seq(astForIfExpression(ctx.ifExpression()))
    case ctx: UnlessExpressionPrimaryContext => Seq(astForUnlessExpression(ctx.unlessExpression()))
    case ctx: CaseExpressionPrimaryContext   => astForCaseExpressionPrimaryContext(ctx)
    case ctx: WhileExpressionPrimaryContext  => Seq(astForWhileExpression(ctx.whileExpression()))
    case ctx: UntilExpressionPrimaryContext  => Seq(astForUntilExpression(ctx.untilExpression()))
    case ctx: ForExpressionPrimaryContext    => Seq(astForForExpression(ctx.forExpression()))
    case ctx: ReturnWithParenthesesPrimaryContext =>
      Seq(
        returnAst(returnNode(ctx, ctx.getText), astForArgumentsWithParenthesesContext(ctx.argumentsWithParentheses()))
      )
    case ctx: JumpExpressionPrimaryContext     => astForJumpExpressionPrimaryContext(ctx)
    case ctx: BeginExpressionPrimaryContext    => astForBeginExpressionPrimaryContext(ctx)
    case ctx: GroupingExpressionPrimaryContext => astForCompoundStatement(ctx.compoundStatement(), false, false)
    case ctx: VariableReferencePrimaryContext  => Seq(astForVariableReference(ctx.variableReference()))
    case ctx: SimpleScopedConstantReferencePrimaryContext =>
      astForSimpleScopedConstantReferencePrimaryContext(ctx)
    case ctx: ChainedScopedConstantReferencePrimaryContext =>
      astForChainedScopedConstantReferencePrimaryContext(ctx)
    case ctx: ArrayConstructorPrimaryContext => astForArrayLiteral(ctx.arrayConstructor())
    case ctx: HashConstructorPrimaryContext  => astForHashConstructorPrimaryContext(ctx)
    case ctx: LiteralPrimaryContext          => astForLiteralPrimaryExpression(ctx)
    case ctx: StringExpressionPrimaryContext => astForStringExpression(ctx.stringExpression)
    case ctx: RegexInterpolationPrimaryContext =>
      astForRegexInterpolationPrimaryContext(ctx.regexInterpolation)
    case ctx: IsDefinedPrimaryContext                 => Seq(astForIsDefinedPrimaryExpression(ctx))
    case ctx: SuperExpressionPrimaryContext           => Seq(astForSuperExpression(ctx))
    case ctx: IndexingExpressionPrimaryContext        => astForIndexingExpressionPrimaryContext(ctx)
    case ctx: MethodOnlyIdentifierPrimaryContext      => astForMethodOnlyIdentifier(ctx.methodOnlyIdentifier())
    case ctx: InvocationWithBlockOnlyPrimaryContext   => astForInvocationWithBlockOnlyPrimaryContext(ctx)
    case ctx: InvocationWithParenthesesPrimaryContext => astForInvocationWithParenthesesPrimaryContext(ctx)
    case ctx: ChainedInvocationPrimaryContext         => astForChainedInvocationPrimaryContext(ctx)
    case ctx: ChainedInvocationWithoutArgumentsPrimaryContext =>
      astForChainedInvocationWithoutArgumentsPrimaryContext(ctx)
    case _ =>
      logger.error(s"astForPrimaryContext() $filename, ${ctx.getText} All contexts mismatched.")
      Seq(Ast())
  }

  def astForExpressionContext(ctx: ExpressionContext): Seq[Ast] = ctx match {
    case ctx: PrimaryExpressionContext             => astForPrimaryContext(ctx.primary())
    case ctx: UnaryExpressionContext               => Seq(astForUnaryExpression(ctx))
    case ctx: PowerExpressionContext               => Seq(astForPowerExpression(ctx))
    case ctx: UnaryMinusExpressionContext          => Seq(astForUnaryMinusExpression(ctx))
    case ctx: MultiplicativeExpressionContext      => Seq(astForMultiplicativeExpression(ctx))
    case ctx: AdditiveExpressionContext            => Seq(astForAdditiveExpression(ctx))
    case ctx: BitwiseShiftExpressionContext        => Seq(astForBitwiseShiftExpression(ctx))
    case ctx: BitwiseAndExpressionContext          => Seq(astForBitwiseAndExpression(ctx))
    case ctx: BitwiseOrExpressionContext           => Seq(astForBitwiseOrExpression(ctx))
    case ctx: RelationalExpressionContext          => Seq(astForRelationalExpression(ctx))
    case ctx: EqualityExpressionContext            => Seq(astForEqualityExpression(ctx))
    case ctx: OperatorAndExpressionContext         => Seq(astForAndExpression(ctx))
    case ctx: OperatorOrExpressionContext          => Seq(astForOrExpression(ctx))
    case ctx: RangeExpressionContext               => astForRangeExpressionContext(ctx)
    case ctx: ConditionalOperatorExpressionContext => Seq(astForTernaryConditionalOperator(ctx))
    case ctx: SingleAssignmentExpressionContext    => astForSingleAssignmentExpressionContext(ctx)
    case ctx: MultipleAssignmentExpressionContext  => astForMultipleAssignmentExpressionContext(ctx)
    case ctx: IsDefinedExpressionContext           => Seq(astForIsDefinedExpression(ctx))
    case _ =>
      logger.error(s"astForExpressionContext() $filename, ${ctx.getText} All contexts mismatched.")
      Seq(Ast())
  }

  def astForProcDefinitionContext(ctx: ProcDefinitionContext): Seq[Ast] = {
    /*
     * Model a proc as a method
     */
    // Note: For parameters in the Proc definiton, an implicit parameter which goes by the name of `this` is added to the cpg
    val astMethodParam = astForParametersContext(ctx.parameters())
    scope.pushNewScope(())
    val astBody = astForCompoundStatement(ctx.block.compoundStatement, true)
    scope.popScope()

    val procId = blockIdCounter
    blockIdCounter += 1
    val procMethodName = "proc_" + procId

    val methodFullName = classStack.reverse :+ procMethodName mkString pathSep
    val methodNode = NewMethod()
      .code(ctx.getText)
      .name(procMethodName)
      .fullName(methodFullName)
      .filename(filename)

    val methodRetNode = NewMethodReturn()
      .typeFullName(Defines.Any)

    val publicModifier = NewModifier().modifierType(ModifierTypes.PUBLIC)

    val blockNode = NewBlock().typeFullName(Defines.Any)
    val methAst = methodAst(
      methodNode,
      astMethodParam,
      blockAst(blockNode, astBody.toList),
      methodRetNode,
      Seq[NewModifier](publicModifier)
    )
    blockMethods.addOne(methAst)

    val callArgs = astMethodParam
      .map(ast => {
        val param = ast.nodes.head.asInstanceOf[NewMethodParameterIn]
        val node  = createIdentifierWithScope(ctx, param.name, param.code, Defines.Any, Seq())
        Ast(node)
      })

    val callNode = NewCall()
      .name(procMethodName)
      .methodFullName(methodFullName)
      .typeFullName(Defines.Any)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .code(ctx.getText)
    Seq(callAst(callNode, callArgs))
  }

  def astForDefinedMethodNameOrSymbolContext(ctx: DefinedMethodNameOrSymbolContext): Seq[Ast] = {
    if (ctx == null) return Seq(Ast())

    if (ctx.definedMethodName() != null) {
      astForDefinedMethodNameContext(ctx.definedMethodName())
    } else {
      Seq(astForSymbolLiteral(ctx.symbol()))
    }
  }

  def astForIndexingArgumentsContext(ctx: IndexingArgumentsContext): Seq[Ast] = ctx match {
    case ctx: RubyParser.CommandOnlyIndexingArgumentsContext =>
      astForCommand(ctx.command())
    case ctx: RubyParser.ExpressionsOnlyIndexingArgumentsContext =>
      ctx
        .expressions()
        .expression()
        .asScala
        .flatMap(exp => {
          astForExpressionContext(exp)
        })
        .toSeq
    case ctx: RubyParser.ExpressionsAndSplattingIndexingArgumentsContext =>
      val expAsts = ctx
        .expressions()
        .expression()
        .asScala
        .flatMap(exp => {
          astForExpressionContext(exp)
        })
        .toSeq
      val splatAsts = astForExpressionOrCommand(ctx.splattingArgument().expressionOrCommand())
      val callNode = NewCall()
        .name(ctx.COMMA().getText)
        .methodFullName(Operators.arrayInitializer)
        .signature(Operators.arrayInitializer)
        .typeFullName(Defines.Any)
        .dispatchType(DispatchTypes.STATIC_DISPATCH)
        .code(ctx.getText)
        .lineNumber(ctx.COMMA().getSymbol.getLine)
        .columnNumber(ctx.COMMA().getSymbol.getCharPositionInLine)
      Seq(callAst(callNode, expAsts ++ splatAsts))
    case ctx: AssociationsOnlyIndexingArgumentsContext =>
      astForAssociationsContext(ctx.associations())
    case ctx: RubyParser.SplattingOnlyIndexingArgumentsContext =>
      astForExpressionOrCommand(ctx.splattingArgument().expressionOrCommand())
    case _ =>
      logger.error(s"astForIndexingArgumentsContext() $filename, ${ctx.getText} All contexts mismatched.")
      Seq(Ast())
  }

  def astForBeginExpressionPrimaryContext(ctx: BeginExpressionPrimaryContext): Seq[Ast] = {
    astForBodyStatementContext(ctx.beginExpression().bodyStatement())
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
        expAsts ++ astForExpressionOrCommand(ctx.splattingArgument().expressionOrCommand())
      } else {
        expAsts
      }

    val blockNode = NewBlock().typeFullName(Defines.Any)
    Seq(blockAst(blockNode, asts))
  }

  def astForCaseExpressionPrimaryContext(ctx: CaseExpressionPrimaryContext): Seq[Ast] = {
    val code       = ctx.caseExpression().CASE().getText
    val switchNode = controlStructureNode(ctx, ControlStructureTypes.SWITCH, code)
    val conditionAst = Option(ctx.caseExpression().expressionOrCommand()).toList
      .flatMap(astForExpressionOrCommand)
      .headOption

    val whenThenAstsList = ctx
      .caseExpression()
      .whenClause()
      .asScala
      .flatMap(wh => {
        val whenNode = NewJumpTarget()
          .parserTypeName(wh.getClass.getSimpleName)
          .name("case " + wh.getText)
          .code(wh.getText)
          .lineNumber(wh.WHEN().getSymbol.getLine)
          .columnNumber(wh.WHEN().getSymbol.getCharPositionInLine)

        val whenACondAsts = astForWhenArgumentContext(wh.whenArgument())
        val thenAsts = astForCompoundStatement(wh.thenClause().compoundStatement()) ++ Seq(
          Ast(NewControlStructure().controlStructureType(ControlStructureTypes.BREAK))
        )
        Seq(Ast(whenNode)) ++ whenACondAsts ++ thenAsts
      })
      .toList

    val stmtAsts = whenThenAstsList ++ Option(ctx.caseExpression().elseClause())
      .map(ctx => astForCompoundStatement(ctx.compoundStatement()))
      .getOrElse(Seq())
    val block = blockNode(ctx.caseExpression())
    Seq(controlStructureAst(switchNode, conditionAst, Seq(Ast(block).withChildren(stmtAsts))))
  }

  def astForChainedInvocationPrimaryContext(ctx: ChainedInvocationPrimaryContext): Seq[Ast] = {
    val methodNameAst = astForMethodNameContext(ctx.methodName())
    val primaryAst    = astForPrimaryContext(ctx.primary())

    val terminalNode = if (ctx.COLON2() != null) {
      ctx.COLON2()
    } else if (ctx.DOT() != null) {
      ctx.DOT()
    } else {
      ctx.AMPDOT()
    }

    val argsAst = if (ctx.argumentsWithParentheses() != null) {
      astForArgumentsWithParenthesesContext(ctx.argumentsWithParentheses())
    } else {
      Seq()
    }

    if (ctx.block() != null) {
      val blockName = methodNameAst.head.nodes.head
        .asInstanceOf[NewCall]
        .name
      val blockMethodName = blockName + terminalNode.getSymbol.getLine
      val blockMethodAsts =
        astForBlockMethod(
          ctxStmt = ctx.block().compoundStatement.statements(),
          ctxParam = ctx.block().blockParameter,
          blockMethodName,
          line(ctx).head,
          column(ctx).head,
          lineEnd(ctx).head,
          columnEnd(ctx).head
        )
      val blockMethodNode =
        blockMethodAsts.head.nodes.head
          .asInstanceOf[NewMethod]

      blockMethods.addOne(blockMethodAsts.head)

      val callNode = NewCall()
        .name(blockName)
        .methodFullName(blockMethodNode.fullName)
        .typeFullName(Defines.Any)
        .code(blockMethodNode.code)
        .lineNumber(blockMethodNode.lineNumber)
        .columnNumber(blockMethodNode.columnNumber)

      val methodRefNode = NewMethodRef()
        .methodFullName(blockMethodNode.fullName)
        .typeFullName(Defines.Any)
        .code(blockMethodNode.code)
        .lineNumber(blockMethodNode.lineNumber)
        .columnNumber(blockMethodNode.columnNumber)

      Seq(callAst(callNode, Seq(Ast(methodRefNode)), primaryAst.headOption))
    } else {
      val callNode = methodNameAst.head.nodes
        .filter(node => node.isInstanceOf[NewCall])
        .head
        .asInstanceOf[NewCall]

      if (callNode.name == "call" && ctx.primary().isInstanceOf[ProcDefinitionPrimaryContext]) {
        // this is a proc.call
        val baseCallNode = primaryAst.head.nodes.head.asInstanceOf[NewCall]
        Seq(callAst(baseCallNode, argsAst))
      } else {
        callNode
          .code(ctx.getText)
          .lineNumber(terminalNode.getSymbol().getLine())
          .columnNumber(terminalNode.getSymbol().getCharPositionInLine())

        primaryAst.headOption match {
          case Some(value) =>
            if (value.root.map(_.isInstanceOf[NewMethod]).getOrElse(false)) {
              val methodNode = value.root.head.asInstanceOf[NewMethod]
              val methodRefNode = NewMethodRef()
                .code("def " + methodNode.name + "(...)")
                .methodFullName(methodNode.fullName)
                .typeFullName(methodNode.fullName)
              blockMethods.addOne(primaryAst.head)
              Seq(callAst(callNode, Seq(Ast(methodRefNode)) ++ argsAst))
            } else {
              Seq(callAst(callNode, argsAst, primaryAst.headOption))
            }
          case None =>
            Seq(callAst(callNode, argsAst, primaryAst.headOption))
        }
      }
    }
  }

  def astForChainedInvocationWithoutArgumentsPrimaryContext(
    ctx: ChainedInvocationWithoutArgumentsPrimaryContext
  ): Seq[Ast] = {
    val methodNameAst = astForMethodNameContext(ctx.methodName())
    val baseAst       = astForPrimaryContext(ctx.primary())

    val blocksAst = if (ctx.block() != null) {
      Seq(astForBlock(ctx.block()))
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

      paramAsts

    case ctx: PackingLeftHandSideOnlyMultipleLeftHandSideContext =>
      astForPackingLeftHandSideContext(ctx.packingLeftHandSide())
    case ctx: GroupedLeftHandSideOnlyMultipleLeftHandSideContext =>
      astForGroupedLeftHandSideContext(ctx.groupedLeftHandSide())
    case _ =>
      logger.error(s"astForMultipleLeftHandSideContext() $filename, ${ctx.getText} All contexts mismatched.")
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

  // TODO: Clean-up and take into account other hash elements
  def astForHashConstructorPrimaryContext(ctx: HashConstructorPrimaryContext): Seq[Ast] = {
    if (ctx.hashConstructor().hashConstructorElements() == null) return Seq(Ast())
    val hashCtorElemCtxs = ctx.hashConstructor().hashConstructorElements().hashConstructorElement().asScala
    val associationCtxs  = hashCtorElemCtxs.filter(_.association() != null).map(_.association()).toSeq
    val expressionCtxs   = hashCtorElemCtxs.filter(_.expression() != null).map(_.expression()).toSeq
    expressionCtxs.flatMap(astForExpressionContext) ++ associationCtxs.flatMap(astForAssociationContext)
  }

  def astForIndexingExpressionPrimaryContext(ctx: IndexingExpressionPrimaryContext): Seq[Ast] = {
    val lhsExpressionAst = astForPrimaryContext(ctx.primary())
    val rhsExpressionAst = Option(ctx.indexingArguments).map(astForIndexingArgumentsContext).getOrElse(Seq())
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
    case ctx: SingleCommandOnlyInvocationWithoutParenthesesContext => astForCommand(ctx.command())
    case ctx: ChainedCommandDoBlockInvocationWithoutParenthesesContext =>
      astForChainedCommandWithDoBlockContext(ctx.chainedCommandWithDoBlock())
    case ctx: ReturnArgsInvocationWithoutParenthesesContext =>
      val retNode = NewReturn()
        .code(ctx.getText)
        .lineNumber(ctx.RETURN().getSymbol().getLine)
        .columnNumber(ctx.RETURN().getSymbol().getCharPositionInLine)
      val argAst = Option(ctx.arguments).map(astForArguments).getOrElse(Seq())
      Seq(returnAst(retNode, argAst))
    case ctx: BreakArgsInvocationWithoutParenthesesContext =>
      val args = ctx.arguments()
      Option(args) match {
        case Some(args) =>
          /*
           * This is break with args inside a block. The argument passed to break will be returned by the bloc
           * Model this as a return since this is effectively a  return
           */
          val retNode = NewReturn()
            .code(ctx.getText)
            .lineNumber(ctx.BREAK().getSymbol().getLine)
            .columnNumber(ctx.BREAK().getSymbol().getCharPositionInLine)
          val argAst = astForArguments(args)
          Seq(returnAst(retNode, argAst))
        case None =>
          val node = NewControlStructure()
            .controlStructureType(ControlStructureTypes.BREAK)
            .lineNumber(ctx.BREAK().getSymbol.getLine)
            .columnNumber(ctx.BREAK().getSymbol.getCharPositionInLine)
            .code(ctx.getText)
          Seq(
            Ast(node)
              .withChildren(astForArguments(ctx.arguments()))
          )
      }
    case ctx: NextArgsInvocationWithoutParenthesesContext =>
      // failing test case. Exception:  Only jump labels and integer literals are currently supported for continue statements.
      // this overlaps with the problem if returning a value from a block
      val node = NewControlStructure()
        .controlStructureType(ControlStructureTypes.CONTINUE)
        .lineNumber(ctx.NEXT().getSymbol.getLine)
        .columnNumber(ctx.NEXT().getSymbol.getCharPositionInLine)
        .code(Defines.ModifierNext)
      Seq(
        Ast(node)
          .withChildren(astForArguments(ctx.arguments()))
      )
    case _ =>
      logger.error(s"astForInvocationWithoutParenthesesContext() $filename, ${ctx.getText} All contexts mismatched.")
      Seq(Ast())
  }

  def astForInvocationWithBlockOnlyPrimaryContext(ctx: InvocationWithBlockOnlyPrimaryContext): Seq[Ast] = {
    val methodIdAst = astForMethodIdentifierContext(ctx.methodIdentifier(), ctx.getText)
    val blockName = methodIdAst.head.nodes.head
      .asInstanceOf[NewCall]
      .name

    val isYieldMethod = if (blockName.endsWith(YIELD_SUFFIX)) {
      val lookupMethodName = blockName.take(blockName.length - YIELD_SUFFIX.length)
      methodNamesWithYield.contains(lookupMethodName)
    } else {
      false
    }

    if (isYieldMethod) {
      /*
       * This is a yield block. Create a fake method out of it. The yield call will be a call to the yield block
       */
      astForBlockMethod(
        ctx.block().compoundStatement.statements(),
        ctx.block().blockParameter,
        blockName,
        line(ctx).head,
        lineEnd(ctx).head,
        column(ctx).head,
        columnEnd(ctx).head
      )
    } else {
      val blockAst = Seq(astForBlock(ctx.block()))
      // this is expected to be a call node
      val callNode = methodIdAst.head.nodes.head.asInstanceOf[NewCall]
      Seq(callAst(callNode, blockAst))
    }
  }

  def astForInvocationWithParenthesesPrimaryContext(ctx: InvocationWithParenthesesPrimaryContext): Seq[Ast] = {
    val methodIdAst = astForMethodIdentifierContext(ctx.methodIdentifier(), ctx.getText)
    val parenAst    = astForArgumentsWithParenthesesContext(ctx.argumentsWithParentheses())
    val callNode    = methodIdAst.head.nodes.filter(_.isInstanceOf[NewCall]).head.asInstanceOf[NewCall]
    callNode.name(getActualMethodName(callNode.name))

    if (ctx.block() != null) {
      val isYieldMethod = if (callNode.name.endsWith(YIELD_SUFFIX)) {
        val lookupMethodName = callNode.name.take(callNode.name.length - YIELD_SUFFIX.length)
        methodNamesWithYield.contains(lookupMethodName)
      } else {
        false
      }
      if (isYieldMethod) {
        val methAst = astForBlock(ctx.block(), Some(callNode.name))
        blockMethods.addOne(methAst)
        Seq(callAst(callNode, parenAst))
      } else {
        val blockAst = Seq(astForBlock(ctx.block()))
        Seq(callAst(callNode, parenAst ++ blockAst))
      }
    } else
      Seq(callAst(callNode, parenAst))
  }

  def astForJumpExpressionPrimaryContext(ctx: JumpExpressionPrimaryContext): Seq[Ast] = {
    if (ctx.jumpExpression().BREAK() != null) {
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

  def astForSimpleMethodNamePartContext(ctx: SimpleMethodNamePartContext): Seq[Ast] = {
    astForDefinedMethodNameContext(ctx.definedMethodName())
  }

  def astForCallNode(ctx: ParserRuleContext, code: String, isYieldBlock: Boolean = false): Ast = {
    val nameSuffix =
      if (isYieldBlock) {
        YIELD_SUFFIX
      } else {
        ""
      }
    val name = s"${getActualMethodName(ctx.getText)}$nameSuffix"
    // Add the call name to the global builtIn callNames set
    if (isBuiltin(name))
      builtInCallNames.add(name)

    callAst(callNode(ctx, code, name, DynamicCallUnknownFullName, DispatchTypes.STATIC_DISPATCH))
  }

  def astForMethodOnlyIdentifier(ctx: MethodOnlyIdentifierContext): Seq[Ast] = {
    if (ctx.LOCAL_VARIABLE_IDENTIFIER() != null) {
      Seq(astForCallNode(ctx, ctx.getText))
    } else if (ctx.CONSTANT_IDENTIFIER() != null) {
      Seq(astForCallNode(ctx, ctx.getText))
    } else if (ctx.keyword() != null) {
      Seq(astForCallNode(ctx, ctx.keyword().getText))
    } else {
      Seq(Ast())
    }
  }

  def astForMethodIdentifierContext(ctx: MethodIdentifierContext, code: String): Seq[Ast] = {
    // the local/const identifiers are definitely method names
    if (ctx.methodOnlyIdentifier() != null) {
      astForMethodOnlyIdentifier(ctx.methodOnlyIdentifier())
    } else if (ctx.LOCAL_VARIABLE_IDENTIFIER() != null) {
      val localVar  = ctx.LOCAL_VARIABLE_IDENTIFIER()
      val varSymbol = localVar.getSymbol
      Seq(astForCallNode(ctx, code, methodNamesWithYield.contains(varSymbol.getText)))
    } else if (ctx.CONSTANT_IDENTIFIER() != null) {
      Seq(astForCallNode(ctx, code))
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
    val methodFullName = classStack.reverse :+ name mkString pathSep

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
      astForMethodIdentifierContext(ctx.methodIdentifier(), ctx.getText)
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
    Seq(
      callAst(
        callNode(ctx, ctx.getText, ctx.getText, ctx.getText, DispatchTypes.STATIC_DISPATCH, Some(""), Some(Defines.Any))
      )
    )
  }

  def astForDefinedMethodNameContext(ctx: DefinedMethodNameContext): Seq[Ast] = {
    Option(ctx.methodName()) match
      case Some(methodNameCtx) => astForMethodNameContext(methodNameCtx)
      case None                => astForAssignmentLikeMethodIdentifierContext(ctx.assignmentLikeMethodIdentifier())
  }

  def astForSingletonObjectContext(ctx: SingletonObjectContext): Seq[Ast] = {
    if (ctx.variableIdentifier() != null) {
      Seq(astForVariableIdentifierHelper(ctx.variableIdentifier(), true))
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
    val singletonObjAst      = astForSingletonObjectContext(ctx.singletonObject())
    definedMethodNameAst ++ singletonObjAst
  }

  def astForMethodNamePartContext(ctx: MethodNamePartContext): Seq[Ast] = ctx match {
    case ctx: SimpleMethodNamePartContext    => astForSimpleMethodNamePartContext(ctx)
    case ctx: SingletonMethodNamePartContext => astForSingletonMethodNamePartContext(ctx)
    case _ =>
      logger.error(s"astForMethodNamePartContext() $filename, ${ctx.getText} All contexts mismatched.")
      Seq(Ast())
  }

  private def astForParametersContext(ctx: ParametersContext): Seq[Ast] = {
    if (ctx == null) return Seq()

    // the parameterTupleList holds the parameter terminal node and is the parameter a variadic parameter
    val parameterTupleList = ctx.parameter().asScala.map {
      case procCtx if procCtx.procParameter() != null =>
        (Option(procCtx.procParameter().LOCAL_VARIABLE_IDENTIFIER()), false)
      case optCtx if optCtx.optionalParameter() != null =>
        (Option(optCtx.optionalParameter().LOCAL_VARIABLE_IDENTIFIER()), false)
      case manCtx if manCtx.mandatoryParameter() != null =>
        (Option(manCtx.mandatoryParameter().LOCAL_VARIABLE_IDENTIFIER()), false)
      case arrCtx if arrCtx.arrayParameter() != null =>
        (Option(arrCtx.arrayParameter().LOCAL_VARIABLE_IDENTIFIER()), arrCtx.arrayParameter().STAR() != null)
      case keywordCtx if keywordCtx.keywordParameter() != null =>
        (Option(keywordCtx.keywordParameter().LOCAL_VARIABLE_IDENTIFIER()), false)
      case _ => (None, false)
    }

    parameterTupleList.zipWithIndex.map { case (paraTuple, paraIndex) =>
      paraTuple match
        case (Some(paraValue), isVariadic) =>
          val varSymbol = paraValue.getSymbol
          createIdentifierWithScope(ctx, varSymbol.getText, varSymbol.getText, Defines.Any, Seq[String](Defines.Any))
          Ast(
            createMethodParameterIn(
              varSymbol.getText,
              lineNumber = Some(varSymbol.getLine),
              colNumber = Some(varSymbol.getCharPositionInLine),
              order = paraIndex + 1,
              index = paraIndex + 1
            ).isVariadic(isVariadic)
          )
        case _ =>
          Ast(
            createMethodParameterIn(
              getUnusedVariableNames(usedVariableNames, Defines.TempParameter),
              order = paraIndex + 1,
              index = paraIndex + 1
            )
          )
    }.toList
  }

  // TODO: Rewrite for simplicity and take into account more than parameter names.
  def astForMethodParameterPartContext(ctx: MethodParameterPartContext): Seq[Ast] = {
    if (ctx == null || ctx.parameters() == null) return Seq()
    astForParametersContext(ctx.parameters())
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

    asts.addAll(astForCompoundStatement(ctx.thenClause().compoundStatement(), false))
    blockAst(blockNode(ctx), asts.toList)
  }

  /** Handles body statements differently from [[astForBodyStatementContext]] by noting that method definitions should
    * be on the root level and assignments where the LHS starts with @@ should be treated as fields.
    */
  def astForClassBody(ctx: BodyStatementContext): Seq[Ast] = {
    val rootStatements =
      Option(ctx).map(_.compoundStatement()).map(_.statements()).map(astForStatements(_)).getOrElse(Seq())
    retrieveAndGenerateClassChildren(ctx, rootStatements)
  }

  /** As class bodies are not treated much differently to other procedure bodies, we need to retrieve certain components
    * that would result in the creation of interprocedural constructs.
    *
    * TODO: This is pretty hacky and the parser could benefit from more specific tokens
    */
  private def retrieveAndGenerateClassChildren(classCtx: BodyStatementContext, rootStatements: Seq[Ast]): Seq[Ast] = {
    val (memberLikeStmts, blockStmts) = rootStatements
      .flatMap { ast =>
        ast.root match
          case Some(x: NewMethod)                                 => Seq(ast)
          case Some(x: NewCall) if x.name == Operators.assignment => Seq(ast) ++ membersFromStatementAsts(ast)
          case _                                                  => Seq(ast)
      }
      .partition(_.root match
        case Some(_: NewMethod) => true
        case Some(_: NewMember) => true
        case _                  => false
      )

    val methodStmts = memberLikeStmts.filter(_.root.exists(_.isInstanceOf[NewMethod]))
    val memberNodes = memberLikeStmts.flatMap(_.root).collect { case m: NewMember => m }

    val uniqueMemberReferences =
      (memberNodes ++ fieldReferences.getOrElse(classStack.top, Set.empty).groupBy(_.getText).map { case (code, ctxs) =>
        NewMember()
          .name(code.replaceAll("@", ""))
          .code(code)
          .typeFullName(Defines.Any)
      }).toList.distinctBy(_.name).map { m =>
        val modifierType = m.name match
          case x if x.startsWith("@@") => ModifierTypes.STATIC
          case _                       => ModifierTypes.VIRTUAL
        val modifierAst = Ast(NewModifier().modifierType(modifierType))
        Ast(m).withChild(modifierAst)
      }

    // Create class initialization method to host all field initializers
    val classInitMethodAst = if (blockStmts.nonEmpty) {
      val classInitFullName = (classStack.reverse :+ XDefines.StaticInitMethodName).mkString(pathSep)
      val classInitMethod = methodNode(
        classCtx,
        XDefines.StaticInitMethodName,
        XDefines.StaticInitMethodName,
        classInitFullName,
        None,
        filename,
        Option(NodeTypes.TYPE_DECL),
        Option(classStack.reverse.mkString(pathSep))
      )
      val classInitBody = blockAst(blockNode(classCtx), blockStmts.toList)
      Seq(methodAst(classInitMethod, Seq.empty, classInitBody, methodReturnNode(classCtx, Defines.Any)))
    } else {
      Seq.empty
    }

    classInitMethodAst ++ uniqueMemberReferences ++ methodStmts
  }

  def astForBodyStatementContext(ctx: BodyStatementContext, isMethodBody: Boolean = false): Seq[Ast] = {
    if (ctx.rescueClause().size > 0) {
      val compoundStatementAsts = astForCompoundStatement(ctx.compoundStatement())
      val elseClauseAsts = Option(ctx.elseClause()) match
        case Some(ctx) => astForCompoundStatement(ctx.compoundStatement(), false)
        case None      => Seq()

      /*
       * TODO Conversion of last statement to return AST is needed here
       * This can be done after the data flow engine issue with return from a try block is fixed
       */
      val tryBodyAsts = compoundStatementAsts ++ elseClauseAsts
      val tryBodyAst  = blockAst(blockNode(ctx), tryBodyAsts.toList)

      val finallyAst = Option(ctx.ensureClause()) match
        case Some(ctx) => astForCompoundStatement(ctx.compoundStatement()).headOption
        case None      => None

      val catchAsts = ctx
        .rescueClause()
        .asScala
        .map(astForRescueClauseContext)
        .toSeq

      val tryNode = NewControlStructure()
        .controlStructureType(ControlStructureTypes.TRY)
        .code("try")
        .lineNumber(line(ctx))
        .columnNumber(column(ctx))

      Seq(tryCatchAst(tryNode, tryBodyAst, catchAsts, finallyAst))
    } else {
      astForCompoundStatement(ctx.compoundStatement(), isMethodBody)
    }
  }

  def astForMethodDefinitionContext(ctx: MethodDefinitionContext): Seq[Ast] = {
    scope.pushNewScope(())
    val astMethodName = Option(ctx.methodNamePart()) match
      case Some(ctxMethodNamePart) =>
        astForMethodNamePartContext(ctxMethodNamePart)
      case None =>
        astForMethodIdentifierContext(ctx.methodIdentifier(), ctx.getText)
    val callNode = astMethodName.head.nodes.filter(node => node.isInstanceOf[NewCall]).head.asInstanceOf[NewCall]

    // Create thisParameter if this is an instance method
    // TODO may need to revisit to make this more robust
    val astMethodParamSeq = ctx.methodNamePart() match {
      case _: SimpleMethodNamePartContext if !classStack.top.endsWith(":program") =>
        val thisParameterNode = createMethodParameterIn(
          "this",
          typeFullName = callNode.methodFullName,
          lineNumber = callNode.lineNumber,
          colNumber = callNode.columnNumber,
          index = 0,
          order = 0
        )
        Seq(Ast(thisParameterNode)) ++ astForMethodParameterPartContext(ctx.methodParameterPart())
      case _ => astForMethodParameterPartContext(ctx.methodParameterPart())
    }

    // there can be only one call node
    val astBody = Option(ctx.bodyStatement()) match {
      case Some(ctxBodyStmt) => astForBodyStatementContext(ctxBodyStmt, true)
      case None =>
        val expAst = astForExpressionContext(ctx.expression())
        Seq(lastStmtAsReturn(ctx.expression().getText, expAst.head))
    }
    scope.popScope()

    /*
     * The method astForMethodNamePartContext() returns a call node in the AST.
     * This is because it has been called from several places some of which need a call node.
     * We will use fields from the call node to construct the method node. Post that,
     * we will discard the call node since it is of no further use to us
     *
     * TODO Dave: ^ This seems like it needs a re-design, it is confusing
     */

    val methodFullName = classStack.reverse :+ callNode.name mkString pathSep
    val methodNode = NewMethod()
      .code(ctx.getText)
      .name(callNode.name)
      .fullName(methodFullName)
      .columnNumber(callNode.columnNumber)
      .lineNumber(callNode.lineNumber)
      .filename(filename)

    Option(ctx.END()) match
      case Some(value) => methodNode.lineNumberEnd(value.getSymbol.getLine)
      case None        =>

    callNode.methodFullName(methodFullName)

    val classType = if (classStack.isEmpty) "Standalone" else classStack.top
    val classPath = classStack.reverse.toList.mkString(pathSep)
    packageContext.packageTable.addPackageMethod(packageContext.moduleName, callNode.name, classPath, classType)

    // process yield calls.
    astBody
      .flatMap(ast =>
        ast.nodes
          .filter(_.isInstanceOf[NewCall])
          .filter(_.asInstanceOf[NewCall].name == UNRESOLVED_YIELD)
      )
      .foreach(node => {
        val yieldCallNode  = node.asInstanceOf[NewCall]
        val name           = methodNode.name
        val methodFullName = classStack.reverse :+ callNode.name mkString pathSep
        yieldCallNode.name(name + YIELD_SUFFIX)
        yieldCallNode.methodFullName(methodFullName + YIELD_SUFFIX)
        methodNamesWithYield.add(methodNode.name)
        /*
         * These are calls to the yield block of this method.
         * Add this method to the list of yield blocks.
         * The add() is idempotent and so adding the same method multiple times makes no difference.
         * It just needs to be added at this place so that it gets added iff it has a yield block
         */

      })

    val methodRetNode = NewMethodReturn()
      .lineNumber(None)
      .columnNumber(None)
      .typeFullName(Defines.Any)

    val modifierNode = lastModifier match {
      case Some(modifier) => NewModifier().modifierType(modifier).code(modifier)
      case None           => NewModifier().modifierType(ModifierTypes.PUBLIC).code(ModifierTypes.PUBLIC)
    }
    /*
     * public/private/protected modifiers are in a separate statement
     * TODO find out how they should be used. Need to do this iff it adds any value
     */

    methodNameToMethod.put(methodNode.name, methodNode)
    val blockNode = NewBlock().typeFullName(Defines.Any)

    /* Before creating ast, we traverse the method params and identifiers and link them*/
    val identifiers =
      astBody.flatMap(ast => ast.nodes.filter(_.isInstanceOf[NewIdentifier])).asInstanceOf[Seq[NewIdentifier]]

    astMethodParamSeq
      .flatMap(ast =>
        ast.nodes
          .filter(_.isInstanceOf[NewMethodParameterIn])
          .asInstanceOf[Seq[NewMethodParameterIn]]
      )
      .foreach { paramNode =>
        val linkIdentifiers = identifiers.filter(_.name == paramNode.name)
        linkIdentifiers.foreach { identifier =>
          diffGraph.addEdge(identifier, paramNode, EdgeTypes.REF)
        }
      }

    Seq(
      methodAst(
        methodNode,
        astMethodParamSeq,
        blockAst(blockNode, astBody.toList),
        methodRetNode,
        Seq[NewModifier](modifierNode)
      )
    )
  }

  def getPackedRHS(astsToConcat: Seq[Ast]) = {
    val callNode = NewCall()
      .name(Operators.arrayInitializer)
      .methodFullName(Operators.arrayInitializer)
      .signature(Operators.arrayInitializer)
      .typeFullName(Defines.Any)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
    Seq(callAst(callNode, astsToConcat))
  }

  def astForMultipleAssignmentExpressionContext(ctx: MultipleAssignmentExpressionContext): Seq[Ast] = {
    val rhsAsts      = astForMultipleRightHandSideContext(ctx.multipleRightHandSide())
    val lhsAsts      = astForMultipleLeftHandSideContext(ctx.multipleLeftHandSide())
    val operatorName = getOperatorName(ctx.EQ().getSymbol)

    if (lhsAsts.size == 1 && rhsAsts.size > 1) {
      /*
       * This is multiple RHS packed into a single LHS. That is, packing left hand side.
       * This is as good as multiple RHS packed into an array and put into a single LHS
       */
      val callNode = NewCall()
        .name(operatorName)
        .code(ctx.getText)
        .methodFullName(operatorName)
        .dispatchType(DispatchTypes.STATIC_DISPATCH)
        .typeFullName(Defines.Any)
        .lineNumber(ctx.EQ().getSymbol().getLine())
        .columnNumber(ctx.EQ().getSymbol().getCharPositionInLine())

      val packedRHS = getPackedRHS(rhsAsts)
      Seq(callAst(callNode, lhsAsts ++ packedRHS))
    } else {
      /*
       * This is multiple LHS and multiple RHS
       *Since we have multiple LHS and RHS elements here, we will now create synthetic assignment
       * call nodes to model how ruby assigns values from RHS elements to LHS elements. We create
       * tuples for each assignment and then pass them to the assignment calls nodes
       */
      val assigns = lhsAsts.zip(rhsAsts)
      assigns.map { argPair =>
        val lhsCode = argPair._1.nodes.headOption match {
          case Some(id: NewIdentifier) => id.code
          case Some(lit: NewLiteral)   => lit.code
          case _                       => ""
        }

        val rhsCode = argPair._2.nodes.headOption match {
          case Some(id: NewIdentifier) => id.code
          case Some(lit: NewLiteral)   => lit.code
          case _                       => ""
        }

        val syntheticCallNode = NewCall()
          .name(operatorName)
          .code(lhsCode + " = " + rhsCode)
          .methodFullName(operatorName)
          .dispatchType(DispatchTypes.STATIC_DISPATCH)
          .typeFullName(Defines.Any)
          .lineNumber(ctx.EQ().getSymbol().getLine())
          .columnNumber(ctx.EQ().getSymbol().getCharPositionInLine())

        callAst(syntheticCallNode, Seq(argPair._1, argPair._2))
      }
    }
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

  def astForCommandWithDoBlockContext(ctx: CommandWithDoBlockContext): Seq[Ast] = ctx match {
    case ctx: ArgsAndDoBlockCommandWithDoBlockContext =>
      val argsAsts   = astForArguments(ctx.argumentsWithoutParentheses().arguments())
      val doBlockAst = Seq(astForDoBlock(ctx.doBlock()))
      argsAsts ++ doBlockAst
    case ctx: RubyParser.ArgsAndDoBlockAndMethodIdCommandWithDoBlockContext =>
      val argsAsts     = astForArguments(ctx.argumentsWithoutParentheses().arguments())
      val doBlockAsts  = Seq(astForDoBlock(ctx.doBlock()))
      val methodIdAsts = astForMethodIdentifierContext(ctx.methodIdentifier(), ctx.getText)
      methodIdAsts ++ argsAsts ++ doBlockAsts
    case ctx: RubyParser.PrimaryMethodArgsDoBlockCommandWithDoBlockContext =>
      val argsAsts       = astForArguments(ctx.argumentsWithoutParentheses().arguments())
      val doBlockAsts    = Seq(astForDoBlock(ctx.doBlock()))
      val methodNameAsts = astForMethodNameContext(ctx.methodName())
      val primaryAsts    = astForPrimaryContext(ctx.primary())
      primaryAsts ++ methodNameAsts ++ argsAsts ++ doBlockAsts
    case _ =>
      logger.error(s"astForCommandWithDoBlockContext() $filename, ${ctx.getText} All contexts mismatched.")
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
    case ctx: ArgsOnlyArgumentsWithParenthesesContext  => astForArguments(ctx.arguments())
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
      logger.error(s"astForArgumentsWithParenthesesContext() $filename, ${ctx.getText} All contexts mismatched.")
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

  def astForBlockMethod(
    ctxStmt: StatementsContext,
    ctxParam: Option[BlockParameterContext],
    blockMethodName: String,
    lineStart: Int,
    lineEnd: Int,
    colStart: Int,
    colEnd: Int
  ): Seq[Ast] = {
    /*
     * Model a block as a method
     */

    val astMethodParam = ctxParam.map(astForBlockParameterContext).getOrElse(Seq())
    scope.pushNewScope(())
    val astBody = astForStatements(ctxStmt, true)
    scope.popScope()

    val methodFullName = classStack.reverse :+ blockMethodName mkString pathSep
    val methodNode = NewMethod()
      .code(ctxStmt.getText)
      .name(blockMethodName)
      .fullName(methodFullName)
      .filename(filename)
      .lineNumber(lineStart)
      .lineNumberEnd(lineEnd)
      .columnNumber(colStart)
      .columnNumberEnd(colEnd)

    val methodRetNode = NewMethodReturn()
      .typeFullName(Defines.Any)

    val publicModifier = NewModifier().modifierType(ModifierTypes.PUBLIC)
    val paramSeq = astMethodParam.headOption match {
      case Some(value) =>
        value.nodes.map {
          /* In majority of cases, node will be an identifier */
          case identifierNode: NewIdentifier =>
            val param = NewMethodParameterIn()
              .name(identifierNode.name)
              .code(identifierNode.code)
              .lineNumber(identifierNode.lineNumber)
              .columnNumber(identifierNode.columnNumber)
            Ast(param)
          case callNode: NewCall =>
            /* TODO: Occasionally, we might encounter a _ call in cases like "do |_, x|" where we should handle this?
             * But for now, we just return an empty AST. Keeping this match explicitly here so we come back */
            Ast()
          case _ =>
            Ast()
        }.toSeq
      case None => Seq()
    }

    val blockNode = NewBlock().typeFullName(Defines.Any)
    val methAst = methodAst(
      methodNode,
      paramSeq,
      blockAst(blockNode, astBody.toList),
      methodRetNode,
      Seq[NewModifier](publicModifier)
    )
    Seq(methAst)
  }

  def astForAssociationContext(ctx: AssociationContext): Seq[Ast] = {
    val terminalNode = Option(ctx.COLON()) match
      case Some(value) => value
      case None        => ctx.EQGT()
    val operatorText = getOperatorName(terminalNode.getSymbol)

    val callArgs =
      Option(ctx.keyword()) match {
        case Some(ctxKeyword) =>
          val expr1Ast  = astForCallNode(ctx, ctxKeyword.getText)
          val expr2Asts = astForExpressionContext(ctx.expression().get(0))
          Seq(expr1Ast) ++ expr2Asts
        case None =>
          var expr2Asts = Seq(Ast())
          val expr1Asts = astForExpressionContext(ctx.expression().get(0))
          if (ctx.expression().size() > 1 && ctx.expression().get(1) != null) {
            expr2Asts = astForExpressionContext(ctx.expression().get(1))
          }
          expr1Asts ++ expr2Asts
      }

    val callNode = NewCall()
      .name(operatorText)
      .code(ctx.getText)
      .methodFullName(operatorText)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .typeFullName(Defines.Any)
      .lineNumber(terminalNode.getSymbol.getLine)
      .columnNumber(terminalNode.getSymbol.getCharPositionInLine)
    Seq(callAst(callNode, callArgs))
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

}
