package io.joern.rubysrc2cpg.deprecated.astcreation

import io.joern.rubysrc2cpg.deprecated.parser.DeprecatedRubyParser.*
import io.joern.rubysrc2cpg.deprecated.passes.Defines
import io.joern.rubysrc2cpg.deprecated.passes.Defines.*
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.{AstNodeNew, NewCall, NewIdentifier}
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, DispatchTypes, ModifierTypes, Operators}
import org.antlr.v4.runtime.ParserRuleContext
import org.slf4j.LoggerFactory

import scala.collection.immutable.Set
import scala.jdk.CollectionConverters.CollectionHasAsScala

trait AstForExpressionsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  private val logger                         = LoggerFactory.getLogger(this.getClass)
  protected var lastModifier: Option[String] = None

  protected def astForPowerExpression(ctx: PowerExpressionContext): Ast =
    astForBinaryOperatorExpression(ctx, Operators.exponentiation, ctx.expression().asScala)

  protected def astForOrExpression(ctx: OperatorOrExpressionContext): Ast =
    astForBinaryOperatorExpression(ctx, Operators.or, ctx.expression().asScala)

  protected def astForAndExpression(ctx: OperatorAndExpressionContext): Ast =
    astForBinaryOperatorExpression(ctx, Operators.and, ctx.expression().asScala)

  protected def astForUnaryExpression(ctx: UnaryExpressionContext): Ast = ctx.op.getType match {
    case TILDE => astForBinaryOperatorExpression(ctx, Operators.not, Seq(ctx.expression()))
    case PLUS  => astForBinaryOperatorExpression(ctx, Operators.plus, Seq(ctx.expression()))
    case EMARK => astForBinaryOperatorExpression(ctx, Operators.not, Seq(ctx.expression()))
  }

  protected def astForUnaryMinusExpression(ctx: UnaryMinusExpressionContext): Ast =
    astForBinaryOperatorExpression(ctx, Operators.minus, Seq(ctx.expression()))

  protected def astForAdditiveExpression(ctx: AdditiveExpressionContext): Ast = ctx.op.getType match {
    case PLUS  => astForBinaryOperatorExpression(ctx, Operators.addition, ctx.expression().asScala)
    case MINUS => astForBinaryOperatorExpression(ctx, Operators.subtraction, ctx.expression().asScala)
  }

  protected def astForMultiplicativeExpression(ctx: MultiplicativeExpressionContext): Ast = ctx.op.getType match {
    case STAR    => astForMultiplicativeStarExpression(ctx)
    case SLASH   => astForMultiplicativeSlashExpression(ctx)
    case PERCENT => astForMultiplicativePercentExpression(ctx)
  }

  protected def astForMultiplicativeStarExpression(ctx: MultiplicativeExpressionContext): Ast =
    astForBinaryOperatorExpression(ctx, Operators.multiplication, ctx.expression().asScala)

  protected def astForMultiplicativeSlashExpression(ctx: MultiplicativeExpressionContext): Ast =
    astForBinaryOperatorExpression(ctx, Operators.division, ctx.expression().asScala)

  protected def astForMultiplicativePercentExpression(ctx: MultiplicativeExpressionContext): Ast =
    astForBinaryOperatorExpression(ctx, Operators.modulo, ctx.expression().asScala)

  protected def astForEqualityExpression(ctx: EqualityExpressionContext): Ast = ctx.op.getType match {
    case LTEQGT     => astForBinaryOperatorExpression(ctx, Operators.compare, ctx.expression().asScala)
    case EQ2        => astForBinaryOperatorExpression(ctx, Operators.equals, ctx.expression().asScala)
    case EQ3        => astForBinaryOperatorExpression(ctx, Operators.is, ctx.expression().asScala)
    case EMARKEQ    => astForBinaryOperatorExpression(ctx, Operators.notEquals, ctx.expression().asScala)
    case EQTILDE    => astForBinaryOperatorExpression(ctx, RubyOperators.patternMatch, ctx.expression().asScala)
    case EMARKTILDE => astForBinaryOperatorExpression(ctx, RubyOperators.notPatternMatch, ctx.expression().asScala)
  }

  protected def astForRelationalExpression(ctx: RelationalExpressionContext): Ast = ctx.op.getType match {
    case GT   => astForBinaryOperatorExpression(ctx, Operators.greaterThan, ctx.expression().asScala)
    case GTEQ => astForBinaryOperatorExpression(ctx, Operators.greaterEqualsThan, ctx.expression().asScala)
    case LT   => astForBinaryOperatorExpression(ctx, Operators.lessThan, ctx.expression().asScala)
    case LTEQ => astForBinaryOperatorExpression(ctx, Operators.lessEqualsThan, ctx.expression().asScala)
  }

  protected def astForBitwiseOrExpression(ctx: BitwiseOrExpressionContext): Ast = ctx.op.getType match {
    case BAR   => astForBinaryOperatorExpression(ctx, Operators.logicalOr, ctx.expression().asScala)
    case CARET => astForBinaryOperatorExpression(ctx, Operators.logicalOr, ctx.expression().asScala)
  }

  protected def astForBitwiseAndExpression(ctx: BitwiseAndExpressionContext): Ast =
    astForBinaryOperatorExpression(ctx, Operators.logicalAnd, ctx.expression().asScala)

  protected def astForBitwiseShiftExpression(ctx: BitwiseShiftExpressionContext): Ast = ctx.op.getType match {
    case LT2 => astForBinaryOperatorExpression(ctx, Operators.shiftLeft, ctx.expression().asScala)
    case GT2 => astForBinaryOperatorExpression(ctx, Operators.logicalShiftRight, ctx.expression().asScala)
  }

  private def astForBinaryOperatorExpression(
    ctx: ParserRuleContext,
    name: String,
    arguments: Iterable[ExpressionContext]
  ): Ast = {
    val argsAst = arguments.flatMap(astForExpressionContext)
    val call    = callNode(ctx, code(ctx), name, name, DispatchTypes.STATIC_DISPATCH)
    callAst(call, argsAst.toList)
  }

  protected def astForIsDefinedExpression(ctx: IsDefinedExpressionContext): Ast =
    astForBinaryOperatorExpression(ctx, RubyOperators.defined, Seq(ctx.expression()))

  // TODO: Maybe merge (in DeprecatedRubyParser.g4) isDefinedExpression with isDefinedPrimaryExpression?
  protected def astForIsDefinedPrimaryExpression(ctx: IsDefinedPrimaryContext): Ast = {
    val argsAst = astForExpressionOrCommand(ctx.expressionOrCommand())
    val call    = callNode(ctx, code(ctx), RubyOperators.defined, RubyOperators.defined, DispatchTypes.STATIC_DISPATCH)
    callAst(call, argsAst.toList)
  }

  protected def astForLiteralPrimaryExpression(ctx: LiteralPrimaryContext): Seq[Ast] = ctx.literal() match {
    case ctx: NumericLiteralLiteralContext    => Seq(astForNumericLiteral(ctx.numericLiteral()))
    case ctx: SymbolLiteralContext            => astForSymbol(ctx.symbol())
    case ctx: RegularExpressionLiteralContext => Seq(astForRegularExpressionLiteral(ctx))
    case ctx: HereDocLiteralContext           => Seq(astForHereDocLiteral(ctx))
    case _ =>
      logger.error(s"astForLiteralPrimaryExpression() $relativeFilename, ${text(ctx)} All contexts mismatched.")
      Seq()
  }

  private def astForSymbol(ctx: SymbolContext): Seq[Ast] = {
    if (
      ctx.stringExpression() != null && ctx.stringExpression().children.get(0).isInstanceOf[StringInterpolationContext]
    ) {
      val node = callNode(
        ctx,
        code(ctx),
        RubyOperators.formattedString,
        RubyOperators.formattedString,
        DispatchTypes.STATIC_DISPATCH,
        None,
        Option(Defines.Any)
      )
      astForStringExpression(ctx.stringExpression()) ++ Seq(Ast(node))
    } else {
      Seq(astForSymbolLiteral(ctx))
    }
  }

  protected def astForMultipleRightHandSideContext(ctx: MultipleRightHandSideContext): Seq[Ast] =
    if (ctx == null) {
      Seq.empty
    } else {
      val expCmd = ctx.expressionOrCommands()
      val exprAsts = Option(expCmd) match
        case Some(expCmd) =>
          expCmd.expressionOrCommand().asScala.flatMap(astForExpressionOrCommand).toSeq
        case None =>
          Seq.empty

      if (ctx.splattingArgument != null) {
        val splattingAsts = astForExpressionOrCommand(ctx.splattingArgument.expressionOrCommand)
        exprAsts ++ splattingAsts
      } else {
        exprAsts
      }
    }

  protected def astForSingleLeftHandSideContext(ctx: SingleLeftHandSideContext): Seq[Ast] = ctx match {
    case ctx: VariableIdentifierOnlySingleLeftHandSideContext =>
      Seq(astForVariableIdentifierHelper(ctx.variableIdentifier, true))
    case ctx: PrimaryInsideBracketsSingleLeftHandSideContext =>
      val primaryAsts     = astForPrimaryContext(ctx.primary)
      val argsAsts        = astForArguments(ctx.arguments)
      val indexAccessCall = createOpCall(ctx.LBRACK, Operators.indexAccess, code(ctx))
      Seq(callAst(indexAccessCall, primaryAsts ++ argsAsts))
    case ctx: XdotySingleLeftHandSideContext =>
      // TODO handle obj.foo=arg being interpreted as obj.foo(arg) here.
      val xAsts = astForPrimaryContext(ctx.primary)

      Seq(ctx.LOCAL_VARIABLE_IDENTIFIER, ctx.CONSTANT_IDENTIFIER)
        .flatMap(Option(_))
        .headOption match
        case Some(localVar) =>
          val name = localVar.getSymbol.getText
          val node = createIdentifierWithScope(ctx, name, name, Defines.Any, List(Defines.Any), true)
          val yAst = Ast(node)

          val callNode = createOpCall(localVar, Operators.fieldAccess, code(ctx))
          Seq(callAst(callNode, xAsts ++ Seq(yAst)))
        case None =>
          Seq.empty
    case ctx: ScopedConstantAccessSingleLeftHandSideContext =>
      val localVar  = ctx.CONSTANT_IDENTIFIER
      val varSymbol = localVar.getSymbol
      val node =
        createIdentifierWithScope(ctx, varSymbol.getText, varSymbol.getText, Defines.Any, List(Defines.Any), true)
      Seq(Ast(node))
    case _ =>
      logger.error(s"astForSingleLeftHandSideContext() $relativeFilename, ${text(ctx)} All contexts mismatched.")
      Seq.empty
  }

  protected def astForSingleAssignmentExpressionContext(ctx: SingleAssignmentExpressionContext): Seq[Ast] = {
    val rightAst = astForMultipleRightHandSideContext(ctx.multipleRightHandSide)
    val leftAst  = astForSingleLeftHandSideContext(ctx.singleLeftHandSide)

    val operatorName = getOperatorName(ctx.op)
    val opCallNode =
      callNode(ctx, code(ctx), operatorName, operatorName, DispatchTypes.STATIC_DISPATCH, None, Option(Defines.Any))
        .lineNumber(ctx.op.getLine)
        .columnNumber(ctx.op.getCharPositionInLine)
    if (leftAst.size == 1 && rightAst.size > 1) {
      /*
       * This is multiple RHS packed into a single LHS. That is, packing left hand side.
       * This is as good as multiple RHS packed into an array and put into a single LHS
       */
      val packedRHS = getPackedRHS(rightAst, wrapInBrackets = true)
      Seq(callAst(opCallNode, leftAst ++ packedRHS))
    } else {
      Seq(callAst(opCallNode, leftAst ++ rightAst))
    }
  }

  protected def astForMultipleAssignmentExpressionContext(ctx: MultipleAssignmentExpressionContext): Seq[Ast] = {
    val rhsAsts      = astForMultipleRightHandSideContext(ctx.multipleRightHandSide())
    val lhsAsts      = astForMultipleLeftHandSideContext(ctx.multipleLeftHandSide())
    val operatorName = getOperatorName(ctx.EQ.getSymbol)

    /*
     * This is multiple LHS and multiple RHS
     *Since we have multiple LHS and RHS elements here, we will now create synthetic assignment
     * call nodes to model how ruby assigns values from RHS elements to LHS elements. We create
     * tuples for each assignment and then pass them to the assignment calls nodes
     */
    val assigns =
      if (lhsAsts.size < rhsAsts.size) {
        /* The rightmost AST in the LHS is a packed variable.
         * Pack the extra ASTs and the rightmost AST in the RHS in one array like the if() part
         */
        val diff        = rhsAsts.size - lhsAsts.size
        val packedRHS   = getPackedRHS(rhsAsts.takeRight(diff + 1)).headOption.to(Seq)
        val alignedAsts = lhsAsts.take(lhsAsts.size - 1) zip rhsAsts.take(lhsAsts.size - 1)
        val packedAsts  = lhsAsts.takeRight(1) zip packedRHS
        alignedAsts ++ packedAsts
      } else {
        lhsAsts.zip(rhsAsts)
      }

    assigns.map { case (lhsAst, rhsAst) =>
      val lhsCode           = lhsAst.nodes.collectFirst { case x: AstNodeNew => x.code }.getOrElse("")
      val rhsCode           = rhsAst.nodes.collectFirst { case x: AstNodeNew => x.code }.getOrElse("")
      val code              = s"$lhsCode = $rhsCode"
      val syntheticCallNode = createOpCall(ctx.EQ, operatorName, code)

      callAst(syntheticCallNode, Seq(lhsAst, rhsAst))
    }
  }

  protected def astForIndexingExpressionPrimaryContext(ctx: IndexingExpressionPrimaryContext): Seq[Ast] = {
    val lhsExpressionAst = astForPrimaryContext(ctx.primary())
    val rhsExpressionAst = Option(ctx.indexingArguments).map(astForIndexingArgumentsContext).getOrElse(Seq())

    val operator = lhsExpressionAst.flatMap(_.nodes).collectFirst { case x: NewIdentifier => x } match
      case Some(node) if node.name == "Array" => Operators.arrayInitializer
      case _                                  => Operators.indexAccess

    val callNode = createOpCall(ctx.LBRACK, operator, code(ctx))
    Seq(callAst(callNode, lhsExpressionAst ++ rhsExpressionAst))

  }

  private def getPackedRHS(astsToConcat: Seq[Ast], wrapInBrackets: Boolean = false) = {
    val code = astsToConcat
      .flatMap(_.nodes)
      .collect { case x: AstNodeNew => x.code }
      .mkString(", ")

    val callNode = NewCall()
      .name(Operators.arrayInitializer)
      .methodFullName(Operators.arrayInitializer)
      .typeFullName(Defines.Any)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .code(if (wrapInBrackets) s"[$code]" else code)
    Seq(callAst(callNode, astsToConcat))
  }

  def astForStringInterpolationContext(ctx: InterpolatedStringExpressionContext): Seq[Ast] = {
    val varAsts = ctx.stringInterpolation.interpolatedStringSequence.asScala
      .flatMap(inter =>
        Seq(
          Ast(
            callNode(
              ctx,
              code(inter),
              RubyOperators.formattedValue,
              RubyOperators.formattedValue,
              DispatchTypes.STATIC_DISPATCH,
              None,
              Option(Defines.Any)
            )
          )
        ) ++
          astForStatements(inter.compoundStatement.statements, false, false)
      )
      .toSeq

    val literalAsts = ctx
      .stringInterpolation()
      .DOUBLE_QUOTED_STRING_CHARACTER_SEQUENCE()
      .asScala
      .map(substr =>
        Ast(
          createLiteralNode(
            substr.getText,
            Defines.String,
            List(Defines.String),
            Option(substr.lineNumber),
            Option(substr.columnNumber)
          )
        )
      )
      .toSeq
    varAsts ++ literalAsts
  }

  // TODO: Return Ast instead of Seq[Ast]
  protected def astForStringExpression(ctx: StringExpressionContext): Seq[Ast] = ctx match {
    case ctx: SimpleStringExpressionContext       => Seq(astForSimpleString(ctx.simpleString))
    case ctx: InterpolatedStringExpressionContext => astForStringInterpolationContext(ctx)
    case ctx: ConcatenatedStringExpressionContext => Seq(astForConcatenatedStringExpressions(ctx))
  }

  // Regex interpolation has been modeled just as a set of statements, that suffices to track dataflows
  protected def astForRegexInterpolationPrimaryContext(ctx: RegexInterpolationContext): Seq[Ast] = {
    val varAsts = ctx
      .interpolatedRegexSequence()
      .asScala
      .flatMap(inter => {
        astForStatements(inter.compoundStatement().statements(), false, false)
      })
      .toSeq
    varAsts
  }

  protected def astForSimpleString(ctx: SimpleStringContext): Ast = ctx match {
    case ctx: SingleQuotedStringLiteralContext => astForSingleQuotedStringLiteral(ctx)
    case ctx: DoubleQuotedStringLiteralContext => astForDoubleQuotedStringLiteral(ctx)
  }

  protected def astForConcatenatedStringExpressions(ctx: ConcatenatedStringExpressionContext): Ast = {
    val stringExpressionAsts = ctx.stringExpression().asScala.flatMap(astForStringExpression)
    val callNode_ = callNode(
      ctx,
      code(ctx),
      RubyOperators.stringConcatenation,
      RubyOperators.stringConcatenation,
      DispatchTypes.STATIC_DISPATCH
    )
    callAst(callNode_, stringExpressionAsts.toSeq)
  }

  protected def astForTernaryConditionalOperator(ctx: ConditionalOperatorExpressionContext): Ast = {
    val testAst = astForExpressionContext(ctx.expression(0))
    val thenAst = astForExpressionContext(ctx.expression(1))
    val elseAst = astForExpressionContext(ctx.expression(2))
    val ifNode  = controlStructureNode(ctx, ControlStructureTypes.IF, code(ctx))
    controlStructureAst(ifNode, testAst.headOption, thenAst ++ elseAst)
  }

  def astForRangeExpressionContext(ctx: RangeExpressionContext): Seq[Ast] =
    Seq(astForBinaryOperatorExpression(ctx, Operators.range, ctx.expression().asScala))

  protected def astForSuperExpression(ctx: SuperExpressionPrimaryContext): Ast = {
    val argsAst = Option(ctx.argumentsWithParentheses()) match
      case Some(ctxArgs) => astForArgumentsWithParenthesesContext(ctxArgs)
      case None          => Seq()
    astForSuperCall(ctx, argsAst)
  }

  // TODO: Handle the optional block.
  // NOTE: `super` is quite complicated semantically speaking. We'll need
  //       to revisit how to represent them.
  protected def astForSuperCall(ctx: ParserRuleContext, arguments: Seq[Ast]): Ast = {
    val call =
      callNode(ctx, code(ctx), RubyOperators.superKeyword, RubyOperators.superKeyword, DispatchTypes.STATIC_DISPATCH)
    callAst(call, arguments.toList)
  }

  protected def astForYieldCall(ctx: ParserRuleContext, argumentsCtx: Option[ArgumentsContext]): Ast = {
    val args = argumentsCtx.map(astForArguments).getOrElse(Seq())
    val call = callNode(ctx, code(ctx), UNRESOLVED_YIELD, UNRESOLVED_YIELD, DispatchTypes.STATIC_DISPATCH)
    callAst(call, args)
  }

  protected def astForUntilExpression(ctx: UntilExpressionContext): Ast = {
    val testAst = astForExpressionOrCommand(ctx.expressionOrCommand()).headOption
    val bodyAst = astForCompoundStatement(ctx.doClause().compoundStatement())
    // TODO: testAst should be negated if it's going to be modelled as a while stmt.
    whileAst(testAst, bodyAst, Some(text(ctx)), line(ctx), column(ctx))
  }

  protected def astForForExpression(ctx: ForExpressionContext): Ast = {
    val forVarAst  = astForForVariableContext(ctx.forVariable())
    val forExprAst = astForExpressionOrCommand(ctx.expressionOrCommand())
    val forBodyAst = astForCompoundStatement(ctx.doClause().compoundStatement())
    // TODO: for X in Y is not properly modelled by while Y
    val forRootAst = whileAst(forExprAst.headOption, forBodyAst, Some(text(ctx)), line(ctx), column(ctx))
    forVarAst.headOption.map(forRootAst.withChild).getOrElse(forRootAst)
  }

  private def astForForVariableContext(ctx: ForVariableContext): Seq[Ast] = {
    if (ctx.singleLeftHandSide() != null) {
      astForSingleLeftHandSideContext(ctx.singleLeftHandSide())
    } else if (ctx.multipleLeftHandSide() != null) {
      astForMultipleLeftHandSideContext(ctx.multipleLeftHandSide())
    } else {
      Seq(Ast())
    }
  }

  protected def astForWhileExpression(ctx: WhileExpressionContext): Ast = {
    val testAst = astForExpressionOrCommand(ctx.expressionOrCommand())
    val bodyAst = astForCompoundStatement(ctx.doClause().compoundStatement())
    whileAst(testAst.headOption, bodyAst, Some(text(ctx)), line(ctx), column(ctx))
  }

  protected def astForIfExpression(ctx: IfExpressionContext): Ast = {
    val testAst   = astForExpressionOrCommand(ctx.expressionOrCommand())
    val thenAst   = astForCompoundStatement(ctx.thenClause().compoundStatement())
    val elsifAsts = Option(ctx.elsifClause).map(_.asScala).getOrElse(Seq()).map(astForElsifClause)
    val elseAst = Option(ctx.elseClause()).map(ctx => astForCompoundStatement(ctx.compoundStatement())).getOrElse(Seq())
    val ifNode  = controlStructureNode(ctx, ControlStructureTypes.IF, code(ctx))
    controlStructureAst(ifNode, testAst.headOption)
      .withChildren(thenAst)
      .withChildren(elsifAsts.toSeq)
      .withChildren(elseAst)
  }

  private def astForElsifClause(ctx: ElsifClauseContext): Ast = {
    val ifNode  = controlStructureNode(ctx, ControlStructureTypes.IF, code(ctx))
    val testAst = astForExpressionOrCommand(ctx.expressionOrCommand())
    val bodyAst = astForCompoundStatement(ctx.thenClause().compoundStatement())
    controlStructureAst(ifNode, testAst.headOption, bodyAst)
  }

  protected def astForVariableReference(ctx: VariableReferenceContext): Ast = ctx match {
    case ctx: VariableIdentifierVariableReferenceContext => astForVariableIdentifierHelper(ctx.variableIdentifier())
    case ctx: PseudoVariableIdentifierVariableReferenceContext =>
      astForPseudoVariableIdentifier(ctx.pseudoVariableIdentifier())
  }

  private def astForPseudoVariableIdentifier(ctx: PseudoVariableIdentifierContext): Ast = ctx match {
    case ctx: NilPseudoVariableIdentifierContext      => astForNilLiteral(ctx)
    case ctx: TruePseudoVariableIdentifierContext     => astForTrueLiteral(ctx)
    case ctx: FalsePseudoVariableIdentifierContext    => astForFalseLiteral(ctx)
    case ctx: SelfPseudoVariableIdentifierContext     => astForSelfPseudoIdentifier(ctx)
    case ctx: FilePseudoVariableIdentifierContext     => astForFilePseudoIdentifier(ctx)
    case ctx: LinePseudoVariableIdentifierContext     => astForLinePseudoIdentifier(ctx)
    case ctx: EncodingPseudoVariableIdentifierContext => astForEncodingPseudoIdentifier(ctx)
  }

  protected def astForVariableIdentifierHelper(
    ctx: VariableIdentifierContext,
    definitelyIdentifier: Boolean = false
  ): Ast = {
    /*
     * Preferences
     * 1. If definitelyIdentifier is SET, create a identifier node
     * 2. If an identifier with the variable name exists within the scope, create a identifier node
     * 3. If a method with the variable name exists, create a method node
     * 4. Otherwise default to identifier node creation since there is no reason (point 2) to create a call node
     */

    val variableName      = code(ctx)
    val isSelfFieldAccess = variableName.startsWith("@")
    if (isSelfFieldAccess) {
      // Very basic field detection
      fieldReferences.updateWith(classStack.top) {
        case Some(xs) => Option(xs ++ Set(ctx))
        case None     => Option(Set(ctx))
      }
      val thisNode = createThisIdentifier(ctx)
      astForFieldAccess(ctx, thisNode)
    } else if (definitelyIdentifier || scope.lookupVariable(variableName).isDefined) {
      val node = createIdentifierWithScope(ctx, variableName, variableName, Defines.Any, List(), definitelyIdentifier)
      Ast(node)
    } else if (methodNameToMethod.contains(variableName)) {
      astForCallNode(ctx, variableName)
    } else if (ModifierTypes.ALL.contains(variableName.toUpperCase)) {
      lastModifier = Option(variableName.toUpperCase)
      Ast()
    } else if (ctx.GLOBAL_VARIABLE_IDENTIFIER() != null) {
      val globalVar = ctx.GLOBAL_VARIABLE_IDENTIFIER().getText
      Ast(createIdentifierWithScope(ctx, globalVar, globalVar, Defines.String, List()))
    } else {
      val node = createIdentifierWithScope(ctx, variableName, variableName, Defines.Any, List())
      Ast(node)
    }
  }

  protected def astForUnlessExpression(ctx: UnlessExpressionContext): Ast = {
    val testAst = astForExpressionOrCommand(ctx.expressionOrCommand())
    val thenAst = astForCompoundStatement(ctx.thenClause().compoundStatement())
    val elseAst =
      Option(ctx.elseClause()).map(_.compoundStatement()).map(st => astForCompoundStatement(st)).getOrElse(Seq())
    val ifNode = controlStructureNode(ctx, ControlStructureTypes.IF, code(ctx))
    controlStructureAst(ifNode, testAst.headOption, thenAst ++ elseAst)
  }

  protected def astForQuotedStringExpression(ctx: QuotedStringExpressionContext): Seq[Ast] = ctx match
    case ctx: NonExpandedQuotedStringLiteralContext => Seq(astForNonExpandedQuotedString(ctx))
    case _ =>
      logger.error(s"Translation for ${text(ctx)} not implemented yet")
      Seq()

  private def astForNonExpandedQuotedString(ctx: NonExpandedQuotedStringLiteralContext): Ast = {
    Ast(literalNode(ctx, code(ctx), getBuiltInType(Defines.String)))
  }

  // TODO: handle interpolation
  protected def astForQuotedRegexInterpolation(ctx: QuotedRegexInterpolationContext): Seq[Ast] = {
    Seq(Ast(literalNode(ctx, code(ctx), Defines.Regexp)))
  }

}
