package io.joern.rubysrc2cpg.astcreation

import io.joern.rubysrc2cpg.parser.RubyParser.*
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.nodes.{NewFieldIdentifier, NewJumpTarget, NewLiteral, NewNode}
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, DispatchTypes, ModifierTypes, Operators}
import org.antlr.v4.runtime.ParserRuleContext

import scala.collection.immutable.Set
import scala.jdk.CollectionConverters.CollectionHasAsScala

trait AstForExpressionsCreator { this: AstCreator =>

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
    val call    = callNode(ctx, ctx.getText, name, name, DispatchTypes.STATIC_DISPATCH)
    callAst(call, argsAst.toList)
  }

  protected def astForIsDefinedExpression(ctx: IsDefinedExpressionContext): Ast =
    astForBinaryOperatorExpression(ctx, RubyOperators.defined, Seq(ctx.expression()))

  // TODO: Maybe merge (in RubyParser.g4) isDefinedExpression with isDefinedPrimaryExpression?
  protected def astForIsDefinedPrimaryExpression(ctx: IsDefinedPrimaryContext): Ast = {
    val argsAst = astForExpressionOrCommand(ctx.expressionOrCommand())
    val call = callNode(ctx, ctx.getText, RubyOperators.defined, RubyOperators.defined, DispatchTypes.STATIC_DISPATCH)
    callAst(call, argsAst.toList)
  }

  protected def astForLiteralPrimaryExpression(ctx: LiteralPrimaryContext): Ast = ctx.literal() match {
    case ctx: NumericLiteralLiteralContext    => astForNumericLiteral(ctx.numericLiteral())
    case ctx: SymbolLiteralContext            => astForSymbolLiteral(ctx.symbol())
    case ctx: RegularExpressionLiteralContext => astForRegularExpressionLiteral(ctx)
  }

  // TODO: Return Ast instead of Seq[Ast]
  protected def astForStringExpression(ctx: StringExpressionContext): Seq[Ast] = ctx match {
    case ctx: SimpleStringExpressionContext       => Seq(astForSimpleString(ctx.simpleString))
    case ctx: InterpolatedStringExpressionContext => astForStringInterpolationContext(ctx)
    case ctx: ConcatenatedStringExpressionContext => Seq(astForConcatenatedStringExpressions(ctx))
  }

  protected def astForRegexInterpolationPrimaryContext(ctx: RegexInterpolationContext): Seq[Ast] = {
    val varAsts = ctx
      .interpolatedRegexSequence()
      .asScala
      .flatMap(inter => {
        astForStatements(inter.compoundStatement().statements(), false, false)
      })
      .toSeq
    println(varAsts)
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
      ctx.getText,
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
    val ifNode  = controlStructureNode(ctx, ControlStructureTypes.IF, ctx.getText)
    controlStructureAst(ifNode, testAst.headOption, thenAst ++ elseAst)
  }

  def astForRangeExpressionContext(ctx: RangeExpressionContext): Seq[Ast] =
    Seq(astForBinaryOperatorExpression(ctx, Operators.range, ctx.expression().asScala))

  protected def astForSuperExpression(ctx: SuperExpressionPrimaryContext): Ast =
    astForSuperCall(ctx, astForArgumentsWithParenthesesContext(ctx.argumentsWithParentheses))

  // TODO: Handle the optional block.
  // NOTE: `super` is quite complicated semantically speaking. We'll need
  //       to revisit how to represent them.
  protected def astForSuperCall(ctx: ParserRuleContext, arguments: Seq[Ast]): Ast = {
    val call =
      callNode(ctx, ctx.getText, RubyOperators.superKeyword, RubyOperators.superKeyword, DispatchTypes.STATIC_DISPATCH)
    callAst(call, arguments.toList)
  }

  protected def astForYieldCall(ctx: ParserRuleContext, argumentsCtx: Option[ArgumentsContext]): Ast = {
    val args = argumentsCtx.map(astForArguments).getOrElse(Seq())
    val call = callNode(ctx, ctx.getText, UNRESOLVED_YIELD, UNRESOLVED_YIELD, DispatchTypes.STATIC_DISPATCH)
    callAst(call, args)
  }

  protected def astForUntilExpression(ctx: UntilExpressionContext): Ast = {
    val testAst = astForExpressionOrCommand(ctx.expressionOrCommand()).headOption
    val bodyAst = astForCompoundStatement(ctx.doClause().compoundStatement())
    // TODO: testAst should be negated if it's going to be modelled as a while stmt.
    whileAst(testAst, bodyAst, Some(ctx.getText), line(ctx), column(ctx))
  }

  protected def astForForExpression(ctx: ForExpressionContext): Ast = {
    val forVarAst  = astForForVariableContext(ctx.forVariable())
    val forExprAst = astForExpressionOrCommand(ctx.expressionOrCommand())
    val forBodyAst = astForCompoundStatement(ctx.doClause().compoundStatement())
    // TODO: for X in Y is not properly modelled by while Y
    val forRootAst = whileAst(forExprAst.headOption, forBodyAst, Some(ctx.getText), line(ctx), column(ctx))
    forVarAst.headOption.map(forRootAst.withChild).getOrElse(forRootAst)
  }

  protected def astForWhileExpression(ctx: WhileExpressionContext): Ast = {
    val testAst = astForExpressionOrCommand(ctx.expressionOrCommand())
    val bodyAst = astForCompoundStatement(ctx.doClause().compoundStatement())
    whileAst(testAst.headOption, bodyAst, Some(ctx.getText), line(ctx), column(ctx))
  }

  protected def astForIfExpression(ctx: IfExpressionContext): Ast = {
    val testAst   = astForExpressionOrCommand(ctx.expressionOrCommand())
    val thenAst   = astForCompoundStatement(ctx.thenClause().compoundStatement())
    val elsifAsts = Option(ctx.elsifClause).map(_.asScala).getOrElse(Seq()).map(astForElsifClause)
    val elseAst = Option(ctx.elseClause()).map(ctx => astForCompoundStatement(ctx.compoundStatement())).getOrElse(Seq())
    val ifNode  = controlStructureNode(ctx, ControlStructureTypes.IF, ctx.getText)
    controlStructureAst(ifNode, testAst.headOption)
      .withChildren(thenAst)
      .withChildren(elsifAsts.toSeq)
      .withChildren(elseAst)
  }

  private def astForElsifClause(ctx: ElsifClauseContext): Ast = {
    val ifNode  = controlStructureNode(ctx, ControlStructureTypes.IF, ctx.getText)
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

    val variableName      = ctx.getText
    val isSelfFieldAccess = variableName.startsWith("@")
    if (isSelfFieldAccess) {
      // Very basic field detection
      fieldReferences.updateWith(classStack.top) {
        case Some(xs) => Option(xs ++ Set(ctx))
        case None     => Option(Set(ctx))
      }
      val thisNode = createIdentifierWithScope(ctx, "this", "this", Defines.Any, List.empty)
      astForFieldAccess(ctx, thisNode)
    } else if (definitelyIdentifier || scope.lookupVariable(variableName).isDefined) {
      val node = createIdentifierWithScope(ctx, variableName, variableName, Defines.Any, List())
      Ast(node)
    } else if (methodNames.contains(variableName)) {
      astForCallNode(ctx, variableName)
    } else if (ModifierTypes.ALL.contains(variableName.toUpperCase)) {
      lastModifier = Option(variableName.toUpperCase)
      Ast()
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
    val ifNode = controlStructureNode(ctx, ControlStructureTypes.IF, ctx.getText)
    controlStructureAst(ifNode, testAst.headOption, thenAst ++ elseAst)
  }

  protected def astForFieldAccess(ctx: ParserRuleContext, baseNode: NewNode): Ast = {
    val fieldAccess =
      callNode(ctx, ctx.getText, Operators.fieldAccess, Operators.fieldAccess, DispatchTypes.STATIC_DISPATCH)
    val fieldIdentifier = newFieldIdentifier(ctx)
    val astChildren     = Seq(baseNode, fieldIdentifier)
    callAst(fieldAccess, astChildren.map(Ast.apply))
  }

  protected def newFieldIdentifier(ctx: ParserRuleContext): NewFieldIdentifier = {
    val code = ctx.getText
    val name = code.replaceAll("@", "")
    NewFieldIdentifier()
      .code(code)
      .canonicalName(name)
      .lineNumber(ctx.start.getLine)
      .columnNumber(ctx.start.getCharPositionInLine)
  }

}
