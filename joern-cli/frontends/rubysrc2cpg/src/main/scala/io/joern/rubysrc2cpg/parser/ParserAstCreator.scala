package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.parser.AntlrContextHelpers.*
import io.joern.rubysrc2cpg.parser.ParserAst.*
import io.joern.rubysrc2cpg.parser.RubyParser.*
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.rubysrc2cpg.passes.Defines.getBuiltInType
import org.antlr.v4.runtime.ParserRuleContext

import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*

object ParserAstCreator {

  @tailrec
  def create(ctx: ParserRuleContext): ParserNode = ctx match
    /*
     * Fall-through contexts
     */
    case ctx: ProgramContext                                   => create(ctx.compoundStatement())
    case ctx: ExpressionOrCommandStatementContext              => create(ctx.expressionOrCommand())
    case ctx: OperatorExpressionOrCommandContext               => create(ctx.operatorExpression())
    case ctx: PrimaryOperatorExpressionContext                 => create(ctx.primary())
    case ctx: PrimaryValuePrimaryContext                       => create(ctx.primaryValue())
    case ctx: PrimaryValueCommandOrPrimaryValueContext         => create(ctx.primaryValue())
    case ctx: MethodCallOrVariableReferenceContext             => create(ctx.variableReference())
    case ctx: VariableVariableReferenceContext                 => create(ctx.variable())
    case ctx: CommandMethodInvocationWithoutParenthesesContext => create(ctx.command())
    case ctx: PseudoVariableVariableReferenceContext           => create(ctx.pseudoVariable())
    case ctx: SymbolExpressionContext                          => create(ctx.symbol())
    case ctx: NumericLiteralContext if !ctx.hasSign            => create(ctx.unsignedNumericLiteral())
    case ctx: DoClauseContext                                  => create(ctx.compoundStatement())
    case ctx: DoBlockBlockContext                              => create(ctx.doBlock())
    case ctx: ThenClauseContext                                => create(ctx.compoundStatement())
    case ctx: MandatoryMandatoryOrOptionalParameterContext     => create(ctx.mandatoryParameter())
    case ctx: OptionalMandatoryOrOptionalParameterContext      => create(ctx.optionalParameter())
    case ctx: VariableReferenceSingletonObjectContext          => create(ctx.variableReference())
    case ctx: AssociationKeyContext if Option(ctx.operatorExpression()).isDefined => create(ctx.operatorExpression())
    case ctx: CommandExpressionOrCommandContext if Option(ctx.EMARK()).isEmpty =>
      create(ctx.methodInvocationWithoutParentheses())
    case ctx: BodyStatementContext
        if ctx.rescueClause().isEmpty && Option(ctx.elseClause()).isEmpty && Option(ctx.ensureClause()).isEmpty =>
      create(ctx.compoundStatement())

    /*
     * Statements
     */
    case ctx: CompoundStatementContext => StatementList(ctx, ctx.getStatements)
    case ctx: StatementsContext        => StatementList(ctx, ctx.statement().asScala.toList)

    /*
     * Control structures
     */
    case ctx: WhileExpressionContext => WhileExpression(ctx, ctx.commandOrPrimaryValue(), ctx.doClause())
    case ctx: UntilExpressionContext => UntilExpression(ctx, ctx.commandOrPrimaryValue(), ctx.doClause())
    case ctx: IfExpressionContext =>
      IfExpression(
        ctx,
        ctx.commandOrPrimaryValue(),
        ctx.thenClause(),
        ctx.elsifClause().asScala.toList,
        Option(ctx.elseClause())
      )
    case ctx: ElsifClauseContext => ElsIfClause(ctx, ctx.expressionOrCommand(), ctx.thenClause())
    case ctx: ElseClauseContext  => ElseClause(ctx, ctx.compoundStatement())
    case ctx: UnlessExpressionContext =>
      UnlessExpression(ctx, ctx.commandOrPrimaryValue(), ctx.thenClause(), Option(ctx.elseClause()))
    case ctx: ModifierStatementContext if ctx.isIf =>
      IfExpression(ctx, ctx.expressionOrCommand(), ctx.statement(), List(), None)
    case ctx: ModifierStatementContext if ctx.isUnless =>
      UnlessExpression(ctx, ctx.expressionOrCommand(), ctx.statement(), None)
    case ctx: TernaryOperatorExpressionContext =>
      ConditionalExpression(ctx, ctx.operatorExpression(0), ctx.operatorExpression(1), ctx.operatorExpression(2))
    case ctx: ReturnMethodInvocationWithoutParenthesesContext =>
      ReturnExpression(ctx, ctx.primaryValueList().primaryValue().asScala.toList)

    /*
     * Unary expressions
     */
    case ctx: NumericLiteralContext if ctx.hasSign =>
      UnaryExpression(ctx, ctx.sign.getText, ctx.unsignedNumericLiteral())
    case ctx: UnaryExpressionContext        => UnaryExpression(ctx, ctx.unaryOperator().getText, ctx.primaryValue())
    case ctx: UnaryMinusExpressionContext   => UnaryExpression(ctx, ctx.MINUS().getText, ctx.primaryValue())
    case ctx: NotExpressionOrCommandContext => UnaryExpression(ctx, ctx.NOT().getText, ctx.expressionOrCommand())
    case ctx: CommandExpressionOrCommandContext if Option(ctx.EMARK()).isDefined =>
      UnaryExpression(ctx, "!", ctx.methodInvocationWithoutParentheses())

    /*
     * Binary expressions
     */
    case ctx: PowerExpressionContext =>
      BinaryExpression(ctx, ctx.primaryValue(0), ctx.powerOperator.getText, ctx.primaryValue(1))
    case ctx: AdditiveExpressionContext =>
      BinaryExpression(ctx, ctx.primaryValue(0), ctx.additiveOperator().getText, ctx.primaryValue(1))
    case ctx: MultiplicativeExpressionContext =>
      BinaryExpression(ctx, ctx.primaryValue(0), ctx.multiplicativeOperator().getText, ctx.primaryValue(1))
    case ctx: LogicalAndExpressionContext =>
      BinaryExpression(ctx, ctx.primaryValue(0), ctx.andOperator.getText, ctx.primaryValue(1))
    case ctx: LogicalOrExpressionContext =>
      BinaryExpression(ctx, ctx.primaryValue(0), ctx.orOperator.getText, ctx.primaryValue(1))
    case ctx: KeywordAndOrExpressionOrCommandContext =>
      BinaryExpression(ctx, ctx.lhs, ctx.binOp.getText, ctx.rhs)
    case ctx: ShiftExpressionContext =>
      BinaryExpression(ctx, ctx.primaryValue(0), ctx.bitwiseShiftOperator().getText, ctx.primaryValue(1))
    case ctx: BitwiseAndExpressionContext =>
      BinaryExpression(ctx, ctx.primaryValue(0), ctx.bitwiseAndOperator.getText, ctx.primaryValue(1))
    case ctx: BitwiseOrExpressionContext =>
      BinaryExpression(ctx, ctx.primaryValue(0), ctx.bitwiseOrOperator().getText, ctx.primaryValue(1))
    case ctx: RelationalExpressionContext =>
      BinaryExpression(ctx, ctx.primaryValue(0), ctx.relationalOperator().getText, ctx.primaryValue(1))
    case ctx: EqualityExpressionContext =>
      BinaryExpression(ctx, ctx.primaryValue(0), ctx.equalityOperator().getText, ctx.primaryValue(1))

    /*
     * Literals
     */
    case ctx: DecimalUnsignedLiteralContext              => StaticLiteral(ctx, getBuiltInType(Defines.Integer))
    case ctx: BinaryUnsignedLiteralContext               => StaticLiteral(ctx, getBuiltInType(Defines.Integer))
    case ctx: OctalUnsignedLiteralContext                => StaticLiteral(ctx, getBuiltInType(Defines.Integer))
    case ctx: HexadecimalUnsignedLiteralContext          => StaticLiteral(ctx, getBuiltInType(Defines.Integer))
    case ctx: FloatWithoutExponentUnsignedLiteralContext => StaticLiteral(ctx, getBuiltInType(Defines.Float))
    case ctx: FloatWithExponentUnsignedLiteralContext    => StaticLiteral(ctx, getBuiltInType(Defines.Float))
    case ctx: PureSymbolLiteralContext                   => StaticLiteral(ctx, getBuiltInType(Defines.Symbol))
    case ctx: SingleQuotedSymbolLiteralContext           => StaticLiteral(ctx, getBuiltInType(Defines.Symbol))
    case ctx: NilPseudoVariableContext                   => StaticLiteral(ctx, getBuiltInType(Defines.NilClass))
    case ctx: TruePseudoVariableContext                  => StaticLiteral(ctx, getBuiltInType(Defines.TrueClass))
    case ctx: FalsePseudoVariableContext                 => StaticLiteral(ctx, getBuiltInType(Defines.FalseClass))
    case ctx: SingleQuotedStringExpressionContext if !ctx.isInterpolated =>
      StaticLiteral(ctx, getBuiltInType(Defines.String))
    case ctx: QuotedNonExpandedStringLiteralContext => StaticLiteral(ctx, getBuiltInType(Defines.String))
    case ctx: DoubleQuotedStringExpressionContext if !ctx.isInterpolated =>
      StaticLiteral(ctx, getBuiltInType(Defines.String))
    case ctx: RegularExpressionLiteralContext if ctx.isStatic => StaticLiteral(ctx, getBuiltInType(Defines.Regexp))
    case ctx: SingleQuotedStringExpressionContext if ctx.isInterpolated =>
      DynamicLiteral(ctx, getBuiltInType(Defines.String), ctx.interpolations)
    case ctx: DoubleQuotedStringExpressionContext if ctx.isInterpolated =>
      DynamicLiteral(ctx, getBuiltInType(Defines.String), ctx.interpolations)

    /*
     * Assignments
     */
    case ctx: LocalVariableAssignmentExpressionContext =>
      SingleAssignment(ctx, ctx.lhs, ctx.assignmentOperator().getText, ctx.rhs)
    case ctx: AttributeAssignmentExpressionContext =>
      AttributeAssignment(ctx, ctx.primaryValue(), ctx.op.getText, ctx.methodName().getText, ctx.operatorExpression())

    /*
     * Blocks
     */
    case ctx: CurlyBracesBlockContext =>
      Block(ctx, Option(ctx.blockParameter()).fold(List())(_.parameters), ctx.compoundStatement())
    case ctx: DoBlockContext =>
      Block(ctx, Option(ctx.blockParameter()).fold(List())(_.parameters), ctx.bodyStatement())

    /*
     * Invocations
     */
    case ctx: SimpleCommandContext if !ctx.methodIdentifier().isAttrDeclaration =>
      SimpleCall(ctx, ctx.methodIdentifier(), ctx.commandArgument().arguments)
    case ctx: IsDefinedExpressionContext =>
      SimpleCall(ctx, ctx.isDefinedKeyword, List(ctx.expressionOrCommand()))
    case ctx: IsDefinedCommandContext =>
      SimpleCall(ctx, ctx.isDefinedKeyword, List(ctx.primaryValue()))
    case ctx: MethodCallExpressionContext =>
      SimpleCall(ctx, ctx.methodOnlyIdentifier(), List())
    case ctx: MethodCallWithBlockExpressionContext =>
      SimpleCallWithBlock(ctx, ctx.methodIdentifier(), List(), ctx.block())
    case ctx: MethodCallWithParenthesesExpressionContext =>
      if (Option(ctx.block()).isDefined) {
        SimpleCallWithBlock(ctx, ctx.methodIdentifier(), ctx.argumentWithParentheses().arguments, ctx.block())
      } else {
        SimpleCall(ctx, ctx.methodIdentifier(), ctx.argumentWithParentheses().arguments)
      }
    case ctx: MemberAccessExpressionContext
        if Option(ctx.argumentWithParentheses()).isDefined && Option(ctx.block()).isEmpty =>
      MemberCall(
        ctx,
        ctx.primaryValue(),
        ctx.op.getText,
        ctx.methodName().getText,
        ctx.argumentWithParentheses().arguments
      )
    case ctx: MemberAccessExpressionContext if Option(ctx.block()).isDefined =>
      MemberCallWithBlock(
        ctx,
        ctx.primaryValue(),
        ctx.op.getText,
        ctx.methodName().getText,
        Option(ctx.argumentWithParentheses()).map(_.arguments).getOrElse(List()),
        ctx.block()
      )

    /*
     * Member accesses
     */
    case ctx: MemberAccessExpressionContext
        if Option(ctx.argumentWithParentheses).isEmpty && Option(ctx.block()).isEmpty =>
      MemberAccess(ctx, ctx.primaryValue(), ctx.op.getText, ctx.methodName().getText)
    case ctx: IndexingAccessExpressionContext =>
      IndexAccess(ctx, ctx.primaryValue(), Option(ctx.indexingArgumentList()).map(_.arguments).getOrElse(List()))

    /*
     * Identifiers
     */
    case ctx: ConstantIdentifierVariableContext => SimpleIdentifier(ctx)
    case ctx: GlobalIdentifierVariableContext   => SimpleIdentifier(ctx)
    case ctx: ClassIdentifierVariableContext    => SimpleIdentifier(ctx)
    case ctx: InstanceIdentifierVariableContext => SimpleIdentifier(ctx)
    case ctx: LocalIdentifierVariableContext    => SimpleIdentifier(ctx)
    case ctx: ClassNameContext                  => SimpleIdentifier(ctx)
    case ctx: MethodIdentifierContext           => SimpleIdentifier(ctx)
    case ctx: IsDefinedKeywordContext           => SimpleIdentifier(ctx)
    case ctx: LinePseudoVariableContext         => SimpleIdentifier(ctx, Some(getBuiltInType(Defines.Integer)))
    case ctx: FilePseudoVariableContext         => SimpleIdentifier(ctx, Some(getBuiltInType(Defines.String)))
    case ctx: EncodingPseudoVariableContext     => SimpleIdentifier(ctx, Some(getBuiltInType(Defines.Encoding)))
    case ctx: SelfPseudoVariableContext         => SelfIdentifier(ctx)

    /*
     * Array literals
     */
    case ctx: BracketedArrayLiteralContext =>
      ArrayLiteral(ctx, Option(ctx.indexingArgumentList()).map(_.arguments).getOrElse(List()))
    case ctx: QuotedNonExpandedStringArrayLiteralContext =>
      ArrayLiteral(ctx, Option(ctx.quotedNonExpandedArrayElementList()).map(_.elements).getOrElse(List()))
    case ctx: QuotedNonExpandedSymbolArrayLiteralContext =>
      ArrayLiteral(ctx, Option(ctx.quotedNonExpandedArrayElementList()).map(_.elements).getOrElse(List()))

    case ctx: RangeExpressionContext =>
      RangeExpression(ctx, ctx.primaryValue(0), ctx.primaryValue(1))

    /*
     * Hash literals
     */
    case ctx: HashLiteralContext =>
      HashLiteral(ctx, Option(ctx.associationList()).map(_.associations).getOrElse(List()))
    case ctx: AssociationContext =>
      Association(ctx, ctx.associationKey(), ctx.operatorExpression())

    /*
     * Declarations
     */
    case ctx: ModuleDefinitionContext =>
      ModuleDeclaration(ctx, ctx.classPath(), ctx.bodyStatement())

    case ctx: ClassDefinitionContext =>
      ClassDeclaration(ctx, ctx.classPath(), Option(ctx.commandOrPrimaryValue()), ctx.bodyStatement())
    case ctx: MethodDefinitionContext =>
      MethodDeclaration(
        ctx,
        ctx.definedMethodName().getText,
        Option(ctx.methodParameterPart().parameterList()).fold(List())(_.parameters),
        ctx.bodyStatement()
      )
    case ctx: EndlessMethodDefinitionContext =>
      MethodDeclaration(
        ctx,
        ctx.definedMethodName().getText,
        Option(ctx.parameterList()).fold(List())(_.parameters),
        ctx.commandOrPrimaryValue()
      )
    case ctx: SingletonMethodDefinitionContext =>
      SingletonMethodDeclaration(
        ctx,
        ctx.singletonObject(),
        ctx.definedMethodName().getText,
        Option(ctx.methodParameterPart().parameterList()).fold(List())(_.parameters),
        ctx.bodyStatement()
      )
    case ctx: SimpleCommandContext if ctx.methodIdentifier().isAttrDeclaration =>
      FieldsDeclaration(ctx, ctx.commandArgument().arguments)
    case ctx: ProcParameterContext      => ProcParameter(ctx, ctx.procParameterName())
    case ctx: HashParameterContext      => HashParameter(ctx, Option(ctx.LOCAL_VARIABLE_IDENTIFIER()).map(_.getText))
    case ctx: ArrayParameterContext     => ArrayParameter(ctx, Option(ctx.LOCAL_VARIABLE_IDENTIFIER()).map(_.getText))
    case ctx: OptionalParameterContext  => OptionalParameter(ctx, ctx.optionalParameterName(), ctx.operatorExpression())
    case ctx: MandatoryParameterContext => MandatoryParameter(ctx)
    case ctx: VariableLeftHandSideContext if Option(ctx.primary()).isEmpty => MandatoryParameter(ctx)

    // Fallback
    case _ => Unknown(ctx)
}
