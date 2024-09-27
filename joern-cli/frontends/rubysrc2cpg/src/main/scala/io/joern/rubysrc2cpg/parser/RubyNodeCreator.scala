package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.astcreation.RubyIntermediateAst.{Block, *}
import io.joern.rubysrc2cpg.parser.AntlrContextHelpers.*
import io.joern.rubysrc2cpg.parser.RubyParser.*
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.rubysrc2cpg.passes.Defines.{Self, getBuiltInType}
import io.joern.rubysrc2cpg.passes.GlobalTypes.builtinPrefix
import io.joern.rubysrc2cpg.utils.FreshNameGenerator
import io.joern.x2cpg.frontendspecific.rubysrc2cpg.ImportsPass
import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.tree.{ParseTree, RuleNode}
import org.slf4j.LoggerFactory

import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*

/** Converts an ANTLR Ruby Parse Tree into the intermediate Ruby AST.
  */
class RubyNodeCreator(
  variableNameGen: FreshNameGenerator[String] = FreshNameGenerator(id => s"<tmp-$id>"),
  procParamGen: FreshNameGenerator[Left[String, Nothing]] = FreshNameGenerator(id => Left(s"<proc-param-$id>"))
) extends RubyParserBaseVisitor[RubyExpression] {

  private val logger       = LoggerFactory.getLogger(getClass)
  private val classNameGen = FreshNameGenerator(id => s"<anon-class-$id>")

  protected def freshClassName(span: TextSpan): SimpleIdentifier = {
    SimpleIdentifier(None)(span.spanStart(classNameGen.fresh))
  }

  private def defaultTextSpan(code: String = ""): TextSpan = TextSpan(None, None, None, None, None, code)

  override def defaultResult(): RubyExpression = Unknown()(defaultTextSpan())

  override protected def shouldVisitNextChild(node: RuleNode, currentResult: RubyExpression): Boolean =
    currentResult.isInstanceOf[Unknown]

  override def visit(tree: ParseTree): RubyExpression = {
    Option(tree).map(super.visit).getOrElse(defaultResult())
  }

  override def visitProgram(ctx: RubyParser.ProgramContext): RubyExpression = {
    visit(ctx.compoundStatement())
  }

  override def visitCompoundStatement(ctx: RubyParser.CompoundStatementContext): RubyExpression = {
    StatementList(ctx.getStatements.map(visit))(ctx.toTextSpan)
  }

  override def visitNextWithoutArguments(ctx: RubyParser.NextWithoutArgumentsContext): RubyExpression = {
    NextExpression()(ctx.toTextSpan)
  }

  override def visitGroupingStatement(ctx: RubyParser.GroupingStatementContext): RubyExpression = {
    // When there's only 1 statement, we can use it directly, instead of wrapping it in a StatementList.
    val statements = ctx.compoundStatement().getStatements.map(visit)
    if (statements.size == 1) {
      statements.head
    } else {
      StatementList(statements)(ctx.toTextSpan)
    }
  }

  override def visitStatements(ctx: RubyParser.StatementsContext): RubyExpression = {
    StatementList(ctx.statement().asScala.map(visit).toList)(ctx.toTextSpan)
  }

  override def visitWhileExpression(ctx: RubyParser.WhileExpressionContext): RubyExpression = {
    val condition = visit(ctx.expressionOrCommand())
    val body      = visit(ctx.doClause())
    WhileExpression(condition, body)(ctx.toTextSpan)
  }

  override def visitUntilExpression(ctx: RubyParser.UntilExpressionContext): RubyExpression = {
    val condition = visit(ctx.expressionOrCommand())
    val body      = visit(ctx.doClause())
    UntilExpression(condition, body)(ctx.toTextSpan)
  }

  override def visitBeginEndExpression(ctx: RubyParser.BeginEndExpressionContext): RubyExpression = {
    visit(ctx.bodyStatement())
  }

  override def visitIfExpression(ctx: RubyParser.IfExpressionContext): RubyExpression = {
    val condition = visit(ctx.expressionOrCommand())
    val thenBody  = visit(ctx.thenClause())
    val elsifs    = ctx.elsifClause().asScala.map(visit).toList
    val elseBody  = Option(ctx.elseClause()).map(visit)
    IfExpression(condition, thenBody, elsifs, elseBody)(ctx.toTextSpan)
  }

  override def visitElsifClause(ctx: RubyParser.ElsifClauseContext): RubyExpression = {
    ElsIfClause(visit(ctx.expressionOrCommand()), visit(ctx.thenClause()))(ctx.toTextSpan)
  }

  override def visitElseClause(ctx: RubyParser.ElseClauseContext): RubyExpression = {
    ElseClause(visit(ctx.compoundStatement()))(ctx.toTextSpan)
  }

  override def visitUnlessExpression(ctx: RubyParser.UnlessExpressionContext): RubyExpression = {
    val condition = visit(ctx.expressionOrCommand())
    val thenBody  = visit(ctx.thenClause())
    val elseBody  = Option(ctx.elseClause()).map(visit)
    UnlessExpression(condition, thenBody, elseBody)(ctx.toTextSpan)
  }

  override def visitForExpression(ctx: RubyParser.ForExpressionContext): RubyExpression = {
    val forVariable      = visit(ctx.forVariable())
    val iterableVariable = visit(ctx.commandOrPrimaryValue())
    val doBlock          = visit(ctx.doClause())
    ForExpression(forVariable, iterableVariable, doBlock)(ctx.toTextSpan)
  }

  override def visitForVariable(ctx: RubyParser.ForVariableContext): RubyExpression = {
    if (ctx.leftHandSide() != null) visit(ctx.leftHandSide())
    else visit(ctx.multipleLeftHandSide())
  }

  override def visitModifierStatement(ctx: RubyParser.ModifierStatementContext): RubyExpression = {
    ctx.statementModifier().getText match
      case "if" =>
        val condition = visit(ctx.expressionOrCommand())
        val thenBody  = visit(ctx.statement())
        val elsifs    = List()
        val elseBody  = None
        IfExpression(condition, thenBody, elsifs, elseBody)(ctx.toTextSpan)
      case "unless" =>
        val condition = visit(ctx.expressionOrCommand())
        val thenBody  = visit(ctx.statement())
        val elseBody  = None
        UnlessExpression(condition, thenBody, elseBody)(ctx.toTextSpan)
      case "while" =>
        val condition = visit(ctx.expressionOrCommand())
        val body      = visit(ctx.statement())
        WhileExpression(condition, body)(ctx.toTextSpan)
      case "until" =>
        val condition = visit(ctx.expressionOrCommand())
        val body      = visit(ctx.statement())
        DoWhileExpression(condition, body)(ctx.toTextSpan)
      case "rescue" =>
        val body       = visit(ctx.statement())
        val thenClause = visit(ctx.expressionOrCommand())
        val rescueClause =
          RescueClause(Option.empty, Option.empty, thenClause)(ctx.toTextSpan)
        val rescExp =
          RescueExpression(body, List(rescueClause), Option.empty, Option.empty)(ctx.toTextSpan)
        rescExp
      case _ =>
        logger.warn(s"Unhandled modifier statement ${ctx.getClass} ${ctx.toTextSpan} ")
        Unknown()(ctx.toTextSpan)
  }

  override def visitCommandTernaryOperatorExpression(ctx: CommandTernaryOperatorExpressionContext): RubyExpression = {
    val condition = visit(ctx.operatorExpression(0))
    val thenBody  = visit(ctx.operatorExpression(1))
    val elseBody  = visit(ctx.operatorExpression(2))
    IfExpression(
      condition,
      thenBody,
      List.empty,
      Option(ElseClause(StatementList(elseBody :: Nil)(elseBody.span))(elseBody.span))
    )(ctx.toTextSpan)
  }

  override def visitTernaryOperatorExpression(ctx: RubyParser.TernaryOperatorExpressionContext): RubyExpression = {
    val condition = visit(ctx.operatorExpression(0))
    val thenBody  = visit(ctx.operatorExpression(1))
    val elseBody  = visit(ctx.operatorExpression(2))
    IfExpression(
      condition,
      thenBody,
      List.empty,
      Option(ElseClause(StatementList(elseBody :: Nil)(elseBody.span))(elseBody.span))
    )(ctx.toTextSpan)
  }

  override def visitReturnMethodInvocationWithoutParentheses(
    ctx: RubyParser.ReturnMethodInvocationWithoutParenthesesContext
  ): RubyExpression = {
    val expressions = Option(ctx.primaryValueListWithAssociation().methodInvocationWithoutParentheses()) match {
      case Some(methodInvocation) => visit(methodInvocation) :: Nil
      case None                   => ctx.primaryValueListWithAssociation().elements.map(visit).toList
    }

    ReturnExpression(expressions)(ctx.toTextSpan)
  }

  override def visitReturnWithoutArguments(ctx: RubyParser.ReturnWithoutArgumentsContext): RubyExpression = {
    ReturnExpression(Nil)(ctx.toTextSpan)
  }

  override def visitNumericLiteral(ctx: RubyParser.NumericLiteralContext): RubyExpression = {
    if (ctx.hasSign) {
      UnaryExpression(ctx.sign.getText, visit(ctx.unsignedNumericLiteral()))(ctx.toTextSpan)
    } else {
      visit(ctx.unsignedNumericLiteral())
    }
  }

  override def visitUnaryExpression(ctx: RubyParser.UnaryExpressionContext): RubyExpression = {
    UnaryExpression(ctx.unaryOperator().getText, visit(ctx.primaryValue()))(ctx.toTextSpan)
  }

  override def visitUnaryMinusExpression(ctx: RubyParser.UnaryMinusExpressionContext): RubyExpression = {
    UnaryExpression(ctx.MINUS().getText, visit(ctx.primaryValue()))(ctx.toTextSpan)
  }

  override def visitNotExpressionOrCommand(ctx: RubyParser.NotExpressionOrCommandContext): RubyExpression = {
    UnaryExpression(ctx.NOT().getText, visit(ctx.expressionOrCommand()))(ctx.toTextSpan)
  }

  override def visitCommandExpressionOrCommand(ctx: RubyParser.CommandExpressionOrCommandContext): RubyExpression = {
    val methodInvocation = visit(ctx.methodInvocationWithoutParentheses())
    if (Option(ctx.EMARK()).isDefined) {
      UnaryExpression(ctx.EMARK().getText, methodInvocation)(ctx.toTextSpan)
    } else {
      methodInvocation
    }
  }

  override def visitCommandWithDoBlock(ctx: CommandWithDoBlockContext): RubyExpression = {
    val name = Option(ctx.methodIdentifier()).orElse(Option(ctx.methodName())).map(visit).getOrElse(defaultResult())
    val arguments = ctx.arguments.map(visit)
    val block     = visit(ctx.doBlock()).asInstanceOf[Block]
    SimpleCallWithBlock(name, arguments, block)(ctx.toTextSpan)
  }

  override def visitHereDocs(ctx: RubyParser.HereDocsContext): RubyExpression = {
    HereDocNode(ctx.hereDoc().getText)(ctx.toTextSpan)
  }

  override def visitPrimaryOperatorExpression(ctx: RubyParser.PrimaryOperatorExpressionContext): RubyExpression = {
    super.visitPrimaryOperatorExpression(ctx) match {
      case expr @ BinaryExpression(SimpleCall(lhs: SimpleIdentifier, Nil), "*", rhs) if lhs.text.endsWith("=") =>
        // fixme: This workaround handles a parser ambiguity with method identifiers having `=` and assignments with
        //  splatting on the RHS. The Ruby parser gives precedence to assignments over methods called with this suffix
        //  however. See https://github.com/joernio/joern/issues/4775
        val newLhs = SimpleIdentifier(None)(lhs.span.spanStart(lhs.span.text.stripSuffix("=")))
        val newRhs = SplattingRubyNode(rhs)(rhs.span.spanStart(s"*${rhs.span.text}"))
        SingleAssignment(newLhs, "=", newRhs)(expr.span)
      case x => x
    }
  }

  override def visitPowerExpression(ctx: RubyParser.PowerExpressionContext): RubyExpression = {
    BinaryExpression(visit(ctx.primaryValue(0)), ctx.powerOperator.getText, visit(ctx.primaryValue(1)))(ctx.toTextSpan)
  }

  override def visitAdditiveExpression(ctx: RubyParser.AdditiveExpressionContext): RubyExpression = {
    BinaryExpression(visit(ctx.primaryValue(0)), ctx.additiveOperator().getText, visit(ctx.primaryValue(1)))(
      ctx.toTextSpan
    )
  }

  override def visitMultiplicativeExpression(ctx: RubyParser.MultiplicativeExpressionContext): RubyExpression = {
    BinaryExpression(visit(ctx.primaryValue(0)), ctx.multiplicativeOperator().getText, visit(ctx.primaryValue(1)))(
      ctx.toTextSpan
    )
  }

  override def visitLogicalAndExpression(ctx: RubyParser.LogicalAndExpressionContext): RubyExpression = {
    val rhs = Option(ctx.RETURN()) match {
      case Some(returnExpr) => ReturnExpression(List.empty)(ctx.toTextSpan.spanStart(returnExpr.toString))
      case None             => visit(ctx.primaryValue(1))
    }

    BinaryExpression(visit(ctx.primaryValue(0)), ctx.andOperator.getText, rhs)(ctx.toTextSpan)
  }

  override def visitLogicalOrExpression(ctx: RubyParser.LogicalOrExpressionContext): RubyExpression = {
    val rhs = Option(ctx.RETURN()) match {
      case Some(returnExpr) => ReturnExpression(List.empty)(ctx.toTextSpan.spanStart(returnExpr.toString))
      case None             => visit(ctx.primaryValue(1))
    }

    BinaryExpression(visit(ctx.primaryValue(0)), ctx.orOperator.getText, rhs)(ctx.toTextSpan)
  }

  override def visitKeywordAndOrExpressionOrCommand(
    ctx: RubyParser.KeywordAndOrExpressionOrCommandContext
  ): RubyExpression = {
    BinaryExpression(visit(ctx.lhs), ctx.binOp.getText, visit(ctx.rhs))(ctx.toTextSpan)
  }

  override def visitShiftExpression(ctx: RubyParser.ShiftExpressionContext): RubyExpression = {
    BinaryExpression(visit(ctx.primaryValue(0)), ctx.bitwiseShiftOperator().getText, visit(ctx.primaryValue(1)))(
      ctx.toTextSpan
    )
  }

  override def visitBitwiseAndExpression(ctx: RubyParser.BitwiseAndExpressionContext): RubyExpression = {
    BinaryExpression(visit(ctx.primaryValue(0)), ctx.bitwiseAndOperator.getText, visit(ctx.primaryValue(1)))(
      ctx.toTextSpan
    )
  }

  override def visitBitwiseOrExpression(ctx: RubyParser.BitwiseOrExpressionContext): RubyExpression = {
    BinaryExpression(visit(ctx.primaryValue(0)), ctx.bitwiseOrOperator().getText, visit(ctx.primaryValue(1)))(
      ctx.toTextSpan
    )
  }

  override def visitRelationalExpression(ctx: RubyParser.RelationalExpressionContext): RubyExpression = {
    BinaryExpression(visit(ctx.primaryValue(0)), ctx.relationalOperator().getText, visit(ctx.primaryValue(1)))(
      ctx.toTextSpan
    )
  }

  override def visitEqualityExpression(ctx: RubyParser.EqualityExpressionContext): RubyExpression = {
    BinaryExpression(visit(ctx.primaryValue(0)), ctx.equalityOperator().getText, visit(ctx.primaryValue(1)))(
      ctx.toTextSpan
    )
  }

  override def visitDecimalUnsignedLiteral(ctx: RubyParser.DecimalUnsignedLiteralContext): RubyExpression = {
    StaticLiteral(getBuiltInType(Defines.Integer))(ctx.toTextSpan)
  }

  override def visitBinaryUnsignedLiteral(ctx: RubyParser.BinaryUnsignedLiteralContext): RubyExpression = {
    StaticLiteral(getBuiltInType(Defines.Integer))(ctx.toTextSpan)
  }

  override def visitOctalUnsignedLiteral(ctx: RubyParser.OctalUnsignedLiteralContext): RubyExpression = {
    StaticLiteral(getBuiltInType(Defines.Integer))(ctx.toTextSpan)
  }

  override def visitHexadecimalUnsignedLiteral(ctx: RubyParser.HexadecimalUnsignedLiteralContext): RubyExpression = {
    StaticLiteral(getBuiltInType(Defines.Integer))(ctx.toTextSpan)
  }

  override def visitFloatWithExponentUnsignedLiteral(
    ctx: RubyParser.FloatWithExponentUnsignedLiteralContext
  ): RubyExpression = {
    StaticLiteral(getBuiltInType(Defines.Float))(ctx.toTextSpan)
  }

  override def visitFloatWithoutExponentUnsignedLiteral(
    ctx: RubyParser.FloatWithoutExponentUnsignedLiteralContext
  ): RubyExpression = {
    StaticLiteral(getBuiltInType(Defines.Float))(ctx.toTextSpan)
  }

  override def visitPureSymbolLiteral(ctx: RubyParser.PureSymbolLiteralContext): RubyExpression = {
    StaticLiteral(getBuiltInType(Defines.Symbol))(ctx.toTextSpan)
  }

  override def visitSingleQuotedSymbolLiteral(ctx: RubyParser.SingleQuotedSymbolLiteralContext): RubyExpression = {
    StaticLiteral(getBuiltInType(Defines.Symbol))(ctx.toTextSpan)
  }

  override def visitNilPseudoVariable(ctx: RubyParser.NilPseudoVariableContext): RubyExpression = {
    StaticLiteral(getBuiltInType(Defines.NilClass))(ctx.toTextSpan)
  }

  override def visitTruePseudoVariable(ctx: RubyParser.TruePseudoVariableContext): RubyExpression = {
    StaticLiteral(getBuiltInType(Defines.TrueClass))(ctx.toTextSpan)
  }

  override def visitFalsePseudoVariable(ctx: RubyParser.FalsePseudoVariableContext): RubyExpression = {
    StaticLiteral(getBuiltInType(Defines.FalseClass))(ctx.toTextSpan)
  }

  override def visitSingleQuotedStringExpression(
    ctx: RubyParser.SingleQuotedStringExpressionContext
  ): RubyExpression = {
    if (!ctx.isInterpolated) {
      StaticLiteral(getBuiltInType(Defines.String))(ctx.toTextSpan)
    } else {
      DynamicLiteral(getBuiltInType(Defines.String), ctx.interpolations.map(visit))(ctx.toTextSpan)
    }
  }

  override def visitQuotedNonExpandedStringLiteral(
    ctx: RubyParser.QuotedNonExpandedStringLiteralContext
  ): RubyExpression = {
    StaticLiteral(getBuiltInType(Defines.String))(ctx.toTextSpan)
  }

  override def visitQuotedExpandedStringArrayLiteral(
    ctx: RubyParser.QuotedExpandedStringArrayLiteralContext
  ): RubyExpression = {
    val elements =
      if Option(ctx.quotedExpandedArrayElementList()).isDefined then
        ctx.quotedExpandedArrayElementList().elements.map(visit)
      else List.empty

    ArrayLiteral(elements)(ctx.toTextSpan)
  }

  override def visitDoubleQuotedStringExpression(
    ctx: RubyParser.DoubleQuotedStringExpressionContext
  ): RubyExpression = {
    if (!ctx.isInterpolated) {
      StaticLiteral(getBuiltInType(Defines.String))(ctx.toTextSpan)
    } else {
      DynamicLiteral(getBuiltInType(Defines.String), ctx.interpolations.map(visit))(ctx.toTextSpan)
    }
  }

  override def visitDoubleQuotedSymbolLiteral(ctx: RubyParser.DoubleQuotedSymbolLiteralContext): RubyExpression = {
    if (!ctx.isInterpolated) {
      StaticLiteral(getBuiltInType(Defines.Symbol))(ctx.toTextSpan)
    } else {
      DynamicLiteral(getBuiltInType(Defines.Symbol), ctx.interpolations.map(visit))(ctx.toTextSpan)
    }
  }

  override def visitQuotedExpandedStringLiteral(ctx: RubyParser.QuotedExpandedStringLiteralContext): RubyExpression = {
    if (!ctx.isInterpolated) {
      StaticLiteral(getBuiltInType(Defines.String))(ctx.toTextSpan)
    } else {
      DynamicLiteral(getBuiltInType(Defines.String), ctx.interpolations.map(visit))(ctx.toTextSpan)
    }
  }

  override def visitRegularExpressionLiteral(ctx: RubyParser.RegularExpressionLiteralContext): RubyExpression = {
    if (ctx.isStatic) {
      StaticLiteral(getBuiltInType(Defines.Regexp))(ctx.toTextSpan)
    } else {
      DynamicLiteral(getBuiltInType(Defines.Regexp), ctx.interpolations.map(visit))(ctx.toTextSpan)
    }
  }

  override def visitQuotedExpandedRegularExpressionLiteral(
    ctx: RubyParser.QuotedExpandedRegularExpressionLiteralContext
  ): RubyExpression = {
    if (ctx.isStatic) {
      StaticLiteral(getBuiltInType(Defines.Regexp))(ctx.toTextSpan)
    } else {
      DynamicLiteral(getBuiltInType(Defines.Regexp), ctx.interpolations.map(visit))(ctx.toTextSpan)
    }
  }

  override def visitQuotedExpandedExternalCommandLiteral(
    ctx: RubyParser.QuotedExpandedExternalCommandLiteralContext
  ): RubyExpression = {
    val commandLiteral =
      if ctx.quotedExpandedLiteralStringContent.asScala.nonEmpty then
        StaticLiteral(Defines.String)(ctx.quotedExpandedLiteralStringContent.asScala.toList.map(_.toTextSpan).head)
      else StaticLiteral(Defines.String)(ctx.toTextSpan.spanStart())

    SimpleCall(SimpleIdentifier()(ctx.toTextSpan.spanStart("exec")), List(commandLiteral))(ctx.toTextSpan)
  }

  override def visitCurlyBracesBlock(ctx: RubyParser.CurlyBracesBlockContext): RubyExpression = {
    val parameters =
      Option(ctx.blockParameter()).fold(List())(_.parameters).map(visit).sortBy(x => (x.span.line, x.span.column))

    val assignments = parameters.collect { case x: GroupedParameter =>
      x.multipleAssignment
    }

    val body                = visit(ctx.compoundStatement())
    val bodyWithAssignments = StatementList(assignments ++ body.asStatementList.statements)(body.span)

    Block(parameters, bodyWithAssignments)(ctx.toTextSpan)
  }

  override def visitGroupedParameterList(ctx: RubyParser.GroupedParameterListContext): RubyExpression = {
    val freshTmpVar       = variableNameGen.fresh
    val tmpMandatoryParam = MandatoryParameter(freshTmpVar)(ctx.toTextSpan.spanStart(freshTmpVar))

    val singleAssignments = ctx.parameters.map { param =>
      val rhsSplattingNode = SplattingRubyNode(tmpMandatoryParam)(ctx.toTextSpan.spanStart(s"*$freshTmpVar"))
      val lhs = param match {
        case x: MandatoryParameterContext => SimpleIdentifier()(ctx.toTextSpan.spanStart(x.getText))
        case x: ArrayParameterContext =>
          SplattingRubyNode(SimpleIdentifier()(ctx.toTextSpan.spanStart(x.getText.stripPrefix("*"))))(
            ctx.toTextSpan.spanStart(s"${x.getText}")
          )
        case x =>
          logger.warn(s"Invalid parameter type in grouped parameter list: ${x.getClass}")
          defaultResult()
      }
      SingleAssignment(lhs, "=", rhsSplattingNode)(
        ctx.toTextSpan.spanStart(s"${lhs.span.text} = ${rhsSplattingNode.span.text}")
      )
    }

    GroupedParameter(
      tmpMandatoryParam.span.text,
      tmpMandatoryParam,
      GroupedParameterDesugaring(singleAssignments)(ctx.toTextSpan)
    )(ctx.toTextSpan)
  }

  override def visitDoBlock(ctx: RubyParser.DoBlockContext): RubyExpression = {
    val parameters = Option(ctx.blockParameter()).fold(List())(_.parameters).map(visit)
    val body       = visit(ctx.bodyStatement())
    Block(parameters, body)(ctx.toTextSpan)
  }
  override def visitLocalVariableAssignmentExpression(
    ctx: RubyParser.LocalVariableAssignmentExpressionContext
  ): RubyExpression = {
    val lhs = visit(ctx.lhs)
    val rhs = visit(ctx.rhs)
    val op  = ctx.assignmentOperator().getText

    if op == "||=" || op == "&&=" then lowerAssignmentOperator(lhs, rhs, op, ctx.toTextSpan)
    else SingleAssignment(lhs, op, rhs)(ctx.toTextSpan)
  }

  override def visitSingleAssignmentStatement(ctx: RubyParser.SingleAssignmentStatementContext): RubyExpression = {
    val lhs =
      if Option(ctx.CONSTANT_IDENTIFIER()).isDefined then
        MemberAccess(
          SelfIdentifier()(ctx.toTextSpan.spanStart(Defines.Self)),
          ctx.COLON2().getText,
          ctx.CONSTANT_IDENTIFIER().getText
        )(ctx.toTextSpan.spanStart(s"$Self::${ctx.CONSTANT_IDENTIFIER().getText}"))
      else if Option(ctx.variable()).isDefined then visit(ctx.variable())
      else if Option(ctx.indexingArgumentList()).isDefined then
        val target = visit(ctx.primary())
        val args =
          Option(ctx.indexingArgumentList()).map(_.arguments).getOrElse(List()).map(visit)
        IndexAccess(target, args)(
          ctx.toTextSpan.spanStart(s"${target.span.text}[${args.map(_.span.text).mkString(",")}]")
        )
      else
        val memberAccessOp = Option(ctx.DOT) match {
          case Some(dot) => ctx.DOT().getText
          case None      => ctx.COLON2().getText
        }
        val target     = visit(ctx.primary)
        val methodName = visit(ctx.methodName)
        MemberAccess(target, memberAccessOp, methodName.span.text)(
          ctx.toTextSpan.spanStart(s"${target.span.text}$memberAccessOp${methodName.span.text}")
        )

    val rhs = visit(ctx.methodInvocationWithoutParentheses())
    val op  = ctx.assignmentOperator().getText

    if op == "||=" || op == "&&=" then lowerAssignmentOperator(lhs, rhs, op, ctx.toTextSpan)
    else SingleAssignment(lhs, op, rhs)(ctx.toTextSpan)
  }

  private def flattenStatementLists(x: List[RubyExpression]): List[RubyExpression] = {
    x match {
      case (head: StatementList) :: xs => head.statements ++ flattenStatementLists(xs)
      case head :: tail                => head +: flattenStatementLists(tail)
      case Nil                         => Nil
    }
  }

  override def visitMultipleAssignmentStatement(ctx: RubyParser.MultipleAssignmentStatementContext): RubyExpression = {

    /** Recursively expand and duplicate splatting nodes so that they line up with what they consume.
      *
      * @param nodes
      *   the splat nodes.
      * @param expandSize
      *   how many more duplicates to create.
      */
    def slurp(nodes: List[RubyExpression], expandSize: Int): List[RubyExpression] = nodes match {
      case (head: SplattingRubyNode) :: tail if expandSize > 0 => head :: slurp(head :: tail, expandSize - 1)
      case head :: tail                                        => head :: slurp(tail, expandSize)
      case Nil                                                 => List.empty
    }

    val lhsNodes = Option(ctx.multipleLeftHandSide())
      .map(visit)
      .orElse(
        Option(ctx.leftHandSide())
          .map(visit)
          .map(node => SplattingRubyNode(node)(node.span.spanStart(s"*${node.span.text}")))
      )
      .getOrElse(defaultResult()) match {
      case x: StatementList => flattenStatementLists(x.statements)
      case x                => List(x)
    }
    val rhsNodes = Option(ctx.multipleRightHandSide())
      .map(visit)
      .getOrElse(defaultResult()) match {
      case x: StatementList => flattenStatementLists(x.statements)
      case x                => List(x)
    }
    val op = ctx.EQ().toString

    lazy val defaultAssignments = lhsNodes
      .zipAll(rhsNodes, defaultResult(), Unknown()(defaultTextSpan(Defines.Undefined)))
      .map { case (lhs, rhs) => SingleAssignment(lhs, op, rhs)(ctx.toTextSpan) }

    val assignments = if ((lhsNodes ++ rhsNodes).exists(_.isInstanceOf[SplattingRubyNode])) {
      rhsNodes.size - lhsNodes.size match {
        // Handle slurping the RHS values
        case x if x > 0 => {
          val slurpedLhs = slurp(lhsNodes, x)

          slurpedLhs
            .zip(rhsNodes)
            .groupBy(_._1)
            .toSeq
            .map { case (lhsNode, xs) => lhsNode -> xs.map(_._2) }
            .sortBy { x => slurpedLhs.indexOf(x._1) } // groupBy produces a map which discards insertion order
            .map {
              case (SplattingRubyNode(lhs), rhss) =>
                SingleAssignment(lhs, op, ArrayLiteral(rhss)(ctx.toTextSpan))(ctx.toTextSpan)
              case (lhs, rhs :: Nil) => SingleAssignment(lhs, op, rhs)(ctx.toTextSpan)
              case (lhs, rhss)       => SingleAssignment(lhs, op, ArrayLiteral(rhss)(ctx.toTextSpan))(ctx.toTextSpan)
            }
            .toList
        }
        // Handle splitting the RHS values
        case x if x < 0 => {
          val slurpedRhs = slurp(rhsNodes, Math.abs(x))

          lhsNodes
            .zip(slurpedRhs)
            .groupBy(_._2)
            .toSeq
            .map { case (rhsNode, xs) => rhsNode -> xs.map(_._1) }
            .sortBy { x => slurpedRhs.indexOf(x._1) } // groupBy produces a map which discards insertion order
            .flatMap {
              case (SplattingRubyNode(rhs), lhss) =>
                lhss.map(SingleAssignment(_, op, SplattingRubyNode(rhs)(rhs.span))(ctx.toTextSpan))
              case (rhs, lhs :: Nil) => Seq(SingleAssignment(lhs, op, rhs)(ctx.toTextSpan))
              case (rhs, lhss) => lhss.map(SingleAssignment(_, op, SplattingRubyNode(rhs)(rhs.span))(ctx.toTextSpan))
            }
            .toList
        }
        case _ => defaultAssignments
      }
    } else {
      defaultAssignments
    }
    DefaultMultipleAssignment(assignments)(ctx.toTextSpan)
  }

  override def visitMultipleLeftHandSide(ctx: RubyParser.MultipleLeftHandSideContext): RubyExpression = {
    val multiLhsItems = ctx.multipleLeftHandSideItem.asScala.map(visit).toList
    val packingLHSNodes = Option(ctx.packingLeftHandSide)
      .map(visit)
      .map {
        case StatementList(statements) => statements
        case x                         => List(x)
      }
      .getOrElse(List.empty)
    val procParameter = Option(ctx.procParameter).map(visit).toList
    val groupedLhs    = Option(ctx.groupedLeftHandSide).map(visit).toList
    val statements    = multiLhsItems ++ packingLHSNodes ++ procParameter ++ groupedLhs
    StatementList(statements)(ctx.toTextSpan)
  }

  override def visitPackingLeftHandSide(ctx: RubyParser.PackingLeftHandSideContext): RubyExpression = {
    val splatNode = Option(ctx.leftHandSide()) match {
      case Some(lhs) => SplattingRubyNode(visit(lhs))(ctx.toTextSpan)
      case None =>
        SplattingRubyNode(MandatoryParameter("_")(ctx.toTextSpan.spanStart("_")))(ctx.toTextSpan.spanStart("*_"))
    }

    Option(ctx.multipleLeftHandSideItem()).map(_.asScala.map(visit).toList).getOrElse(List.empty) match {
      case Nil => splatNode
      case xs  => StatementList(splatNode +: xs)(ctx.toTextSpan)
    }
  }

  override def visitMultipleRightHandSide(ctx: RubyParser.MultipleRightHandSideContext): RubyExpression = {
    val rhsStmts = ctx.children.asScala.collect {
      case x: SplattingRightHandSideContext => visit(x) :: Nil
      case x: OperatorExpressionListContext => x.operatorExpression.asScala.map(visit).toList
    }.flatten

    if rhsStmts.nonEmpty then StatementList(rhsStmts.toList)(ctx.toTextSpan)
    else defaultResult()
  }

  override def visitSplattingArgument(ctx: RubyParser.SplattingArgumentContext): RubyExpression = {
    SplattingRubyNode(visit(ctx.operatorExpression()))(ctx.toTextSpan)
  }

  override def visitAttributeAssignmentExpression(
    ctx: RubyParser.AttributeAssignmentExpressionContext
  ): RubyExpression = {
    val lhs                = visit(ctx.primaryValue())
    val op                 = ctx.op.getText
    val assignmentOperator = ctx.assignmentOperator().getText
    val memberName         = ctx.methodName.getText
    val rhs                = visit(ctx.operatorExpression())
    AttributeAssignment(lhs, op, memberName, assignmentOperator, rhs)(ctx.toTextSpan)
  }

  override def visitSimpleCommand(ctx: RubyParser.SimpleCommandContext): RubyExpression = {
    if (Option(ctx.simpleCommandArgumentList()).map(_.getText).exists(_.startsWith("::"))) {
      val memberName = ctx.simpleCommandArgumentList().getText.stripPrefix("::")
      if (memberName.headOption.exists(_.isUpper)) { // Constant accesses are upper-case 1st letter
        MemberAccess(visit(ctx.methodIdentifier()), "::", memberName)(ctx.toTextSpan)
      } else {
        MemberCall(visit(ctx.methodIdentifier()), "::", memberName, Nil)(ctx.toTextSpan)
      }
    } else if (!ctx.methodIdentifier().isAttrDeclaration) {
      val identifierCtx = ctx.methodIdentifier()
      val arguments     = ctx.simpleCommandArgumentList().arguments.map(visit)
      (identifierCtx.getText, arguments) match {
        case (requireLike, List(argument)) if ImportsPass.ImportCallNames.contains(requireLike) =>
          val isRelative = requireLike == "require_relative" || requireLike == "require_all"
          val isWildcard = requireLike == "require_all"
          RequireCall(visit(identifierCtx), argument, isRelative, isWildcard)(ctx.toTextSpan)
        case ("include", List(argument)) =>
          IncludeCall(visit(identifierCtx), argument)(ctx.toTextSpan)
        case ("raise", List(argument: LiteralExpr)) =>
          val simpleErrorId =
            SimpleIdentifier(Option(s"$builtinPrefix.StandardError"))(argument.span.spanStart("StandardError"))
          val implicitSimpleErrInst = SimpleObjectInstantiation(simpleErrorId, argument :: Nil)(
            argument.span.spanStart(s"StandardError.new(${argument.text})")
          )
          RaiseCall(visit(identifierCtx), implicitSimpleErrInst :: Nil)(ctx.toTextSpan)
        case ("raise", _) =>
          RaiseCall(visit(identifierCtx), arguments)(ctx.toTextSpan)
        case (idAssign, arguments) if idAssign.endsWith("=") =>
          // fixme: This workaround handles a parser ambiguity with method identifiers having `=` and assignments.
          //  The Ruby parser gives precedence to assignments over methods called with this suffix however
          val lhsIdentifier = SimpleIdentifier(None)(identifierCtx.toTextSpan.spanStart(idAssign.stripSuffix("=")))
          val argNode = arguments match {
            case arg :: Nil => arg
            case xs         => ArrayLiteral(xs)(ctx.simpleCommandArgumentList().toTextSpan)
          }
          SingleAssignment(lhsIdentifier, "=", argNode)(ctx.toTextSpan)
        case _ =>
          SimpleCall(visit(identifierCtx), arguments)(ctx.toTextSpan)
      }
    } else {
      FieldsDeclaration(ctx.simpleCommandArgumentList().arguments.map(visit))(ctx.toTextSpan)
    }
  }

  override def visitSuperWithParentheses(ctx: RubyParser.SuperWithParenthesesContext): RubyExpression = {
    val block = Option(ctx.block()).map(visit)
    val arguments =
      Option(ctx.argumentWithParentheses()).map(_.arguments.map(visit)).getOrElse(Nil)
    visitSuperCall(ctx, arguments, block)
  }

  override def visitSuperWithoutParentheses(ctx: RubyParser.SuperWithoutParenthesesContext): RubyExpression = {
    val block     = Option(ctx.block()).map(visit)
    val arguments = Option(ctx.argumentList()).map(_.elements.map(visit)).getOrElse(Nil)
    visitSuperCall(ctx, arguments, block)
  }

  private def visitSuperCall(
    ctx: ParserRuleContext,
    arguments: List[RubyExpression],
    block: Option[RubyExpression]
  ): RubyExpression = {
    val callName = SimpleIdentifier()(ctx.toTextSpan.spanStart("super"))
    block match {
      case Some(body) => SimpleCallWithBlock(callName, arguments, body.asInstanceOf[Block])(ctx.toTextSpan)
      case None       => SimpleCall(callName, arguments)(ctx.toTextSpan)
    }
  }

  override def visitIsDefinedExpression(ctx: RubyParser.IsDefinedExpressionContext): RubyExpression = {
    SimpleCall(visit(ctx.isDefinedKeyword), visit(ctx.expressionOrCommand()) :: Nil)(ctx.toTextSpan)
  }

  override def visitIsDefinedCommand(ctx: RubyParser.IsDefinedCommandContext): RubyExpression = {
    SimpleCall(visit(ctx.isDefinedKeyword), visit(ctx.primaryValue()) :: Nil)(ctx.toTextSpan)
  }

  override def visitMethodCallExpression(ctx: RubyParser.MethodCallExpressionContext): RubyExpression = {
    SimpleCall(visit(ctx.methodOnlyIdentifier()), List())(ctx.toTextSpan)
  }

  override def visitMethodCallWithBlockExpression(
    ctx: RubyParser.MethodCallWithBlockExpressionContext
  ): RubyExpression = {
    ctx.methodIdentifier().getText match {
      case Defines.Proc | Defines.Lambda => ProcOrLambdaExpr(visit(ctx.block()).asInstanceOf[Block])(ctx.toTextSpan)
      case Defines.Loop =>
        DoWhileExpression(
          StaticLiteral(Defines.getBuiltInType(Defines.TrueClass))(ctx.methodIdentifier().toTextSpan.spanStart("true")),
          ctx.block() match {
            case b: RubyParser.DoBlockBlockContext =>
              visit(b.doBlock().bodyStatement())
            case y =>
              logger.warn(s"Unexpected loop block body ${y.getClass}")
              visit(ctx.block())
          }
        )(ctx.toTextSpan)
      case _ =>
        SimpleCallWithBlock(visit(ctx.methodIdentifier()), List(), visit(ctx.block()).asInstanceOf[Block])(
          ctx.toTextSpan
        )
    }
  }

  override def visitLambdaExpression(ctx: RubyParser.LambdaExpressionContext): RubyExpression = {
    val parameters = Option(ctx.lambdaExpressionParameterList()) match {
      case Some(parameterList) => Option(parameterList.blockParameterList()).fold(List())(_.parameters).map(visit)
      case None                => List()
    }

    val body = visit(ctx.block()).asInstanceOf[Block]
    ProcOrLambdaExpr(Block(parameters, body)(ctx.toTextSpan))(ctx.toTextSpan)
  }

  override def visitMethodCallWithParenthesesExpression(
    ctx: RubyParser.MethodCallWithParenthesesExpressionContext
  ): RubyExpression = {
    val callArgs = ctx
      .argumentWithParentheses()
      .arguments
      .map {
        case x: BlockArgumentContext =>
          if Option(x.operatorExpression()).isDefined then visit(x)
          else SimpleIdentifier()(ctx.toTextSpan.spanStart(procParamGen.current.value))
        case x => visit(x)
      }
      .sortBy(x => (x.line, x.column))

    val args =
      if (ctx.argumentWithParentheses().isArrayArgumentList) then
        ArrayLiteral(callArgs)(ctx.toTextSpan.spanStart(callArgs.map(_.span.text).mkString(", "))) :: Nil
      else callArgs

    if (Option(ctx.block()).isDefined) {
      SimpleCallWithBlock(visit(ctx.methodIdentifier()), args, visit(ctx.block()).asInstanceOf[Block])(ctx.toTextSpan)
    } else {
      SimpleCall(visit(ctx.methodIdentifier()), args)(ctx.toTextSpan)
    }
  }

  override def visitYieldExpression(ctx: RubyParser.YieldExpressionContext): RubyExpression = {
    val arguments = Option(ctx.argumentWithParentheses()).iterator
      .flatMap(_.arguments)
      .map(visit)
      .toList
    YieldExpr(arguments)(ctx.toTextSpan)
  }

  override def visitYieldMethodInvocationWithoutParentheses(
    ctx: RubyParser.YieldMethodInvocationWithoutParenthesesContext
  ): RubyExpression = {
    val arguments = ctx.primaryValueListWithAssociation().elements.map(visit).toList
    YieldExpr(arguments)(ctx.toTextSpan)
  }

  override def visitMemberAccessCommand(ctx: RubyParser.MemberAccessCommandContext): RubyExpression = {
    val args = ctx.commandArgument.arguments.map(visit)
    val base = visit(ctx.primary())

    if (ctx.methodName().getText == "new") {
      base match {
        case SingleAssignment(lhs, op, rhs) =>
          // fixme: Parser packaging arguments from a parenthesis-less object instantiation is odd
          val assignSpan = base.span.spanStart(s"${base.span.text}.new")
          val rhsSpan    = rhs.span.spanStart(s"${rhs.span.text}.new")
          SingleAssignment(lhs, op, SimpleObjectInstantiation(rhs, args)(rhsSpan))(assignSpan)
        case _ => SimpleObjectInstantiation(base, args)(ctx.toTextSpan)
      }
    } else {
      val methodName = visit(ctx.methodName())
      MemberCall(base, ".", methodName.text, args)(ctx.toTextSpan)
    }
  }

  override def visitConstantIdentifierVariable(ctx: RubyParser.ConstantIdentifierVariableContext): RubyExpression = {
    SimpleIdentifier()(ctx.toTextSpan)
  }

  override def visitGlobalIdentifierVariable(ctx: RubyParser.GlobalIdentifierVariableContext): RubyExpression = {
    SimpleIdentifier()(ctx.toTextSpan)
  }

  override def visitClassIdentifierVariable(ctx: RubyParser.ClassIdentifierVariableContext): RubyExpression = {
    ClassFieldIdentifier()(ctx.toTextSpan)
  }

  override def visitInstanceIdentifierVariable(ctx: RubyParser.InstanceIdentifierVariableContext): RubyExpression = {
    InstanceFieldIdentifier()(ctx.toTextSpan)
  }

  override def visitLocalIdentifierVariable(ctx: RubyParser.LocalIdentifierVariableContext): RubyExpression = {
    // Sometimes pseudo variables aren't given precedence in the parser, so we double-check here
    ctx.getText match {
      case "nil"       => StaticLiteral(getBuiltInType(Defines.NilClass))(ctx.toTextSpan)
      case "true"      => StaticLiteral(getBuiltInType(Defines.TrueClass))(ctx.toTextSpan)
      case "false"     => StaticLiteral(getBuiltInType(Defines.FalseClass))(ctx.toTextSpan)
      case "public"    => PublicModifier()(ctx.toTextSpan)
      case "private"   => PrivateModifier()(ctx.toTextSpan)
      case "protected" => ProtectedModifier()(ctx.toTextSpan)
      case _           => SimpleIdentifier()(ctx.toTextSpan)
    }
  }

  override def visitClassName(ctx: RubyParser.ClassNameContext): RubyExpression = {
    SimpleIdentifier()(ctx.toTextSpan)
  }

  override def visitMethodIdentifier(ctx: RubyParser.MethodIdentifierContext): RubyExpression = {
    SimpleIdentifier()(ctx.toTextSpan)
  }

  override def visitMethodOnlyIdentifier(ctx: RubyParser.MethodOnlyIdentifierContext): RubyExpression = {
    SimpleIdentifier()(ctx.toTextSpan)
  }

  override def visitIsDefinedKeyword(ctx: RubyParser.IsDefinedKeywordContext): RubyExpression = {
    SimpleIdentifier()(ctx.toTextSpan)
  }

  override def visitLinePseudoVariable(ctx: RubyParser.LinePseudoVariableContext): RubyExpression = {
    SimpleIdentifier(Some(getBuiltInType(Defines.Integer)))(ctx.toTextSpan)
  }

  override def visitFilePseudoVariable(ctx: RubyParser.FilePseudoVariableContext): RubyExpression = {
    SimpleIdentifier(Some(getBuiltInType(Defines.String)))(ctx.toTextSpan)
  }

  override def visitEncodingPseudoVariable(ctx: RubyParser.EncodingPseudoVariableContext): RubyExpression = {
    SimpleIdentifier(Some(getBuiltInType(Defines.Encoding)))(ctx.toTextSpan)
  }

  override def visitSelfPseudoVariable(ctx: RubyParser.SelfPseudoVariableContext): RubyExpression = {
    SelfIdentifier()(ctx.toTextSpan)
  }

  override def visitMemberAccessExpression(ctx: RubyParser.MemberAccessExpressionContext): RubyExpression = {
    val hasArguments = Option(ctx.argumentWithParentheses()).isDefined
    val hasBlock     = Option(ctx.block()).isDefined
    val isClassDecl =
      Option(ctx.primaryValue()).map(_.getText).contains("Class") && Option(ctx.methodName())
        .map(_.getText)
        .contains("new")
    val methodName = ctx.methodName().getText

    if (!hasBlock) {
      val target = visit(ctx.primaryValue())
      if (methodName == "new") {
        if (!hasArguments) {
          return SimpleObjectInstantiation(target, List.empty)(ctx.toTextSpan)
        } else {
          return SimpleObjectInstantiation(target, ctx.argumentWithParentheses().arguments.map(visit))(ctx.toTextSpan)
        }
      } else {
        if (!hasArguments) {
          if (methodName.headOption.exists(_.isUpper)) {
            // This would be a symbol-like member
            return MemberAccess(target, ctx.op.getText, methodName)(ctx.toTextSpan)
          } else {
            // Approximate this as a field-load
            return MemberAccess(target, ctx.op.getText, methodName)(ctx.toTextSpan)
          }
        } else {
          val args = ctx.argumentWithParentheses().arguments.map(visit)
          return MemberCall(target, ctx.op.getText, methodName, args)(ctx.toTextSpan)
        }
      }
    }

    if (hasBlock && isClassDecl) {
      val block = visit(ctx.block()).asInstanceOf[Block]
      return AnonymousClassDeclaration(freshClassName(ctx.primaryValue().toTextSpan), None, block.body)(ctx.toTextSpan)
    } else if (hasBlock) {
      val block  = visit(ctx.block()).asInstanceOf[Block]
      val target = visit(ctx.primaryValue())
      if (methodName == "new") {
        if (!hasArguments) {
          return ObjectInstantiationWithBlock(target, List.empty, block)(ctx.toTextSpan)
        } else {
          val args = ctx.argumentWithParentheses().arguments.map(visit)
          return ObjectInstantiationWithBlock(target, args, block)(ctx.toTextSpan)
        }
      } else {
        return MemberCallWithBlock(
          target,
          ctx.op.getText,
          methodName,
          Option(ctx.argumentWithParentheses())
            .map(_.arguments)
            .getOrElse(List())
            .map(visit),
          visit(ctx.block()).asInstanceOf[Block]
        )(ctx.toTextSpan)
      }
    }

    logger.warn(s"MemberAccessExpression not handled: '${ctx.toTextSpan}'")
    Unknown()(ctx.toTextSpan)
  }

  override def visitConstantVariableReference(ctx: ConstantVariableReferenceContext): RubyExpression = {
    MemberAccess(SelfIdentifier()(ctx.toTextSpan.spanStart(Defines.Self)), "::", ctx.CONSTANT_IDENTIFIER().getText)(
      ctx.toTextSpan
    )
  }

  override def visitIndexingAccessExpression(ctx: RubyParser.IndexingAccessExpressionContext): RubyExpression = {
    IndexAccess(
      visit(ctx.primaryValue()),
      Option(ctx.indexingArgumentList()).map(_.arguments).getOrElse(List()).map(visit)
    )(ctx.toTextSpan)
  }

  override def visitBracketAssignmentExpression(ctx: RubyParser.BracketAssignmentExpressionContext): RubyExpression = {
    val lhsBase = visit(ctx.primaryValue())
    val lhsArgs = Option(ctx.indexingArgumentList()).map(_.arguments).getOrElse(List()).map(visit)

    val lhs = IndexAccess(lhsBase, lhsArgs)(
      ctx.toTextSpan.spanStart(s"${lhsBase.span.text}[${lhsArgs.map(_.span.text).mkString(", ")}]")
    )
    val op  = ctx.assignmentOperator().getText
    val rhs = visit(ctx.operatorExpression())

    if op == "||=" || op == "&&=" then lowerAssignmentOperator(lhs, rhs, op, ctx.toTextSpan)
    else SingleAssignment(lhs, op, rhs)(ctx.toTextSpan)
  }

  /** Lowers the `||=` and `&&=` assignment operators to the respective `.nil?` checks
    */
  private def lowerAssignmentOperator(
    lhs: RubyExpression,
    rhs: RubyExpression,
    op: String,
    span: TextSpan
  ): RubyExpression = {
    val condition  = nilCheckCondition(lhs, op, "nil?", span)
    val thenClause = nilCheckThenClause(lhs, rhs, span)
    nilCheckIfStatement(condition, thenClause, span)
  }

  /** Generates the requried `.nil?` check condition used in the lowering of `||=` and `&&=`
    */
  private def nilCheckCondition(lhs: RubyExpression, op: String, memberName: String, span: TextSpan): RubyExpression = {
    val memberAccess =
      MemberAccess(lhs, op = ".", memberName = "nil?")(span.spanStart(s"${lhs.span.text}.nil?"))
    if op == "||=" then memberAccess
    else UnaryExpression(op = "!", expression = memberAccess)(span.spanStart(s"!${memberAccess.span.text}"))
  }

  /** Generates the assignment and the `thenClause` used in the lowering of `||=` and `&&=`
    */
  private def nilCheckThenClause(lhs: RubyExpression, rhs: RubyExpression, span: TextSpan): RubyExpression = {
    StatementList(List(SingleAssignment(lhs, "=", rhs)(span.spanStart(s"${lhs.span.text} = ${rhs.span.text}"))))(
      span.spanStart(s"${lhs.span.text} = ${rhs.span.text}")
    )
  }

  /** Generates the if statement for the lowering of `||=` and `&&=`
    */
  private def nilCheckIfStatement(
    condition: RubyExpression,
    thenClause: RubyExpression,
    span: TextSpan
  ): RubyExpression = {
    IfExpression(condition = condition, thenClause = thenClause, elsifClauses = List.empty, elseClause = None)(
      span.spanStart(s"if ${condition.span.text} then ${thenClause.span.text} end")
    )
  }

  override def visitBracketedArrayLiteral(ctx: RubyParser.BracketedArrayLiteralContext): RubyExpression = {
    ArrayLiteral(Option(ctx.bracketedArrayElementList()).map(_.elements).getOrElse(List()).map(visit))(ctx.toTextSpan)
  }

  override def visitQuotedNonExpandedStringArrayLiteral(
    ctx: RubyParser.QuotedNonExpandedStringArrayLiteralContext
  ): RubyExpression = {
    val elements = Option(ctx.quotedNonExpandedArrayElementList())
      .map(_.elements)
      .getOrElse(List())
      .map(elemCtx => StaticLiteral(getBuiltInType(Defines.String))(elemCtx.toTextSpan))
    ArrayLiteral(elements)(ctx.toTextSpan)
  }

  override def visitQuotedNonExpandedSymbolArrayLiteral(
    ctx: RubyParser.QuotedNonExpandedSymbolArrayLiteralContext
  ): RubyExpression = {
    val elements = Option(ctx.quotedNonExpandedArrayElementList())
      .map(_.elements)
      .getOrElse(List())
      .map(elemCtx => StaticLiteral(getBuiltInType(Defines.Symbol))(elemCtx.toTextSpan))
    ArrayLiteral(elements)(ctx.toTextSpan)
  }

  override def visitQuotedExpandedSymbolArrayLiteral(
    ctx: RubyParser.QuotedExpandedSymbolArrayLiteralContext
  ): RubyExpression = {
    if (Option(ctx.quotedExpandedArrayElementList).isDefined) {
      ArrayLiteral(ctx.quotedExpandedArrayElementList().elements.map(visit))(ctx.toTextSpan)
    } else {
      ArrayLiteral(List())(ctx.toTextSpan)
    }
  }

  override def visitQuotedExpandedArrayElement(ctx: RubyParser.QuotedExpandedArrayElementContext): RubyExpression = {
    val literalType = findParent(ctx) match {
      case Some(parentCtx) =>
        parentCtx match
          case x: QuotedExpandedStringArrayLiteralContext => Defines.String
          case x: QuotedExpandedSymbolArrayLiteralContext => Defines.Symbol
          case _ => logger.warn("Cannot determine type, defaulting to String"); Defines.String
      case _ => logger.warn("Cannot determine type, defaulting to String"); Defines.String
    }

    if (ctx.hasInterpolation) {
      DynamicLiteral(literalType, ctx.interpolations.map(visit))(ctx.toTextSpan)
    } else {
      StaticLiteral(literalType)(ctx.toTextSpan)
    }
  }

  @tailrec
  private def findParent(ctx: ParserRuleContext): Option[ParserRuleContext] = {
    ctx match {
      case x: QuotedExpandedSymbolArrayLiteralContext => Option(ctx)
      case x: QuotedExpandedStringArrayLiteralContext => Option(ctx)
      case null                                       => Option(ctx)
      case _ =>
        if ctx.parent != null then findParent(ctx.parent.asInstanceOf[ParserRuleContext])
        else None
    }
  }

  override def visitBoundedRangeExpression(ctx: RubyParser.BoundedRangeExpressionContext): RubyExpression = {
    RangeExpression(
      visit(ctx.primaryValue(0)),
      visit(ctx.primaryValue(1)),
      visit(ctx.rangeOperator()).asInstanceOf[RangeOperator]
    )(ctx.toTextSpan)
  }

  override def visitEndlessRangeExpression(ctx: RubyParser.EndlessRangeExpressionContext): RubyExpression = {
    val infinityUpperBound =
      MemberAccess(
        SimpleIdentifier(Option(getBuiltInType(Defines.Float)))(ctx.toTextSpan.spanStart("Float")),
        "::",
        "INFINITY"
      )(ctx.toTextSpan.spanStart("Float::INFINITY"))

    RangeExpression(
      visit(ctx.primaryValue),
      infinityUpperBound,
      visit(ctx.rangeOperator()).asInstanceOf[RangeOperator]
    )(ctx.toTextSpan)
  }

  override def visitBeginlessRangeExpression(ctx: RubyParser.BeginlessRangeExpressionContext): RubyExpression = {
    val lowerBoundInfinity =
      UnaryExpression(
        "-",
        MemberAccess(
          SimpleIdentifier(Option(getBuiltInType(Defines.Float)))(ctx.toTextSpan.spanStart("Float")),
          "::",
          "INFINITY"
        )(ctx.toTextSpan.spanStart("Float::INFINITY"))
      )(ctx.toTextSpan.spanStart("-Float::INFINITY"))

    RangeExpression(
      lowerBoundInfinity,
      visit(ctx.primaryValue),
      visit(ctx.rangeOperator()).asInstanceOf[RangeOperator]
    )(ctx.toTextSpan)
  }

  override def visitRangeOperator(ctx: RubyParser.RangeOperatorContext): RubyExpression = {
    RangeOperator(Option(ctx.DOT2()).isEmpty)(ctx.toTextSpan)
  }

  override def visitHashLiteral(ctx: RubyParser.HashLiteralContext): RubyExpression = {
    HashLiteral(Option(ctx.associationList()).map(_.associations).getOrElse(List()).map(visit))(ctx.toTextSpan)
  }

  override def visitAssociationElement(ctx: RubyParser.AssociationElementContext): RubyExpression = {
    ctx.associationKey().getText match {
      case "if" =>
        Association(SimpleIdentifier()(ctx.toTextSpan.spanStart("if")), visit(ctx.operatorExpression()))(ctx.toTextSpan)
      case _ =>
        Association(visit(ctx.associationKey()), visit(ctx.operatorExpression()))(ctx.toTextSpan)
    }
  }

  override def visitAssociationHashArgument(ctx: RubyParser.AssociationHashArgumentContext): RubyExpression = {
    val identifierName = Option(ctx.LOCAL_VARIABLE_IDENTIFIER()).map(_.getText)

    identifierName match {
      case Some(identName) =>
        SplattingRubyNode(SimpleIdentifier()(ctx.toTextSpan.spanStart(identName)))(ctx.toTextSpan)
      case None =>
        if ctx.LPAREN() == null then SplattingRubyNode(visit(ctx.methodCallsWithParentheses()))(ctx.toTextSpan)
        else SplattingRubyNode(visit(ctx.methodInvocationWithoutParentheses()))(ctx.toTextSpan)
    }
  }

  override def visitModuleDefinition(ctx: RubyParser.ModuleDefinitionContext): RubyExpression = {
    val (nonFieldStmts, fields) = genInitFieldStmts(ctx.bodyStatement())

    val (moduleName, namespaceDecl) = ctx.classPath match {
      case x: NestedClassPathContext =>
        (SimpleIdentifier()(ctx.toTextSpan.spanStart(x.CONSTANT_IDENTIFIER().getText)), namespaceDeclaration(x))
      case _ => (visit(ctx.classPath()), None)
    }

    val memberCall = createBodyMemberCall(moduleName.span.text, ctx.toTextSpan)

    ModuleDeclaration(moduleName, nonFieldStmts, fields, Option(memberCall), namespaceDecl)(ctx.toTextSpan)
  }

  override def visitSingletonClassDefinition(ctx: RubyParser.SingletonClassDefinitionContext): RubyExpression = {
    val baseClass = Option(ctx.commandOrPrimaryValueClass()).map(visit)
    val body      = visit(ctx.bodyStatement()).asInstanceOf[StatementList]

    baseClass match {
      case Some(baseClass) =>
        baseClass match {
          case x: SelfIdentifier =>
            SingletonClassDeclaration(freshClassName(ctx.toTextSpan), Option(baseClass), body)(ctx.toTextSpan)
          case x =>
            val stmts = body.statements.map {
              case x: MethodDeclaration =>
                val memberAccess =
                  MemberAccess(baseClass, ".", x.methodName)(
                    x.span.spanStart(s"${baseClass.span.text}.${x.methodName}")
                  )
                val singletonBlockMethod =
                  SingletonObjectMethodDeclaration(x.methodName, x.parameters, x.body, baseClass)(x.span)
                SingleAssignment(memberAccess, "=", singletonBlockMethod)(
                  ctx.toTextSpan.spanStart(s"${memberAccess.span.text} = ${x.span.text}")
                )
              case x => x
            }

            SingletonStatementList(stmts)(ctx.toTextSpan)
        }
      case None =>
        SingletonClassDeclaration(freshClassName(ctx.toTextSpan), baseClass, body)(ctx.toTextSpan)
    }
  }

  private def findFieldsInMethodDecls(
    methodDecls: List[MethodDeclaration]
  ): List[RubyExpression & RubyFieldIdentifier] = {
    // TODO: Handle case where body of method is not a StatementList
    methodDecls
      .flatMap { x =>
        x.body match {
          case stmtList: StatementList =>
            stmtList.statements.collect { case x: SingleAssignment =>
              x.lhs
            }
          case _ => List.empty
        }
      }
      .collect { case x: (RubyExpression & RubyFieldIdentifier) =>
        x
      }
  }

  def genInitFieldStmts(
    ctxBodyStatement: RubyParser.BodyStatementContext
  ): (RubyExpression, List[RubyExpression & RubyFieldIdentifier]) = {
    val loweredClassDecls = lowerSingletonClassDeclarations(ctxBodyStatement)

    /** Generates SingleAssignment RubyNodes for list of fields and fields found in method decls
      */
    def genSingleAssignmentStmtList(
      fields: List[RubyExpression],
      fieldsInMethodDecls: List[RubyExpression]
    ): List[SingleAssignment] = {
      (fields ++ fieldsInMethodDecls).map { x =>
        SingleAssignment(x, "=", StaticLiteral(getBuiltInType(Defines.NilClass))(x.span.spanStart("nil")))(
          x.span.spanStart(s"${x.span.text} = nil")
        )
      }
    }

    /** Partition RubyFields into InstanceFieldIdentifiers and ClassFieldIdentifiers
      */
    def partitionRubyFields(fields: List[RubyExpression]): (List[RubyExpression], List[RubyExpression]) = {
      fields.partition {
        case _: InstanceFieldIdentifier => true
        case _                          => false
      }
    }

    loweredClassDecls match {
      case stmtList: StatementList =>
        val (rubyFieldIdentifiers, otherStructures) = stmtList.statements.partition {
          case x: (RubyExpression & RubyFieldIdentifier) => true
          case _                                         => false
        }
        val (fieldAssignments, rest) = otherStructures
          .map {
            case x @ SingleAssignment(lhs: SimpleIdentifier, op, rhs) =>
              SingleAssignment(ClassFieldIdentifier()(lhs.span), op, rhs)(x.span)
            case x @ SingleAssignment(lhs: RubyFieldIdentifier, op, rhs) =>
              // Perhaps non-intuitive, but @ fields assigned under a type belong to the singleton class
              SingleAssignment(ClassFieldIdentifier()(lhs.span), op, rhs)(x.span)
            case x => x
          }
          .partition {
            case x: SingleAssignment => true
            case _                   => false
          }

        val (instanceFields, classFields) = partitionRubyFields(rubyFieldIdentifiers)

        val methodDecls = rest.collect { case x: MethodDeclaration =>
          x
        }

        val fieldsInMethodDecls = findFieldsInMethodDecls(methodDecls)

        val (instanceFieldsInMethodDecls, classFieldsInMethodDecls) = partitionRubyFields(fieldsInMethodDecls)

        val initializeMethod = methodDecls.collectFirst { case x if x.methodName == Defines.Initialize => x }

        val initStmtListStatements = genSingleAssignmentStmtList(instanceFields, instanceFieldsInMethodDecls)
        val clinitStmtList = genSingleAssignmentStmtList(classFields, classFieldsInMethodDecls) ++ fieldAssignments

        val bodyMethodStmtList =
          StatementList(initStmtListStatements ++ clinitStmtList)(
            stmtList.span
              .spanStart(initStmtListStatements.map(_.span.text).concat(clinitStmtList.map(_.span.text)).mkString("\n"))
          )

        val bodyMethod = MethodDeclaration(Defines.TypeDeclBody, List.empty, bodyMethodStmtList)(
          stmtList.span.spanStart(s"def <body>\n${bodyMethodStmtList.span.text}\nend")
        )

        val combinedFields = rubyFieldIdentifiers ++ fieldsInMethodDecls ++
          fieldAssignments.collect { case SingleAssignment(lhs: RubyFieldIdentifier, _, _) => lhs }

        (
          StatementList(bodyMethod +: rest)(bodyMethod.span),
          combinedFields.asInstanceOf[List[RubyExpression & RubyFieldIdentifier]]
        )
      case decls => (decls, List.empty)
    }
  }

  /** Detects the alias statements and creates methods that reference the aliased method as a call.
    * @param classBody
    *   the class body node
    * @return
    *   the class body as a statement list.
    */
  def lowerAliasStatementsToMethods(classBody: RubyExpression): StatementList = {

    val classBodyStmts = classBody match {
      case StatementList(stmts) => stmts
      case x                    => List(x)
    }

    val methodParamMap = classBodyStmts.collect { case method: MethodDeclaration =>
      method.methodName -> method.parameters
    }.toMap

    val loweredMethods = classBodyStmts.collect { case alias: AliasStatement =>
      methodParamMap.get(alias.oldName) match {
        case Some(aliasingMethodParams) =>
          val argsCode = aliasingMethodParams.map(_.text).mkString(", ")
          val callCode = s"${alias.oldName}($argsCode)"
          MethodDeclaration(
            alias.newName,
            aliasingMethodParams,
            StatementList(
              SimpleCall(
                SimpleIdentifier(None)(alias.span.spanStart(alias.oldName)),
                aliasingMethodParams.map { x => SimpleIdentifier(None)(alias.span.spanStart(x.span.text)) }
              )(alias.span.spanStart(callCode)) :: Nil
            )(alias.span.spanStart(callCode))
          )(alias.span.spanStart(s"def ${alias.newName}($argsCode)"))
        case None =>
          logger.warn(
            s"Unable to correctly lower aliased method ${alias.oldName}, the result will be in degraded parameter/argument flows"
          )
          MethodDeclaration(
            alias.newName,
            List.empty,
            StatementList(
              SimpleCall(SimpleIdentifier(None)(alias.span.spanStart(alias.oldName)), List.empty)(alias.span) :: Nil
            )(alias.span)
          )(alias.span)
      }
    }

    StatementList(classBodyStmts.filterNot(_.isInstanceOf[AliasStatement]) ++ loweredMethods)(classBody.span)
  }

  /** Moves children nodes not allowed directly under TypeDecl to the `initialize` method
    * @param stmts
    *   \- StatementList for ClassDecl
    * @return
    *   - `initialize` MethodDeclaration with all non-allowed children nodes added
    *   - list of all nodes allowed directly under type decl
    */
  def filterNonAllowedTypeDeclChildren(stmts: StatementList): RubyExpression = {
    val (initMethod, nonInitStmts) = stmts.statements.partition {
      case x: MethodDeclaration if x.methodName == Defines.Initialize => true
      case _                                                          => false
    }

    val (allowedTypeDeclChildren, nonAllowedTypeDeclChildren) = nonInitStmts.partition {
      case _: AllowedTypeDeclarationChild => true
      case _                              => false
    }

    val (bodyMethod, otherTypeDeclChildren) = allowedTypeDeclChildren.partition {
      case x: MethodDeclaration if x.methodName == Defines.TypeDeclBody => true
      case _                                                            => false
    }

    val updatedBodyMethod = bodyMethod
      .asInstanceOf[List[MethodDeclaration]]
      .map { x =>
        val methodDeclStmts =
          StatementList(x.body.asInstanceOf[StatementList].statements ++ nonAllowedTypeDeclChildren)(
            x.span.spanStart(s"${x.body.span.text}${nonAllowedTypeDeclChildren.map(_.span.text).mkString("\n")}")
          )

        MethodDeclaration(x.methodName, x.parameters, methodDeclStmts)(
          x.span.spanStart(s"def <body>\n${methodDeclStmts.span.text}\nend")
        )
      }

    val otherTypeDeclChildrenSpan = otherTypeDeclChildren match {
      case head :: tail => s"\n${head.span.text.concat(tail.map(_.span.text).mkString("\n"))}"
      case _            => ""
    }

    val initMethodSpanText = initMethod match {
      case head :: _ => s"\n${head.span.text}"
      case _         => ""
    }

    StatementList(initMethod ++ otherTypeDeclChildren ++ updatedBodyMethod)(
      stmts.span.spanStart(
        updatedBodyMethod.headOption
          .map(x => x.span.text)
          .getOrElse("") + initMethodSpanText + otherTypeDeclChildrenSpan
      )
    )
  }

  override def visitClassDefinition(ctx: RubyParser.ClassDefinitionContext): RubyExpression = {
    val (nonFieldStmts, fields) = genInitFieldStmts(ctx.bodyStatement())

    val stmts = lowerAliasStatementsToMethods(nonFieldStmts)

    val classBody = filterNonAllowedTypeDeclChildren(stmts)

    val (className, namespaceDecl) = ctx.classPath match {
      case x: NestedClassPathContext =>
        (SimpleIdentifier()(ctx.toTextSpan.spanStart(x.CONSTANT_IDENTIFIER().getText)), namespaceDeclaration(x))
      case _ => (visit(ctx.classPath()), None)
    }

    val memberCall = createBodyMemberCall(className.span.text, ctx.toTextSpan)

    ClassDeclaration(
      className,
      Option(ctx.commandOrPrimaryValueClass()).map(visit),
      classBody,
      fields,
      Option(memberCall),
      namespaceDecl
    )(ctx.toTextSpan)
  }

  private def namespaceDeclaration(ctx: RubyParser.NestedClassPathContext): Option[List[String]] = {
    val namespaces = ctx.classPath match {
      case x: NestedClassPathContext => buildNestedClassPath(ctx.classPath.asInstanceOf[NestedClassPathContext])
      case x: ClassNameContext       => ctx.classPath().getText
    }

    Option(namespaces.split("\\s+").toList)
  }

  private def buildNestedClassPath(ctx: RubyParser.NestedClassPathContext): String = {
    Option(ctx.classPath()) match {
      case Some(cp) =>
        cp match {
          case x: NestedClassPathContext => s"${buildNestedClassPath(x)} ${ctx.CONSTANT_IDENTIFIER()}"
          case x: ClassNameContext       => s"${visit(ctx.classPath).span.text} ${ctx.CONSTANT_IDENTIFIER().getText}"
        }
      case None =>
        ctx.CONSTANT_IDENTIFIER().getText
    }

  }

  private def createBodyMemberCall(name: String, textSpan: TextSpan): TypeDeclBodyCall = {
    TypeDeclBodyCall(
      MemberAccess(SelfIdentifier()(textSpan.spanStart(Defines.Self)), "::", name)(
        textSpan.spanStart(s"${Defines.Self}::$name")
      ),
      name
    )(textSpan.spanStart(s"${Defines.Self}::$name::${Defines.TypeDeclBody}"))
  }

  /** Lowers all MethodDeclaration found in SingletonClassDeclaration to SingletonMethodDeclaration.
    * @param ctx
    *   body context from class definitions
    * @return
    *   RubyNode with lowered MethodDeclarations where required
    */
  private def lowerSingletonClassDeclarations(ctx: RubyParser.BodyStatementContext): RubyExpression = {
    visit(ctx) match {
      case stmtList: StatementList =>
        StatementList(stmtList.statements.flatMap {
          case singletonClassDeclaration: SingletonClassDeclaration =>
            singletonClassDeclaration.baseClass match {
              case Some(selfIdentifier: SelfIdentifier) =>
                singletonClassDeclaration.body match {
                  case singletonClassStmtList: StatementList =>
                    singletonClassStmtList.statements.map {
                      case method: MethodDeclaration =>
                        SingletonMethodDeclaration(selfIdentifier, method.methodName, method.parameters, method.body)(
                          method.span
                        )
                      case nonMethodStatement => nonMethodStatement
                    }
                  case singletonBody => singletonBody :: Nil
                }
              case _ => singletonClassDeclaration.body :: Nil
            }
          case nonStmtListBody => nonStmtListBody :: Nil
        })(stmtList.span)
      case nonStmtList => nonStmtList
    }
  }

  override def visitMethodDefinition(ctx: RubyParser.MethodDefinitionContext): RubyExpression = {
    val params =
      Option(ctx.methodParameterPart().parameterList())
        .fold(List())(_.parameters)
        .map(visit)
        .sortBy(x => (x.span.line, x.span.column))

    MethodDeclaration(ctx.definedMethodName().getText, params, visit(ctx.bodyStatement()))(ctx.toTextSpan)
  }

  override def visitEndlessMethodDefinition(ctx: RubyParser.EndlessMethodDefinitionContext): RubyExpression = {
    val body = visit(ctx.statement()) match {
      case x: StatementList => x
      case x                => StatementList(x :: Nil)(x.span)
    }
    MethodDeclaration(
      ctx.definedMethodName().getText,
      Option(ctx.parameterList()).fold(List())(_.parameters).map(visit),
      body
    )(ctx.toTextSpan)
  }

  override def visitSingletonMethodDefinition(ctx: RubyParser.SingletonMethodDefinitionContext): RubyExpression = {
    SingletonMethodDeclaration(
      visit(ctx.singletonObject()),
      ctx.definedMethodName().getText,
      Option(ctx.methodParameterPart().parameterList()).fold(List())(_.parameters).map(visit),
      visit(ctx.bodyStatement())
    )(ctx.toTextSpan)
  }

  override def visitProcParameter(ctx: RubyParser.ProcParameterContext): RubyExpression = {
    val procParamName =
      Option(ctx.procParameterName()).map(_.LOCAL_VARIABLE_IDENTIFIER()).map(_.getText()).getOrElse(ctx.getText) match {
        case "&" =>
          procParamGen.fresh.value
        case x => x
      }

    ProcParameter(procParamName)(ctx.toTextSpan)
  }

  override def visitHashParameter(ctx: RubyParser.HashParameterContext): RubyExpression = {
    val identifierName = Option(ctx.LOCAL_VARIABLE_IDENTIFIER()).map(_.getText).getOrElse(ctx.getText)
    HashParameter(identifierName)(ctx.toTextSpan)
  }

  override def visitArrayParameter(ctx: RubyParser.ArrayParameterContext): RubyExpression = {
    ArrayParameter(Option(ctx.LOCAL_VARIABLE_IDENTIFIER()).map(_.getText).getOrElse(ctx.getText))(ctx.toTextSpan)
  }

  override def visitOptionalParameter(ctx: RubyParser.OptionalParameterContext): RubyExpression = {
    OptionalParameter(
      ctx.optionalParameterName().LOCAL_VARIABLE_IDENTIFIER().toString,
      visit(ctx.operatorExpression())
    )(ctx.toTextSpan)
  }

  override def visitMandatoryParameter(ctx: RubyParser.MandatoryParameterContext): RubyExpression = {
    MandatoryParameter(ctx.LOCAL_VARIABLE_IDENTIFIER().toString)(ctx.toTextSpan)
  }

  override def visitVariableLeftHandSide(ctx: RubyParser.VariableLeftHandSideContext): RubyExpression = {
    if (Option(ctx.primary()).isEmpty) {
      MandatoryParameter(ctx.toTextSpan.text)(ctx.toTextSpan)
    } else {
      logger.warn(s"Variable LHS without primary expression is not handled: '${ctx.toTextSpan}'")
      Unknown()(ctx.toTextSpan)
    }
  }

  override def visitBodyStatement(ctx: RubyParser.BodyStatementContext): RubyExpression = {
    val body = visit(ctx.compoundStatement())
    val rescueClauses =
      Option(ctx.rescueClause.asScala).fold(List())(_.map(visit).toList).collect { case x: RescueClause => x }
    val elseClause   = Option(ctx.elseClause).map(visit).collect { case x: ElseClause => x }
    val ensureClause = Option(ctx.ensureClause).map(visit).collect { case x: EnsureClause => x }

    if (rescueClauses.isEmpty && elseClause.isEmpty && ensureClause.isEmpty) {
      visit(ctx.compoundStatement())
    } else {
      RescueExpression(body, rescueClauses, elseClause, ensureClause)(ctx.toTextSpan)
    }
  }

  override def visitExceptionClassList(ctx: RubyParser.ExceptionClassListContext): RubyExpression = {
    Option(ctx.multipleRightHandSide()).map(visitMultipleRightHandSide).getOrElse(visit(ctx.operatorExpression()))
  }

  override def visitRescueClause(ctx: RubyParser.RescueClauseContext): RubyExpression = {
    val exceptionClassList = Option(ctx.exceptionClassList).map(visit)
    val variables          = Option(ctx.exceptionVariableAssignment).map(visit)
    val thenClause         = visit(ctx.thenClause)
    RescueClause(exceptionClassList, variables, thenClause)(ctx.toTextSpan)
  }

  override def visitEnsureClause(ctx: RubyParser.EnsureClauseContext): RubyExpression = {
    EnsureClause(visit(ctx.compoundStatement()))(ctx.toTextSpan)
  }

  override def visitCaseWithExpression(ctx: RubyParser.CaseWithExpressionContext): RubyExpression = {
    val expression  = Option(ctx.expressionOrCommand()).map(visit)
    val whenClauses = Option(ctx.whenClause().asScala).fold(List())(_.map(visit).toList)
    val elseClause  = Option(ctx.elseClause()).map(visit)
    CaseExpression(expression, whenClauses, elseClause)(ctx.toTextSpan)
  }

  override def visitCaseWithoutExpression(ctx: RubyParser.CaseWithoutExpressionContext): RubyExpression = {
    val expression  = None
    val whenClauses = Option(ctx.whenClause().asScala).fold(List())(_.map(visit).toList)
    val elseClause  = Option(ctx.elseClause()).map(visit)
    CaseExpression(expression, whenClauses, elseClause)(ctx.toTextSpan)
  }

  override def visitWhenClause(ctx: RubyParser.WhenClauseContext): RubyExpression = {
    val whenArgs = ctx.whenArgument()
    val matchArgs =
      Option(whenArgs.operatorExpressionList()).iterator.flatMap(_.operatorExpression().asScala).map(visit).toList
    val matchSplatArg = Option(whenArgs.splattingArgument()).map(visit)
    val thenClause    = visit(ctx.thenClause())
    WhenClause(matchArgs, matchSplatArg, thenClause)(ctx.toTextSpan)
  }

  override def visitAssociationKey(ctx: RubyParser.AssociationKeyContext): RubyExpression = {
    Option(ctx.operatorExpression()) match {
      case Some(ctx) if ctx.isKeyword => SimpleIdentifier()(ctx.toTextSpan)
      case Some(ctx)                  => visit(ctx)
      case None                       => SimpleIdentifier()(ctx.toTextSpan)
    }
  }

  override def visitAliasStatement(ctx: RubyParser.AliasStatementContext): RubyExpression = {
    AliasStatement(ctx.oldName.getText, ctx.newName.getText)(ctx.toTextSpan)
  }

  override def visitBreakWithoutArguments(ctx: RubyParser.BreakWithoutArgumentsContext): RubyExpression = {
    BreakExpression()(ctx.toTextSpan)
  }

}
