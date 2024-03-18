package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.astcreation.RubyIntermediateAst.*
import io.joern.rubysrc2cpg.parser.AntlrContextHelpers.*
import io.joern.rubysrc2cpg.parser.RubyParser.RangeOperatorContext
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.rubysrc2cpg.passes.Defines.getBuiltInType
import org.antlr.v4.runtime.tree.{ParseTree, RuleNode}
import io.joern.x2cpg.Defines as XDefines;

import scala.jdk.CollectionConverters.*

/** Converts an ANTLR Ruby Parse Tree into the intermediate Ruby AST.
  */
class RubyNodeCreator extends RubyParserBaseVisitor[RubyNode] {

  private var classCounter: Int = 0

  private def tmpClassTemplate(id: Int): String = s"<anon-class-$id>"

  protected def freshClassName(span: TextSpan): SimpleIdentifier = {
    val name = tmpClassTemplate(classCounter)
    classCounter += 1
    SimpleIdentifier(None)(span.spanStart(name))
  }

  private def defaultTextSpan(code: String = ""): TextSpan = TextSpan(None, None, None, None, code)

  override def defaultResult(): RubyNode = Unknown()(defaultTextSpan())

  override protected def shouldVisitNextChild(node: RuleNode, currentResult: RubyNode): Boolean =
    currentResult.isInstanceOf[Unknown]

  override def visit(tree: ParseTree): RubyNode = {
    Option(tree).map(super.visit).getOrElse(defaultResult())
  }

  override def visitProgram(ctx: RubyParser.ProgramContext): RubyNode = {
    visit(ctx.compoundStatement())
  }

  override def visitCompoundStatement(ctx: RubyParser.CompoundStatementContext): RubyNode = {
    StatementList(ctx.getStatements.map(visit))(ctx.toTextSpan)
  }

  override def visitGroupingStatement(ctx: RubyParser.GroupingStatementContext): RubyNode = {
    // When there's only 1 statement, we can use it directly, instead of wrapping it in a StatementList.
    val statements = ctx.compoundStatement().getStatements.map(visit)
    if (statements.size == 1) {
      statements.head
    } else {
      StatementList(statements)(ctx.toTextSpan)
    }
  }

  override def visitStatements(ctx: RubyParser.StatementsContext): RubyNode = {
    StatementList(ctx.statement().asScala.map(visit).toList)(ctx.toTextSpan)
  }

  override def visitWhileExpression(ctx: RubyParser.WhileExpressionContext): RubyNode = {
    val condition = visit(ctx.commandOrPrimaryValue())
    val body      = visit(ctx.doClause())
    WhileExpression(condition, body)(ctx.toTextSpan)
  }

  override def visitUntilExpression(ctx: RubyParser.UntilExpressionContext): RubyNode = {
    val condition = visit(ctx.commandOrPrimaryValue())
    val body      = visit(ctx.doClause())
    UntilExpression(condition, body)(ctx.toTextSpan)
  }

  override def visitIfExpression(ctx: RubyParser.IfExpressionContext): RubyNode = {
    val condition = visit(ctx.commandOrPrimaryValue())
    val thenBody  = visit(ctx.thenClause())
    val elsifs    = ctx.elsifClause().asScala.map(visit).toList
    val elseBody  = Option(ctx.elseClause()).map(visit)
    IfExpression(condition, thenBody, elsifs, elseBody)(ctx.toTextSpan)
  }

  override def visitElsifClause(ctx: RubyParser.ElsifClauseContext): RubyNode = {
    ElsIfClause(visit(ctx.expressionOrCommand()), visit(ctx.thenClause()))(ctx.toTextSpan)
  }

  override def visitElseClause(ctx: RubyParser.ElseClauseContext): RubyNode = {
    ElseClause(visit(ctx.compoundStatement()))(ctx.toTextSpan)
  }

  override def visitUnlessExpression(ctx: RubyParser.UnlessExpressionContext): RubyNode = {
    val condition = visit(ctx.commandOrPrimaryValue())
    val thenBody  = visit(ctx.thenClause())
    val elseBody  = Option(ctx.elseClause()).map(visit)
    UnlessExpression(condition, thenBody, elseBody)(ctx.toTextSpan)
  }

  override def visitForExpression(ctx: RubyParser.ForExpressionContext): RubyNode = {
    val forVariable      = visit(ctx.forVariable())
    val iterableVariable = visit(ctx.commandOrPrimaryValue())
    val doBlock          = visit(ctx.doClause())
    ForExpression(forVariable, iterableVariable, doBlock)(ctx.toTextSpan)
  }

  override def visitForVariable(ctx: RubyParser.ForVariableContext): RubyNode = {
    if (ctx.leftHandSide() != null) visit(ctx.leftHandSide())
    else visit(ctx.multipleLeftHandSide())
  }

  override def visitModifierStatement(ctx: RubyParser.ModifierStatementContext): RubyNode = {
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
      case _ =>
        Unknown()(ctx.toTextSpan)
  }

  override def visitTernaryOperatorExpression(ctx: RubyParser.TernaryOperatorExpressionContext): RubyNode = {
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
  ): RubyNode = {
    val expressions = ctx.primaryValueList().primaryValue().asScala.map(visit).toList
    ReturnExpression(expressions)(ctx.toTextSpan)
  }

  override def visitNumericLiteral(ctx: RubyParser.NumericLiteralContext): RubyNode = {
    if (ctx.hasSign) {
      UnaryExpression(ctx.sign.getText, visit(ctx.unsignedNumericLiteral()))(ctx.toTextSpan)
    } else {
      visit(ctx.unsignedNumericLiteral())
    }
  }

  override def visitUnaryExpression(ctx: RubyParser.UnaryExpressionContext): RubyNode = {
    UnaryExpression(ctx.unaryOperator().getText, visit(ctx.primaryValue()))(ctx.toTextSpan)
  }

  override def visitUnaryMinusExpression(ctx: RubyParser.UnaryMinusExpressionContext): RubyNode = {
    UnaryExpression(ctx.MINUS().getText, visit(ctx.primaryValue()))(ctx.toTextSpan)
  }

  override def visitNotExpressionOrCommand(ctx: RubyParser.NotExpressionOrCommandContext): RubyNode = {
    UnaryExpression(ctx.NOT().getText, visit(ctx.expressionOrCommand()))(ctx.toTextSpan)
  }

  override def visitCommandExpressionOrCommand(ctx: RubyParser.CommandExpressionOrCommandContext): RubyNode = {
    val methodInvocation = visit(ctx.methodInvocationWithoutParentheses())
    if (Option(ctx.EMARK()).isDefined) {
      UnaryExpression(ctx.EMARK().getText, methodInvocation)(ctx.toTextSpan)
    } else {
      methodInvocation
    }
  }

  override def visitHereDocs(ctx: RubyParser.HereDocsContext): RubyNode = {
    HereDocNode(ctx.hereDoc().getText)(ctx.toTextSpan)
  }

  override def visitPowerExpression(ctx: RubyParser.PowerExpressionContext): RubyNode = {
    BinaryExpression(visit(ctx.primaryValue(0)), ctx.powerOperator.getText, visit(ctx.primaryValue(1)))(ctx.toTextSpan)
  }

  override def visitAdditiveExpression(ctx: RubyParser.AdditiveExpressionContext): RubyNode = {
    BinaryExpression(visit(ctx.primaryValue(0)), ctx.additiveOperator().getText, visit(ctx.primaryValue(1)))(
      ctx.toTextSpan
    )
  }

  override def visitMultiplicativeExpression(ctx: RubyParser.MultiplicativeExpressionContext): RubyNode = {
    BinaryExpression(visit(ctx.primaryValue(0)), ctx.multiplicativeOperator().getText, visit(ctx.primaryValue(1)))(
      ctx.toTextSpan
    )
  }

  override def visitLogicalAndExpression(ctx: RubyParser.LogicalAndExpressionContext): RubyNode = {
    BinaryExpression(visit(ctx.primaryValue(0)), ctx.andOperator.getText, visit(ctx.primaryValue(1)))(ctx.toTextSpan)
  }

  override def visitLogicalOrExpression(ctx: RubyParser.LogicalOrExpressionContext): RubyNode = {
    BinaryExpression(visit(ctx.primaryValue(0)), ctx.orOperator.getText, visit(ctx.primaryValue(1)))(ctx.toTextSpan)
  }

  override def visitKeywordAndOrExpressionOrCommand(
    ctx: RubyParser.KeywordAndOrExpressionOrCommandContext
  ): RubyNode = {
    BinaryExpression(visit(ctx.lhs), ctx.binOp.getText, visit(ctx.rhs))(ctx.toTextSpan)
  }

  override def visitShiftExpression(ctx: RubyParser.ShiftExpressionContext): RubyNode = {
    BinaryExpression(visit(ctx.primaryValue(0)), ctx.bitwiseShiftOperator().getText, visit(ctx.primaryValue(1)))(
      ctx.toTextSpan
    )
  }

  override def visitBitwiseAndExpression(ctx: RubyParser.BitwiseAndExpressionContext): RubyNode = {
    BinaryExpression(visit(ctx.primaryValue(0)), ctx.bitwiseAndOperator.getText, visit(ctx.primaryValue(1)))(
      ctx.toTextSpan
    )
  }

  override def visitBitwiseOrExpression(ctx: RubyParser.BitwiseOrExpressionContext): RubyNode = {
    BinaryExpression(visit(ctx.primaryValue(0)), ctx.bitwiseOrOperator().getText, visit(ctx.primaryValue(1)))(
      ctx.toTextSpan
    )
  }

  override def visitRelationalExpression(ctx: RubyParser.RelationalExpressionContext): RubyNode = {
    BinaryExpression(visit(ctx.primaryValue(0)), ctx.relationalOperator().getText, visit(ctx.primaryValue(1)))(
      ctx.toTextSpan
    )
  }

  override def visitEqualityExpression(ctx: RubyParser.EqualityExpressionContext): RubyNode = {
    BinaryExpression(visit(ctx.primaryValue(0)), ctx.equalityOperator().getText, visit(ctx.primaryValue(1)))(
      ctx.toTextSpan
    )
  }

  override def visitDecimalUnsignedLiteral(ctx: RubyParser.DecimalUnsignedLiteralContext): RubyNode = {
    StaticLiteral(getBuiltInType(Defines.Integer))(ctx.toTextSpan)
  }

  override def visitBinaryUnsignedLiteral(ctx: RubyParser.BinaryUnsignedLiteralContext): RubyNode = {
    StaticLiteral(getBuiltInType(Defines.Integer))(ctx.toTextSpan)
  }

  override def visitOctalUnsignedLiteral(ctx: RubyParser.OctalUnsignedLiteralContext): RubyNode = {
    StaticLiteral(getBuiltInType(Defines.Integer))(ctx.toTextSpan)
  }

  override def visitHexadecimalUnsignedLiteral(ctx: RubyParser.HexadecimalUnsignedLiteralContext): RubyNode = {
    StaticLiteral(getBuiltInType(Defines.Integer))(ctx.toTextSpan)
  }

  override def visitFloatWithExponentUnsignedLiteral(
    ctx: RubyParser.FloatWithExponentUnsignedLiteralContext
  ): RubyNode = {
    StaticLiteral(getBuiltInType(Defines.Float))(ctx.toTextSpan)
  }

  override def visitFloatWithoutExponentUnsignedLiteral(
    ctx: RubyParser.FloatWithoutExponentUnsignedLiteralContext
  ): RubyNode = {
    StaticLiteral(getBuiltInType(Defines.Float))(ctx.toTextSpan)
  }

  override def visitPureSymbolLiteral(ctx: RubyParser.PureSymbolLiteralContext): RubyNode = {
    StaticLiteral(getBuiltInType(Defines.Symbol))(ctx.toTextSpan)
  }

  override def visitSingleQuotedSymbolLiteral(ctx: RubyParser.SingleQuotedSymbolLiteralContext): RubyNode = {
    StaticLiteral(getBuiltInType(Defines.Symbol))(ctx.toTextSpan)
  }

  override def visitNilPseudoVariable(ctx: RubyParser.NilPseudoVariableContext): RubyNode = {
    StaticLiteral(getBuiltInType(Defines.NilClass))(ctx.toTextSpan)
  }

  override def visitTruePseudoVariable(ctx: RubyParser.TruePseudoVariableContext): RubyNode = {
    StaticLiteral(getBuiltInType(Defines.TrueClass))(ctx.toTextSpan)
  }

  override def visitFalsePseudoVariable(ctx: RubyParser.FalsePseudoVariableContext): RubyNode = {
    StaticLiteral(getBuiltInType(Defines.FalseClass))(ctx.toTextSpan)
  }

  override def visitSingleQuotedStringExpression(ctx: RubyParser.SingleQuotedStringExpressionContext): RubyNode = {
    if (!ctx.isInterpolated) {
      StaticLiteral(getBuiltInType(Defines.String))(ctx.toTextSpan)
    } else {
      DynamicLiteral(getBuiltInType(Defines.String), ctx.interpolations.map(visit))(ctx.toTextSpan)
    }
  }

  override def visitQuotedNonExpandedStringLiteral(ctx: RubyParser.QuotedNonExpandedStringLiteralContext): RubyNode = {
    StaticLiteral(getBuiltInType(Defines.String))(ctx.toTextSpan)
  }

  override def visitDoubleQuotedStringExpression(ctx: RubyParser.DoubleQuotedStringExpressionContext): RubyNode = {
    if (!ctx.isInterpolated) {
      StaticLiteral(getBuiltInType(Defines.String))(ctx.toTextSpan)
    } else {
      DynamicLiteral(getBuiltInType(Defines.String), ctx.interpolations.map(visit))(ctx.toTextSpan)
    }
  }

  override def visitQuotedExpandedStringLiteral(ctx: RubyParser.QuotedExpandedStringLiteralContext): RubyNode = {
    if (!ctx.isInterpolated) {
      StaticLiteral(getBuiltInType(Defines.String))(ctx.toTextSpan)
    } else {
      DynamicLiteral(getBuiltInType(Defines.String), ctx.interpolations.map(visit))(ctx.toTextSpan)
    }
  }

  override def visitRegularExpressionLiteral(ctx: RubyParser.RegularExpressionLiteralContext): RubyNode = {
    if (ctx.isStatic) {
      StaticLiteral(getBuiltInType(Defines.Regexp))(ctx.toTextSpan)
    } else {
      Unknown()(ctx.toTextSpan)
    }
  }

  override def visitCurlyBracesBlock(ctx: RubyParser.CurlyBracesBlockContext): RubyNode = {
    val parameters = Option(ctx.blockParameter()).fold(List())(_.parameters).map(visit)
    val body       = visit(ctx.compoundStatement())
    Block(parameters, body)(ctx.toTextSpan)
  }

  override def visitDoBlock(ctx: RubyParser.DoBlockContext): RubyNode = {
    val parameters = Option(ctx.blockParameter()).fold(List())(_.parameters).map(visit)
    val body       = visit(ctx.bodyStatement())
    Block(parameters, body)(ctx.toTextSpan)
  }
  override def visitLocalVariableAssignmentExpression(
    ctx: RubyParser.LocalVariableAssignmentExpressionContext
  ): RubyNode = {
    val lhs = visit(ctx.lhs)
    val rhs = visit(ctx.rhs)
    val op  = ctx.assignmentOperator().getText
    SingleAssignment(lhs, op, rhs)(ctx.toTextSpan)
  }

  override def visitMultipleAssignmentStatement(ctx: RubyParser.MultipleAssignmentStatementContext): RubyNode = {

    /** Recursively expand and duplicate splatting nodes so that they line up with what they consume.
      *
      * @param nodes
      *   the splat nodes.
      * @param expandSize
      *   how many more duplicates to create.
      */
    def slurp(nodes: List[RubyNode], expandSize: Int): List[RubyNode] = nodes match {
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
      case x: StatementList => x.statements
      case x                => List(x)
    }
    val rhsNodes = Option(ctx.multipleRightHandSide()).map(visit).getOrElse(defaultResult()) match {
      case x: StatementList => x.statements
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
    MultipleAssignment(assignments)(ctx.toTextSpan)
  }

  override def visitMultipleLeftHandSide(ctx: RubyParser.MultipleLeftHandSideContext): RubyNode = {
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

  override def visitPackingLeftHandSide(ctx: RubyParser.PackingLeftHandSideContext): RubyNode = {
    val splatNode = SplattingRubyNode(visit(ctx.leftHandSide))(ctx.toTextSpan)
    Option(ctx.multipleLeftHandSideItem()).map(_.asScala.map(visit).toList).getOrElse(List.empty) match {
      case Nil => splatNode
      case xs  => StatementList(splatNode +: xs)(ctx.toTextSpan)
    }
  }

  override def visitMultipleRightHandSide(ctx: RubyParser.MultipleRightHandSideContext): RubyNode = {
    val rhsSplatting = Option(ctx.splattingRightHandSide()).map(_.splattingArgument()).map(visit).toList
    Option(ctx.operatorExpressionList())
      .map(x => StatementList(x.operatorExpression().asScala.map(visit).toList ++ rhsSplatting)(ctx.toTextSpan))
      .getOrElse(defaultResult())
  }

  override def visitSplattingArgument(ctx: RubyParser.SplattingArgumentContext): RubyNode = {
    SplattingRubyNode(visit(ctx.operatorExpression()))(ctx.toTextSpan)
  }

  override def visitAttributeAssignmentExpression(ctx: RubyParser.AttributeAssignmentExpressionContext): RubyNode = {
    val lhs        = visit(ctx.primaryValue())
    val op         = ctx.op.getText
    val memberName = ctx.methodName.getText
    val rhs        = visit(ctx.operatorExpression())
    AttributeAssignment(lhs, op, memberName, rhs)(ctx.toTextSpan)
  }

  override def visitSimpleCommand(ctx: RubyParser.SimpleCommandContext): RubyNode = {
    if (Option(ctx.commandArgument()).map(_.getText).exists(_.startsWith("::"))) {
      val memberName = ctx.commandArgument().getText.stripPrefix("::")
      MemberAccess(visit(ctx.methodIdentifier()), "::", memberName)(ctx.toTextSpan)
    } else if (!ctx.methodIdentifier().isAttrDeclaration) {
      val identifierCtx = ctx.methodIdentifier()
      val arguments     = ctx.commandArgument().arguments.map(visit)
      (identifierCtx.getText, arguments) match {
        case ("require", List(argument)) =>
          RequireCall(visit(identifierCtx), argument, false)(ctx.toTextSpan)
        case ("require_relative", List(argument)) =>
          RequireCall(visit(identifierCtx), argument, true)(ctx.toTextSpan)
        case ("include", List(argument)) =>
          IncludeCall(visit(identifierCtx), argument)(ctx.toTextSpan)
        case _ =>
          SimpleCall(visit(identifierCtx), arguments)(ctx.toTextSpan)
      }
    } else {
      FieldsDeclaration(ctx.commandArgument().arguments.map(visit))(ctx.toTextSpan)
    }
  }

  override def visitIsDefinedExpression(ctx: RubyParser.IsDefinedExpressionContext): RubyNode = {
    SimpleCall(visit(ctx.isDefinedKeyword), visit(ctx.expressionOrCommand()) :: Nil)(ctx.toTextSpan)
  }

  override def visitIsDefinedCommand(ctx: RubyParser.IsDefinedCommandContext): RubyNode = {
    SimpleCall(visit(ctx.isDefinedKeyword), visit(ctx.primaryValue()) :: Nil)(ctx.toTextSpan)
  }

  override def visitMethodCallExpression(ctx: RubyParser.MethodCallExpressionContext): RubyNode = {
    SimpleCall(visit(ctx.methodOnlyIdentifier()), List())(ctx.toTextSpan)
  }

  override def visitMethodCallWithBlockExpression(ctx: RubyParser.MethodCallWithBlockExpressionContext): RubyNode = {
    ctx.methodIdentifier().getText match {
      case Defines.Proc | Defines.Lambda => ProcOrLambdaExpr(visit(ctx.block()).asInstanceOf[Block])(ctx.toTextSpan)
      case _ =>
        SimpleCallWithBlock(visit(ctx.methodIdentifier()), List(), visit(ctx.block()).asInstanceOf[Block])(
          ctx.toTextSpan
        )
    }
  }

  override def visitMethodCallWithParenthesesExpression(
    ctx: RubyParser.MethodCallWithParenthesesExpressionContext
  ): RubyNode = {
    if (Option(ctx.block()).isDefined) {
      SimpleCallWithBlock(
        visit(ctx.methodIdentifier()),
        ctx.argumentWithParentheses().arguments.map(visit),
        visit(ctx.block()).asInstanceOf[Block]
      )(ctx.toTextSpan)
    } else {
      SimpleCall(visit(ctx.methodIdentifier()), ctx.argumentWithParentheses().arguments.map(visit))(ctx.toTextSpan)
    }
  }

  override def visitConstantIdentifierVariable(ctx: RubyParser.ConstantIdentifierVariableContext): RubyNode = {
    SimpleIdentifier()(ctx.toTextSpan)
  }

  override def visitGlobalIdentifierVariable(ctx: RubyParser.GlobalIdentifierVariableContext): RubyNode = {
    SimpleIdentifier()(ctx.toTextSpan)
  }

  override def visitClassIdentifierVariable(ctx: RubyParser.ClassIdentifierVariableContext): RubyNode = {
    ClassFieldIdentifier()(ctx.toTextSpan)
  }

  override def visitInstanceIdentifierVariable(ctx: RubyParser.InstanceIdentifierVariableContext): RubyNode = {
    InstanceFieldIdentifier()(ctx.toTextSpan)
  }

  override def visitLocalIdentifierVariable(ctx: RubyParser.LocalIdentifierVariableContext): RubyNode = {
    SimpleIdentifier()(ctx.toTextSpan)
  }

  override def visitClassName(ctx: RubyParser.ClassNameContext): RubyNode = {
    SimpleIdentifier()(ctx.toTextSpan)
  }

  override def visitMethodIdentifier(ctx: RubyParser.MethodIdentifierContext): RubyNode = {
    SimpleIdentifier()(ctx.toTextSpan)
  }

  override def visitIsDefinedKeyword(ctx: RubyParser.IsDefinedKeywordContext): RubyNode = {
    SimpleIdentifier()(ctx.toTextSpan)
  }

  override def visitLinePseudoVariable(ctx: RubyParser.LinePseudoVariableContext): RubyNode = {
    SimpleIdentifier(Some(getBuiltInType(Defines.Integer)))(ctx.toTextSpan)
  }

  override def visitFilePseudoVariable(ctx: RubyParser.FilePseudoVariableContext): RubyNode = {
    SimpleIdentifier(Some(getBuiltInType(Defines.String)))(ctx.toTextSpan)
  }

  override def visitEncodingPseudoVariable(ctx: RubyParser.EncodingPseudoVariableContext): RubyNode = {
    SimpleIdentifier(Some(getBuiltInType(Defines.Encoding)))(ctx.toTextSpan)
  }

  override def visitSelfPseudoVariable(ctx: RubyParser.SelfPseudoVariableContext): RubyNode = {
    SelfIdentifier()(ctx.toTextSpan)
  }

  override def visitMemberAccessExpression(ctx: RubyParser.MemberAccessExpressionContext): RubyNode = {
    val hasArguments = Option(ctx.argumentWithParentheses()).isDefined
    val hasBlock     = Option(ctx.block()).isDefined
    val isClassDecl = Option(ctx.primaryValue()).map(_.getText).contains("Class") && Option(ctx.methodName())
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
          return MemberAccess(target, ctx.op.getText, methodName)(ctx.toTextSpan)
        } else {
          return MemberCall(target, ctx.op.getText, methodName, ctx.argumentWithParentheses().arguments.map(visit))(
            ctx.toTextSpan
          )
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
          return ObjectInstantiationWithBlock(target, ctx.argumentWithParentheses().arguments.map(visit), block)(
            ctx.toTextSpan
          )
        }
      } else {
        return MemberCallWithBlock(
          target,
          ctx.op.getText,
          methodName,
          Option(ctx.argumentWithParentheses()).map(_.arguments).getOrElse(List()).map(visit),
          visit(ctx.block()).asInstanceOf[Block]
        )(ctx.toTextSpan)
      }
    }

    Unknown()(ctx.toTextSpan)
  }

  override def visitIndexingAccessExpression(ctx: RubyParser.IndexingAccessExpressionContext): RubyNode = {
    IndexAccess(
      visit(ctx.primaryValue()),
      Option(ctx.indexingArgumentList()).map(_.arguments).getOrElse(List()).map(visit)
    )(ctx.toTextSpan)
  }

  override def visitBracketedArrayLiteral(ctx: RubyParser.BracketedArrayLiteralContext): RubyNode = {
    ArrayLiteral(Option(ctx.indexingArgumentList()).map(_.arguments).getOrElse(List()).map(visit))(ctx.toTextSpan)
  }

  override def visitQuotedNonExpandedStringArrayLiteral(
    ctx: RubyParser.QuotedNonExpandedStringArrayLiteralContext
  ): RubyNode = {
    val elements = Option(ctx.quotedNonExpandedArrayElementList())
      .map(_.elements)
      .getOrElse(List())
      .map(elemCtx => StaticLiteral(getBuiltInType(Defines.String))(elemCtx.toTextSpan))
    ArrayLiteral(elements)(ctx.toTextSpan)
  }

  override def visitQuotedNonExpandedSymbolArrayLiteral(
    ctx: RubyParser.QuotedNonExpandedSymbolArrayLiteralContext
  ): RubyNode = {
    val elements = Option(ctx.quotedNonExpandedArrayElementList())
      .map(_.elements)
      .getOrElse(List())
      .map(elemCtx => StaticLiteral(getBuiltInType(Defines.Symbol))(elemCtx.toTextSpan))
    ArrayLiteral(elements)(ctx.toTextSpan)
  }

  override def visitRangeExpression(ctx: RubyParser.RangeExpressionContext): RubyNode = {
    RangeExpression(
      visit(ctx.primaryValue(0)),
      visit(ctx.primaryValue(1)),
      visit(ctx.rangeOperator()).asInstanceOf[RangeOperator]
    )(ctx.toTextSpan)
  }

  override def visitRangeOperator(ctx: RubyParser.RangeOperatorContext): RubyNode = {
    RangeOperator(Option(ctx.DOT2()).isEmpty)(ctx.toTextSpan)
  }

  override def visitHashLiteral(ctx: RubyParser.HashLiteralContext): RubyNode = {
    HashLiteral(Option(ctx.associationList()).map(_.associations).getOrElse(List()).map(visit))(ctx.toTextSpan)
  }

  override def visitAssociation(ctx: RubyParser.AssociationContext): RubyNode = {
    ctx.associationKey().getText match {
      case "if" =>
        Association(SimpleIdentifier()(ctx.toTextSpan.spanStart("if")), visit(ctx.operatorExpression()))(ctx.toTextSpan)
      case _ =>
        Association(visit(ctx.associationKey()), visit(ctx.operatorExpression()))(ctx.toTextSpan)
    }
  }

  override def visitModuleDefinition(ctx: RubyParser.ModuleDefinitionContext): RubyNode = {
    ModuleDeclaration(visit(ctx.classPath()), visit(ctx.bodyStatement()))(ctx.toTextSpan)
  }

  override def visitSingletonClassDefinition(ctx: RubyParser.SingletonClassDefinitionContext): RubyNode = {
    SingletonClassDeclaration(
      freshClassName(ctx.toTextSpan),
      Option(ctx.commandOrPrimaryValue()).map(visit),
      visit(ctx.bodyStatement())
    )(ctx.toTextSpan)
  }

  private def findFieldsInMethodDecls(methodDecls: List[MethodDeclaration]): List[RubyNode with RubyFieldIdentifier] = {
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
      .collect { case x: RubyNode with RubyFieldIdentifier =>
        x
      }
  }

  private def genInitFieldStmts(
    ctxBodyStatement: RubyParser.BodyStatementContext
  ): (RubyNode, List[RubyNode with RubyFieldIdentifier]) = {
    val loweredClassDecls = lowerSingletonClassDeclarations(ctxBodyStatement)

    /** Generates SingleAssignment RubyNodes for list of fields and fields found in method decls
      * @param fields
      * @param fieldsInMethodDecls
      * @return
      */
    def genSingleAssignmentStmtList(
      fields: List[RubyNode],
      fieldsInMethodDecls: List[RubyNode]
    ): List[SingleAssignment] = {
      (fields ++ fieldsInMethodDecls).map { x =>
        SingleAssignment(x, "=", StaticLiteral(getBuiltInType(Defines.NilClass))(x.span.spanStart("nil")))(
          x.span.spanStart(s"${x.span.text} = nil")
        )
      }
    }

    /** Partition RubyFields into InstanceFieldIdentifiers and ClassFieldIdentifiers
      * @param fields
      * @return
      */
    def partitionRubyFields(fields: List[RubyNode]): (List[RubyNode], List[RubyNode]) = {
      fields.partition {
        case _: InstanceFieldIdentifier => true
        case _                          => false
      }
    }

    loweredClassDecls match {
      case stmtList: StatementList =>
        val (rubyFieldIdentifiers, rest) = stmtList.statements.partition {
          case x: RubyNode with RubyFieldIdentifier => true
          case _                                    => false
        }

        val (instanceFields, classFields) = partitionRubyFields(rubyFieldIdentifiers)

        val methodDecls = rest.collect { case x: MethodDeclaration =>
          x
        }

        val fieldsInMethodDecls = findFieldsInMethodDecls(methodDecls)

        val (instanceFieldsInMethodDecls, classFieldsInMethodDecls) = partitionRubyFields(fieldsInMethodDecls)

        val initializeMethod = methodDecls.collectFirst { x =>
          x.methodName match
            case "initialize" => x
        }

        val initStmtListStatements = genSingleAssignmentStmtList(instanceFields, instanceFieldsInMethodDecls)
        val clinitStmtList         = genSingleAssignmentStmtList(classFields, classFieldsInMethodDecls)

        val clinitMethod =
          MethodDeclaration(XDefines.StaticInitMethodName, List.empty, StatementList(clinitStmtList)(stmtList.span))(
            stmtList.span
          )

        val updatedStmtList = initializeMethod match {
          case Some(initMethod) =>
            initMethod.body match {
              // TODO: Filter out instance fields that are assigned an initial value in the constructor method. Current
              //  implementation leads to "double" assignment happening when the instance field is assigned a value
              //   where you end up having
              //   <instanceField> = nil; <instanceField> = ...;
              case stmtList: StatementList =>
                val initializers = initStmtListStatements :+ clinitMethod
                StatementList(initializers ++ rest)(stmtList.span)
              case x => x
            }
          case None =>
            val newInitMethod =
              MethodDeclaration("initialize", List.empty, StatementList(initStmtListStatements)(stmtList.span))(
                stmtList.span
              )
            val initializers = newInitMethod :: clinitMethod :: Nil
            StatementList(initializers ++ rest)(stmtList.span)
        }
        val combinedFields = rubyFieldIdentifiers ++ fieldsInMethodDecls

        (updatedStmtList, combinedFields.asInstanceOf[List[RubyNode with RubyFieldIdentifier]])
      case decls => (decls, List.empty)
    }
  }

  override def visitClassDefinition(ctx: RubyParser.ClassDefinitionContext): RubyNode = {
    val (stmts, fields) = genInitFieldStmts(ctx.bodyStatement())

    ClassDeclaration(visit(ctx.classPath()), Option(ctx.commandOrPrimaryValue()).map(visit), stmts, fields)(
      ctx.toTextSpan
    )
  }

  /** Lowers all MethodDeclaration found in SingletonClassDeclaration to SingletonMethodDeclaration.
    * @param ctx
    *   body context from class definitions
    * @return
    *   RubyNode with lowered MethodDeclarations where required
    */
  private def lowerSingletonClassDeclarations(ctx: RubyParser.BodyStatementContext): RubyNode = {
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

  override def visitMethodDefinition(ctx: RubyParser.MethodDefinitionContext): RubyNode = {
    MethodDeclaration(
      ctx.definedMethodName().getText,
      Option(ctx.methodParameterPart().parameterList()).fold(List())(_.parameters).map(visit),
      visit(ctx.bodyStatement())
    )(ctx.toTextSpan)
  }

  override def visitEndlessMethodDefinition(ctx: RubyParser.EndlessMethodDefinitionContext): RubyNode = {
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

  override def visitSingletonMethodDefinition(ctx: RubyParser.SingletonMethodDefinitionContext): RubyNode = {
    SingletonMethodDeclaration(
      visit(ctx.singletonObject()),
      ctx.definedMethodName().getText,
      Option(ctx.methodParameterPart().parameterList()).fold(List())(_.parameters).map(visit),
      visit(ctx.bodyStatement())
    )(ctx.toTextSpan)
  }

  override def visitProcParameter(ctx: RubyParser.ProcParameterContext): RubyNode = {
    ProcParameter(visit(ctx.procParameterName()))(ctx.toTextSpan)
  }

  override def visitHashParameter(ctx: RubyParser.HashParameterContext): RubyNode = {
    HashParameter(Option(ctx.LOCAL_VARIABLE_IDENTIFIER()).map(_.getText).getOrElse(ctx.getText))(ctx.toTextSpan)
  }

  override def visitArrayParameter(ctx: RubyParser.ArrayParameterContext): RubyNode = {
    ArrayParameter(Option(ctx.LOCAL_VARIABLE_IDENTIFIER()).map(_.getText).getOrElse(ctx.getText))(ctx.toTextSpan)
  }

  override def visitOptionalParameter(ctx: RubyParser.OptionalParameterContext): RubyNode = {
    OptionalParameter(
      ctx.optionalParameterName().LOCAL_VARIABLE_IDENTIFIER().toString,
      visit(ctx.operatorExpression())
    )(ctx.toTextSpan)
  }

  override def visitMandatoryParameter(ctx: RubyParser.MandatoryParameterContext): RubyNode = {
    MandatoryParameter(ctx.LOCAL_VARIABLE_IDENTIFIER().toString)(ctx.toTextSpan)
  }

  override def visitVariableLeftHandSide(ctx: RubyParser.VariableLeftHandSideContext): RubyNode = {
    if (Option(ctx.primary()).isEmpty) {
      MandatoryParameter(ctx.toTextSpan.text)(ctx.toTextSpan)
    } else {
      Unknown()(ctx.toTextSpan)
    }
  }

  override def visitBodyStatement(ctx: RubyParser.BodyStatementContext): RubyNode = {
    val body          = visit(ctx.compoundStatement())
    val rescueClauses = Option(ctx.rescueClause.asScala).fold(List())(_.map(visit).toList)
    val elseClause    = Option(ctx.elseClause).map(visit)
    val ensureClause  = Option(ctx.ensureClause).map(visit)

    if (rescueClauses.isEmpty && elseClause.isEmpty && ensureClause.isEmpty) {
      visit(ctx.compoundStatement())
    } else {
      RescueExpression(body, rescueClauses, elseClause, ensureClause)(ctx.toTextSpan)
    }
  }

  override def visitExceptionClassList(ctx: RubyParser.ExceptionClassListContext): RubyNode = {
    // Requires implementing multiple rhs with splatting
    Unknown()(ctx.toTextSpan)
  }

  override def visitRescueClause(ctx: RubyParser.RescueClauseContext): RubyNode = {
    val exceptionClassList = Option(ctx.exceptionClassList).map(visit)
    val elseClause         = Option(ctx.exceptionVariableAssignment).map(visit)
    val thenClause         = visit(ctx.thenClause)
    RescueClause(exceptionClassList, elseClause, thenClause)(ctx.toTextSpan)
  }

  override def visitEnsureClause(ctx: RubyParser.EnsureClauseContext): RubyNode = {
    EnsureClause(visit(ctx.compoundStatement()))(ctx.toTextSpan)
  }

  override def visitCaseWithExpression(ctx: RubyParser.CaseWithExpressionContext): RubyNode = {
    val expression  = Option(ctx.commandOrPrimaryValue()).map(visit)
    val whenClauses = Option(ctx.whenClause().asScala).fold(List())(_.map(visit).toList)
    val elseClause  = Option(ctx.elseClause()).map(visit)
    CaseExpression(expression, whenClauses, elseClause)(ctx.toTextSpan)
  }

  override def visitCaseWithoutExpression(ctx: RubyParser.CaseWithoutExpressionContext): RubyNode = {
    val expression  = None
    val whenClauses = Option(ctx.whenClause().asScala).fold(List())(_.map(visit).toList)
    val elseClause  = Option(ctx.elseClause()).map(visit)
    CaseExpression(expression, whenClauses, elseClause)(ctx.toTextSpan)
  }

  override def visitWhenClause(ctx: RubyParser.WhenClauseContext): RubyNode = {
    val whenArgs = ctx.whenArgument()
    val matchArgs =
      Option(whenArgs.operatorExpressionList()).iterator.flatMap(_.operatorExpression().asScala).map(visit).toList
    val matchSplatArg = Option(whenArgs.splattingArgument()).map(visit)
    val thenClause    = visit(ctx.thenClause())
    WhenClause(matchArgs, matchSplatArg, thenClause)(ctx.toTextSpan)
  }

  override def visitAssociationKey(ctx: RubyParser.AssociationKeyContext): RubyNode = {
    if (Option(ctx.operatorExpression()).isDefined) {
      visit(ctx.operatorExpression())
    } else {
      Unknown()(ctx.toTextSpan)
    }
  }

}
