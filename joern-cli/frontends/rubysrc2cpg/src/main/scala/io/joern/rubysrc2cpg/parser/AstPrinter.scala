package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.astcreation.RubyIntermediateAst.*
import io.joern.rubysrc2cpg.parser.AntlrContextHelpers.*
import io.joern.rubysrc2cpg.parser.RubyParser.{
  CommandWithDoBlockContext,
  ConstantVariableReferenceContext,
  MethodCallExpressionContext
}
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.rubysrc2cpg.utils.FreshNameGenerator
import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.tree.{ParseTree, TerminalNode}

import scala.jdk.CollectionConverters.*

class AstPrinter extends RubyParserBaseVisitor[String] {
  private val ls              = "\n"
  private val rubyNodeCreator = new RubyNodeCreator

  private val classNameGen = FreshNameGenerator(id => s"<anon-class-$id>")

  protected def freshClassName(): String = {
    classNameGen.fresh
  }

  override def defaultResult(): String = ""

  override def visit(tree: ParseTree): String = {
    Option(tree).map(super.visit).getOrElse(defaultResult)
  }

  override def visitProgram(ctx: RubyParser.ProgramContext): String = {
    visit(ctx.compoundStatement())
  }

  override def visitCompoundStatement(ctx: RubyParser.CompoundStatementContext): String = {
    ctx.getStatements.map(visit).mkString(ls)
  }

  override def visitNextWithoutArguments(ctx: RubyParser.NextWithoutArgumentsContext): String = {
    ctx.getText
  }

  override def visitGroupingStatement(ctx: RubyParser.GroupingStatementContext): String = {
    val stmts = ctx.compoundStatement().getStatements.map(visit)
    if (stmts.size == 1) then stmts.head
    else stmts.mkString(ls)
  }

  override def visitStatements(ctx: RubyParser.StatementsContext): String = {
    ctx.statement().asScala.map(visit).toList.mkString(ls)
  }

  override def visitWhileExpression(ctx: RubyParser.WhileExpressionContext): String = {
    val outputSb = new StringBuilder(ctx.WHILE.getText)

    val condition = visit(ctx.expressionOrCommand)
    outputSb.append(s" $condition")

    val body = visit(ctx.doClause())

    if body != "" then outputSb.append(s"$ls$body")

    outputSb.append(s"$ls${ctx.END.getText}").toString
  }

  override def visitUntilExpression(ctx: RubyParser.UntilExpressionContext): String = {
    val condition = visit(ctx.expressionOrCommand())
    val body      = visit(ctx.doClause())

    s"${ctx.UNTIL.getText} $condition $body$ls${ctx.END.getText}"
  }

  override def visitBeginEndExpression(ctx: RubyParser.BeginEndExpressionContext): String = {
    s"${ctx.BEGIN.getText}$ls${visit(ctx.bodyStatement())}$ls${ctx.END.getText}"
  }

  override def visitIfExpression(ctx: RubyParser.IfExpressionContext): String = {
    val outputSb = new StringBuilder(ctx.IF.getText)

    val condition = visit(ctx.expressionOrCommand())
    outputSb.append(s" $condition")

    val thenBody = visit(ctx.thenClause())
    if thenBody != "" then outputSb.append(s"$ls$thenBody")

    val elsifs = ctx.elsifClause().asScala.map(visit).toList
    if elsifs.nonEmpty then outputSb.append(s"$ls${elsifs.mkString(ls)}")

    val elseBody = Option(ctx.elseClause()).map(visit)
    if elseBody.isDefined then outputSb.append(s"$ls${elseBody.get}")

    outputSb.append(s"$ls${ctx.END.getText}")
    outputSb.toString
  }

  override def visitElsifClause(ctx: RubyParser.ElsifClauseContext): String = {
    val outputSb = new StringBuilder(ctx.ELSIF.getText)

    val condition = visit(ctx.expressionOrCommand())
    outputSb.append(s" $condition")

    val thenBody = visit(ctx.thenClause())
    if thenBody != "" then outputSb.append(s"$ls$thenBody")

    outputSb.toString
  }

  override def visitElseClause(ctx: RubyParser.ElseClauseContext): String = {
    val outputSb = new StringBuilder(ctx.ELSE.getText)

    val elseBody = visit(ctx.compoundStatement())
    if elseBody != "" then outputSb.append(s"$ls$elseBody")

    outputSb.toString
  }

  override def visitUnlessExpression(ctx: RubyParser.UnlessExpressionContext): String = {
    val outputSb = new StringBuilder(ctx.UNLESS.getText)

    val condition = visit(ctx.expressionOrCommand())
    outputSb.append(s" $condition")

    val thenBody = visit(ctx.thenClause())
    if thenBody != "" then outputSb.append(s"$ls$thenBody")

    val elseBody = Option(ctx.elseClause()).map(visit)
    if elseBody.isDefined then outputSb.append(s"$ls${elseBody.get}")

    outputSb.append(s"$ls${ctx.END.getText}").toString
  }

  override def visitForExpression(ctx: RubyParser.ForExpressionContext): String = {
    val forVariable      = visit(ctx.forVariable())
    val iterableVariable = visit(ctx.commandOrPrimaryValue())
    val doBlock          = visit(ctx.doClause())

    s"${ctx.FOR.getText} $forVariable ${ctx.IN.getText} $iterableVariable$doBlock$ls${ctx.END.getText}"
  }

  override def visitForVariable(ctx: RubyParser.ForVariableContext): String = {
    if (ctx.leftHandSide() != null) visit(ctx.leftHandSide())
    else visit(ctx.multipleLeftHandSide())
  }

  override def visitModifierStatement(ctx: RubyParser.ModifierStatementContext): String = {
    ctx.statementModifier().getText match
      case "if" =>
        val condition = visit(ctx.expressionOrCommand())
        val thenBody  = visit(ctx.statement())
        s"$thenBody if $condition"
      case "unless" =>
        val condition = visit(ctx.expressionOrCommand())
        val thenBody  = visit(ctx.statement())
        s"$thenBody unless $condition"
      case "while" =>
        val condition = visit(ctx.expressionOrCommand())
        val body      = visit(ctx.statement())
        s"$body while $condition"
      case "until" =>
        val condition = visit(ctx.expressionOrCommand())
        val body      = visit(ctx.statement())
        s"$body until $condition"
      case "rescue" =>
        val body       = visit(ctx.statement())
        val thenClause = visit(ctx.expressionOrCommand())
        s"$body rescue $thenClause"
      case _ => defaultResult()
  }

  override def visitTernaryOperatorExpression(ctx: RubyParser.TernaryOperatorExpressionContext): String = {
    val condition = visit(ctx.operatorExpression(0))
    val thenBody  = visit(ctx.operatorExpression(1))
    val elseBody  = visit(ctx.operatorExpression(2))

    s"$condition ${ctx.QMARK.getText} $thenBody ${ctx.COLON.getText} $elseBody"
  }

  override def visitReturnMethodInvocationWithoutParentheses(
    ctx: RubyParser.ReturnMethodInvocationWithoutParenthesesContext
  ): String = {
    s"return ${ctx.primaryValueList().primaryValue().asScala.map(visit).toList.mkString(ls)}"
  }

  override def visitReturnWithoutArguments(ctx: RubyParser.ReturnWithoutArgumentsContext): String = {
    ctx.getText
  }

  override def visitNumericLiteral(ctx: RubyParser.NumericLiteralContext): String = {
    if ctx.hasSign then s"${ctx.sign.getText}${visit(ctx.unsignedNumericLiteral())}"
    else visit(ctx.unsignedNumericLiteral())
  }

  override def visitUnaryExpression(ctx: RubyParser.UnaryExpressionContext): String = {
    s"${ctx.unaryOperator().getText}${visit(ctx.primaryValue())}"
  }

  override def visitUnaryMinusExpression(ctx: RubyParser.UnaryMinusExpressionContext): String = {
    s"${ctx.MINUS().getText}${visit(ctx.primaryValue())}"
  }

  override def visitNotExpressionOrCommand(ctx: RubyParser.NotExpressionOrCommandContext): String = {
    s"${ctx.NOT().getText} ${visit(ctx.expressionOrCommand())}"
  }

  override def visitCommandExpressionOrCommand(ctx: RubyParser.CommandExpressionOrCommandContext): String = {
    val methodInvocation = visit(ctx.methodInvocationWithoutParentheses())

    if (Option(ctx.EMARK()).isDefined) {
      s"${ctx.EMARK().getText}$methodInvocation"
    } else {
      methodInvocation
    }
  }

  override def visitCommandWithDoBlock(ctx: RubyParser.CommandWithDoBlockContext): String = {
    val name = Option(ctx.methodIdentifier()).orElse(Option(ctx.methodName())).map(visit).getOrElse(defaultResult())
    val arguments = ctx.arguments.map(visit).mkString(" ")
    val block     = visit(ctx.doBlock())

    s"$name $arguments $block"
  }

  override def visitPrimaryOperatorExpression(ctx: RubyParser.PrimaryOperatorExpressionContext): String = {
    rubyNodeCreator.visitPrimaryOperatorExpression(ctx) match {
      case x: BinaryExpression if x.lhs.text.endsWith("=") && x.op == "*" =>
        val newLhs = x.lhs match {
          case call: SimpleCall => SimpleIdentifier(None)(call.span.spanStart(call.span.text.stripSuffix("=")))
          case y =>
            y
        }
        val newRhs = {
          val oldRhsSpan = x.rhs.span
          SplattingRubyNode(x.rhs)(oldRhsSpan.spanStart(s"*${oldRhsSpan.text}"))
        }
        s"${newLhs.span.text} = ${newRhs.span.text}"
      case x => super.visitPrimaryOperatorExpression(ctx)
    }
  }

  override def visitPowerExpression(ctx: RubyParser.PowerExpressionContext): String = {
    val lhs = visit(ctx.primaryValue(0))
    val op  = ctx.powerOperator.getText
    val rhs = visit(ctx.primaryValue(1))

    s"$lhs $op $rhs"
  }

  override def visitAdditiveExpression(ctx: RubyParser.AdditiveExpressionContext): String = {
    val lhs = visit(ctx.primaryValue(0))
    val op  = ctx.additiveOperator().getText
    val rhs = visit(ctx.primaryValue(1))

    s"$lhs $op $rhs"
  }

  override def visitMultiplicativeExpression(ctx: RubyParser.MultiplicativeExpressionContext): String = {
    val lhs = visit(ctx.primaryValue(0))
    val op  = ctx.multiplicativeOperator().getText
    val rhs = visit(ctx.primaryValue(1))

    s"$lhs $op $rhs"
  }

  override def visitLogicalAndExpression(ctx: RubyParser.LogicalAndExpressionContext): String = {
    val lhs = visit(ctx.primaryValue(0))
    val op  = ctx.andOperator.getText
    val rhs = visit(ctx.primaryValue(1))

    s"$lhs $op $rhs"
  }

  override def visitLogicalOrExpression(ctx: RubyParser.LogicalOrExpressionContext): String = {
    val lhs = visit(ctx.primaryValue(0))
    val op  = ctx.orOperator.getText
    val rhs = visit(ctx.primaryValue(1))

    s"$lhs $op $rhs"
  }

  override def visitKeywordAndOrExpressionOrCommand(ctx: RubyParser.KeywordAndOrExpressionOrCommandContext): String = {
    val lhs = visit(ctx.lhs)
    val op  = ctx.binOp.getText
    val rhs = visit(ctx.rhs)

    s"$lhs $op $rhs"
  }

  override def visitShiftExpression(ctx: RubyParser.ShiftExpressionContext): String = {
    val lhs = visit(ctx.primaryValue(0))
    val op  = ctx.bitwiseShiftOperator().getText
    val rhs = visit(ctx.primaryValue(1))

    s"$lhs $op $rhs"
  }

  override def visitBitwiseAndExpression(ctx: RubyParser.BitwiseAndExpressionContext): String = {
    val lhs = visit(ctx.primaryValue(0))
    val op  = ctx.bitwiseAndOperator.getText
    val rhs = visit(ctx.primaryValue(1))

    s"$lhs $op $rhs"
  }

  override def visitBitwiseOrExpression(ctx: RubyParser.BitwiseOrExpressionContext): String = {
    val lhs = visit(ctx.primaryValue(0))
    val op  = ctx.bitwiseOrOperator().getText
    val rhs = visit(ctx.primaryValue(1))

    s"$lhs $op $rhs"
  }

  override def visitRelationalExpression(ctx: RubyParser.RelationalExpressionContext): String = {
    val lhs = visit(ctx.primaryValue(0))
    val op  = ctx.relationalOperator().getText
    val rhs = visit(ctx.primaryValue(1))

    s"$lhs $op $rhs"
  }

  override def visitEqualityExpression(ctx: RubyParser.EqualityExpressionContext): String = {
    val lhs = visit(ctx.primaryValue(0))
    val op  = ctx.equalityOperator().getText
    val rhs = visit(ctx.primaryValue(1))

    s"$lhs $op $rhs"
  }

  override def visitDecimalUnsignedLiteral(ctx: RubyParser.DecimalUnsignedLiteralContext): String = {
    ctx.getText
  }

  override def visitBinaryUnsignedLiteral(ctx: RubyParser.BinaryUnsignedLiteralContext): String = {
    ctx.getText
  }

  override def visitOctalUnsignedLiteral(ctx: RubyParser.OctalUnsignedLiteralContext): String = {
    ctx.getText
  }

  override def visitHexadecimalUnsignedLiteral(ctx: RubyParser.HexadecimalUnsignedLiteralContext): String = {
    ctx.getText
  }

  override def visitFloatWithExponentUnsignedLiteral(
    ctx: RubyParser.FloatWithExponentUnsignedLiteralContext
  ): String = {
    ctx.getText
  }

  override def visitFloatWithoutExponentUnsignedLiteral(
    ctx: RubyParser.FloatWithoutExponentUnsignedLiteralContext
  ): String = {
    ctx.getText
  }

  override def visitPureSymbolLiteral(ctx: RubyParser.PureSymbolLiteralContext): String = {
    ctx.getText
  }

  override def visitSingleQuotedSymbolLiteral(ctx: RubyParser.SingleQuotedSymbolLiteralContext): String = {
    ctx.getText
  }

  override def visitNilPseudoVariable(ctx: RubyParser.NilPseudoVariableContext): String = {
    ctx.getText
  }

  override def visitTruePseudoVariable(ctx: RubyParser.TruePseudoVariableContext): String = {
    ctx.getText
  }

  override def visitFalsePseudoVariable(ctx: RubyParser.FalsePseudoVariableContext): String = {
    ctx.getText
  }

  override def visitSingleQuotedStringExpression(ctx: RubyParser.SingleQuotedStringExpressionContext): String = {
    if (!ctx.isInterpolated) {
      ctx.getText
    } else {
      ctx.children.asScala.map(visit).mkString("")
    }
  }

  override def visitQuotedNonExpandedStringLiteral(ctx: RubyParser.QuotedNonExpandedStringLiteralContext): String = {
    ctx.getText
  }

  override def visitDoubleQuotedStringExpression(ctx: RubyParser.DoubleQuotedStringExpressionContext): String = {
    if (!ctx.isInterpolated) {
      ctx.getText
    } else {
      ctx.children.asScala.map(visit).mkString
    }
  }

  override def visitDoubleQuotedSymbolLiteral(ctx: RubyParser.DoubleQuotedSymbolLiteralContext): String = {
    if (!ctx.isInterpolated) {
      ctx.getText
    } else {
      ctx.children.asScala.map(visit).mkString
    }
  }

  override def visitQuotedExpandedStringLiteral(ctx: RubyParser.QuotedExpandedStringLiteralContext): String = {
    if (!ctx.isInterpolated) {
      ctx.getText
    } else {
      val b = ctx.interpolations
      ctx.children.asScala.map(visit).mkString
    }
  }

  override def visitQuotedExpandedStringArrayLiteral(
    ctx: RubyParser.QuotedExpandedStringArrayLiteralContext
  ): String = {
    val elements =
      if Option(ctx.quotedExpandedArrayElementList()).isDefined then
        ctx.quotedExpandedArrayElementList().elements.map(visit).mkString(" ")
      else ""

    s"${ctx.QUOTED_EXPANDED_STRING_ARRAY_LITERAL_START.getText}$elements${ctx.QUOTED_EXPANDED_STRING_ARRAY_LITERAL_END.getText}"
  }

  override def visitQuotedExpandedSymbolArrayLiteral(
    ctx: RubyParser.QuotedExpandedSymbolArrayLiteralContext
  ): String = {
    val elements =
      if Option(ctx.quotedExpandedArrayElementList()).isDefined then
        ctx.quotedExpandedArrayElementList().elements.map(visit).mkString(" ")
      else ""

    s"${ctx.QUOTED_EXPANDED_SYMBOL_ARRAY_LITERAL_START.getText}$elements${ctx.QUOTED_EXPANDED_SYMBOL_ARRAY_LITERAL_END.getText}"
  }

  override def visitQuotedExpandedArrayElement(ctx: RubyParser.QuotedExpandedArrayElementContext): String = {
    ctx.quotedExpandedArrayElementContent().asScala.flatMap(_.children.asScala.map(visit)).mkString
  }

  override def visitQuotedExpandedLiteralStringContent(
    ctx: RubyParser.QuotedExpandedLiteralStringContentContext
  ): String = {
    Option(ctx.compoundStatement()) match {
      case Some(compoundStatement) =>
        ctx.children.asScala.map(visit).mkString
      case None => ctx.getText
    }
  }

  override def visitRegularExpressionLiteral(ctx: RubyParser.RegularExpressionLiteralContext): String = {
    if (ctx.isStatic) {
      ctx.getText
    } else {
      ctx.children.asScala.map(visit).mkString(" ")
    }
  }

  override def visitQuotedExpandedRegularExpressionLiteral(
    ctx: RubyParser.QuotedExpandedRegularExpressionLiteralContext
  ): String = {
    if (ctx.isStatic) {
      ctx.getText
    } else {
      ctx.children.asScala.map(visit).mkString(" ")
    }
  }

  override def visitQuotedExpandedExternalCommandLiteral(
    ctx: RubyParser.QuotedExpandedExternalCommandLiteralContext
  ): String = {
    val command =
      if ctx.quotedExpandedLiteralStringContent.asScala.nonEmpty then
        ctx.quotedExpandedLiteralStringContent.asScala.flatMap(_.children.asScala.map(visit)).mkString("")
      else ""

    s"${ctx.QUOTED_EXPANDED_EXTERNAL_COMMAND_LITERAL_START.getText}$command${ctx.QUOTED_EXPANDED_EXTERNAL_COMMAND_LITERAL_END.getText}"
  }

  override def visitDoubleQuotedString(ctx: RubyParser.DoubleQuotedStringContext): String = {
    if (!ctx.isInterpolated) {
      ctx.getText
    } else {
      ctx.children.asScala.map(visit).mkString
    }
  }

  override def visitDoubleQuotedStringContent(ctx: RubyParser.DoubleQuotedStringContentContext): String = {
    ctx.children.asScala.map(visit).mkString
  }

  override def visitTerminal(node: TerminalNode): String = {
    node.getText
  }

  override def visitCurlyBracesBlock(ctx: RubyParser.CurlyBracesBlockContext): String = {
    val outputSb = new StringBuilder(ctx.LCURLY.getText)

    val params = Option(ctx.blockParameter()).fold(List())(_.parameters).map(visit)
    if params.nonEmpty then outputSb.append(s"|${params.mkString(",")}|")

    val body = visit(ctx.compoundStatement())
    if body != "" then outputSb.append(s"$ls$body$ls")

    outputSb.append(ctx.RCURLY.getText).toString
  }

  override def visitDoBlock(ctx: RubyParser.DoBlockContext): String = {
    val outputSb = new StringBuilder(ctx.DO.getText)

    val params = Option(ctx.blockParameter()).fold(List())(_.parameters).map(visit).mkString(",")
    if params != "" then outputSb.append(s" |$params|")

    outputSb.append(ls)

    val body = visit(ctx.bodyStatement())
    if body != "" then outputSb.append(s"$body$ls")

    outputSb.append(ctx.END.getText).toString
  }

  override def visitLocalVariableAssignmentExpression(
    ctx: RubyParser.LocalVariableAssignmentExpressionContext
  ): String = {
    val lhs = visit(ctx.lhs)
    val rhs = visit(ctx.rhs)
    val op  = ctx.assignmentOperator().getText

    s"$lhs $op $rhs"
  }

  override def visitMultipleAssignmentStatement(ctx: RubyParser.MultipleAssignmentStatementContext): String = {
    // TODO: fixme - ctx.toTextSpan is being used for individual elements in the building of a MultipleAssignment - needs
    //  to be fixed so that individual elements span texts can be used to build MultipleAssignment on AstPrinter side.
    rubyNodeCreator.visitMultipleAssignmentStatement(ctx).span.text
  }

  override def visitMultipleLeftHandSide(ctx: RubyParser.MultipleLeftHandSideContext): String = {
    val multiLhs   = ctx.multipleLeftHandSideItem.asScala.map(visit).mkString(",")
    val packingLhs = Option(ctx.packingLeftHandSide).map(visit).mkString(",")
    val procParam  = Option(ctx.procParameter).map(visit).mkString(",")
    val groupedLhs = Option(ctx.groupedLeftHandSide).map(visit)

    s"$multiLhs $packingLhs $procParam $groupedLhs"
  }

  override def visitPackingLeftHandSide(ctx: RubyParser.PackingLeftHandSideContext): String = {
    val lhs  = visit(ctx.leftHandSide)
    val rest = Option(ctx.multipleLeftHandSideItem()).map(_.asScala.map(visit).mkString(",")).getOrElse("")

    s"$lhs $rest"
  }

  override def visitMultipleRightHandSide(ctx: RubyParser.MultipleRightHandSideContext): String = {
    val rhsSplatting = Option(ctx.splattingRightHandSide()).map(_.splattingArgument()).map(visit).mkString(",")
    Option(ctx.operatorExpressionList())
      .map(x => s"${x.operatorExpression().asScala.map(visit).mkString(",")} $rhsSplatting")
      .getOrElse(defaultResult())
  }

  override def visitSplattingArgument(ctx: RubyParser.SplattingArgumentContext): String = {
    val operator = Option(ctx.STAR) match {
      case Some(star) => ctx.STAR.getText
      case None       => ctx.STAR2.getText
    }

    s"$operator${visit(ctx.operatorExpression())}"
  }

  override def visitAttributeAssignmentExpression(ctx: RubyParser.AttributeAssignmentExpressionContext): String = {
    val lhs        = visit(ctx.primaryValue())
    val op         = ctx.op.getText
    val memberName = ctx.methodName().getText
    val rhs        = visit(ctx.operatorExpression())

    s"$lhs$op$memberName = $rhs"
  }

  override def visitSimpleCommand(ctx: RubyParser.SimpleCommandContext): String = {
    if (Option(ctx.commandArgument()).map(_.getText).exists(_.startsWith("::"))) {
      val memberName       = ctx.commandArgument().getText.stripPrefix("::")
      val methodIdentifier = visit(ctx.methodIdentifier())
      s"$methodIdentifier::$memberName"
    } else if (!ctx.methodIdentifier().isAttrDeclaration) {
      val identifierCtx = ctx.methodIdentifier()
      val arguments     = ctx.commandArgument().arguments.map(visit)
      (identifierCtx.getText, arguments) match {
        case ("require", List(argument)) =>
          s"require ${arguments.mkString(",")}"
        case ("require_relative", List(argument)) =>
          s"require_relative ${arguments.mkString(",")}"
        case ("require_all", List(argument)) =>
          s"require_all ${arguments.mkString(",")}"
        case ("include", List(argument)) =>
          s"include ${arguments.mkString(",")}"
        case (idAssign, arguments) if idAssign.endsWith("=") =>
          val argNode = arguments match {
            case arg :: Nil => arg
            case xs         => visit(ctx.commandArgument())
          }
          s"$idAssign $argNode"
        case _ =>
          s"${visit(identifierCtx)} ${arguments.mkString(",")}"
      }
    } else {
      s"${ctx.commandArgument.arguments.map(visit).mkString(",")}"
    }
  }

  override def visitSuperWithParentheses(ctx: RubyParser.SuperWithParenthesesContext): String = {
    val block     = Option(ctx.block()).map(visit)
    val arguments = Option(ctx.argumentWithParentheses()).map(_.arguments.map(visit).mkString(",")).getOrElse("")
    visitSuperCall(ctx, s"($arguments)", block)
  }

  override def visitSuperWithoutParentheses(ctx: RubyParser.SuperWithoutParenthesesContext): String = {
    val block     = Option(ctx.block()).map(visit)
    val arguments = Option(ctx.argumentList()).map(_.elements.map(visit).mkString(",")).getOrElse("")
    visitSuperCall(ctx, arguments, block)
  }

  private def visitSuperCall(ctx: ParserRuleContext, arguments: String, block: Option[String]): String = {
    block match {
      case Some(body) => s"super $arguments $body"
      case None       => s"super $arguments"
    }
  }

  override def visitIsDefinedExpression(ctx: RubyParser.IsDefinedExpressionContext): String = {
    val definedKeyword = visit(ctx.isDefinedKeyword)
    val value          = visit(ctx.expressionOrCommand())
    s"$definedKeyword $value"
  }

  override def visitIsDefinedCommand(ctx: RubyParser.IsDefinedCommandContext): String = {
    val definedKeyword = visit(ctx.isDefinedKeyword)
    val value          = visit(ctx.primaryValue())

    s"$definedKeyword $value"
  }

  override def visitMethodCallExpression(ctx: RubyParser.MethodCallExpressionContext): String = {
    val identifier = visit(ctx.methodOnlyIdentifier())
    s"$identifier"
  }

  override def visitMethodCallWithBlockExpression(ctx: RubyParser.MethodCallWithBlockExpressionContext): String = {
    ctx.methodIdentifier().getText match {
      case Defines.Proc | Defines.Lambda => s"${ctx.methodIdentifier().getText} ${visit(ctx.block())}"
      case Defines.Loop =>
        ctx.block() match {
          case b: RubyParser.DoBlockBlockContext =>
            val body = visit(b.doBlock().bodyStatement)
            s"${Defines.Loop} do$ls$body${ls}break if false${ls}end"
          case y =>
            val body = visit(ctx.block())
            s"${Defines.Loop}$ls$body${ls}end"
        }
      case _ =>
        val methodIdent = visit(ctx.methodIdentifier)
        val body        = visit(ctx.block)

        ctx.block() match {
          case x: RubyParser.DoBlockBlockContext => s"$methodIdent $body"
          case y                                 => s"$methodIdent {$ls$body$ls}"
        }
    }
  }

  override def visitLambdaExpression(ctx: RubyParser.LambdaExpressionContext): String = {
    val outputSb = new StringBuilder(ctx.MINUSGT.getText)

    val params = Option(ctx.parameterList()).fold(List())(_.parameters).map(visit).mkString(",")
    val body   = visit(ctx.block())

    if params != "" then outputSb.append(s"($params)")
    if body != "" then outputSb.append(s" $body")

    outputSb.toString
  }

  override def visitMethodCallWithParenthesesExpression(
    ctx: RubyParser.MethodCallWithParenthesesExpressionContext
  ): String = {
    val outputSb = new StringBuilder()

    val identifier = visit(ctx.methodIdentifier())
    outputSb.append(identifier)

    val args = ctx.argumentWithParentheses().arguments.map(visit).mkString(",")
    outputSb.append(s"($args)")

    if Option(ctx.block).isDefined then outputSb.append(s" ${visit(ctx.block)}")
    outputSb.toString
  }

  override def visitYieldExpression(ctx: RubyParser.YieldExpressionContext): String = {
    val outputSb = new StringBuilder(ctx.YIELD.getText)
    val args     = Option(ctx.argumentWithParentheses()).iterator.flatMap(_.arguments).map(visit)
    if args.nonEmpty then outputSb.append(s"(${args.mkString(",")})")

    outputSb.toString
  }

  override def visitYieldMethodInvocationWithoutParentheses(
    ctx: RubyParser.YieldMethodInvocationWithoutParenthesesContext
  ): String = {
    val args = ctx.primaryValueList().primaryValue().asScala.map(visit).mkString(",")
    s"${ctx.YIELD.getText} $args"
  }

  override def visitMemberAccessCommand(ctx: RubyParser.MemberAccessCommandContext): String = {
    val arg        = visit(ctx.commandArgument())
    val methodName = visit(ctx.methodName)
    val base       = visit(ctx.primary())

    s"$base.$methodName $arg"
  }

  override def visitConstantIdentifierVariable(ctx: RubyParser.ConstantIdentifierVariableContext): String = {
    ctx.getText
  }

  override def visitGlobalIdentifierVariable(ctx: RubyParser.GlobalIdentifierVariableContext): String = {
    ctx.getText
  }

  override def visitClassIdentifierVariable(ctx: RubyParser.ClassIdentifierVariableContext): String = {
    ctx.getText
  }

  override def visitInstanceIdentifierVariable(ctx: RubyParser.InstanceIdentifierVariableContext): String = {
    ctx.getText
  }

  override def visitLocalIdentifierVariable(ctx: RubyParser.LocalIdentifierVariableContext): String = {
    ctx.getText
  }

  override def visitClassName(ctx: RubyParser.ClassNameContext): String = {
    ctx.getText
  }

  override def visitMethodIdentifier(ctx: RubyParser.MethodIdentifierContext): String = {
    ctx.getText
  }

  override def visitMethodOnlyIdentifier(ctx: RubyParser.MethodOnlyIdentifierContext): String = {
    ctx.getText
  }

  override def visitIsDefinedKeyword(ctx: RubyParser.IsDefinedKeywordContext): String = {
    ctx.getText
  }

  override def visitLinePseudoVariable(ctx: RubyParser.LinePseudoVariableContext): String = {
    ctx.getText
  }

  override def visitFilePseudoVariable(ctx: RubyParser.FilePseudoVariableContext): String = {
    ctx.getText
  }

  override def visitEncodingPseudoVariable(ctx: RubyParser.EncodingPseudoVariableContext): String = {
    ctx.getText
  }

  override def visitSelfPseudoVariable(ctx: RubyParser.SelfPseudoVariableContext): String = {
    ctx.getText
  }

  override def visitMemberAccessExpression(ctx: RubyParser.MemberAccessExpressionContext): String = {
    val hasArgs    = Option(ctx.argumentWithParentheses()).isDefined
    val hasBlock   = Option(ctx.block()).isDefined
    val methodName = ctx.methodName.getText
    val isClassDecl =
      Option(ctx.primaryValue()).map(_.getText).contains("Class") && Option(ctx.methodName())
        .map(_.getText)
        .contains("new")

    if (!hasBlock) {
      val target = visit(ctx.primaryValue())
      if (methodName == "new") {
        if (!hasArgs) {
          return s"$target.$methodName"
        } else {
          val args = ctx.argumentWithParentheses().arguments.map(visit).mkString(",")
          return s"$target.$methodName($args)"
        }
      } else {
        if (!hasArgs) {
          return s"$target${ctx.op.getText}$methodName"
        } else {
          val args = ctx.argumentWithParentheses().arguments.map(visit).mkString(",")
          return s"$target${ctx.op.getText}$methodName($args)"
        }
      }
    }

    if (hasBlock && isClassDecl) {
      val block = visit(ctx.block())
    } else if (hasBlock) {
      val block  = visit(ctx.block())
      val target = visit(ctx.primaryValue())

      if (methodName == "new") {
        val callStr = s"$target.$methodName"

        if (!hasArgs) {
          return s"$target.$methodName $block"
        } else {
          val args = ctx.argumentWithParentheses().arguments.map(visit).mkString(",")
          return s"$target.$methodName($args) $block"
        }
      } else {
        return s"$target${ctx.op.getText}$methodName $block"
      }
    }

    defaultResult()
  }

  override def visitConstantVariableReference(ctx: RubyParser.ConstantVariableReferenceContext): String = {
    s"self::${ctx.CONSTANT_IDENTIFIER().getText}"
  }

  override def visitIndexingAccessExpression(ctx: RubyParser.IndexingAccessExpressionContext): String = {
    val target = visit(ctx.primaryValue())
    val arg    = Option(ctx.indexingArgumentList()).map(_.arguments).getOrElse(List()).map(visit).mkString(",")

    s"$target${ctx.LBRACK.getText}$arg${ctx.RBRACK.getText}"
  }

  override def visitBracketAssignmentExpression(ctx: RubyParser.BracketAssignmentExpressionContext): String = {
    val op = ctx.assignmentOperator().getText

    if (op != "=") {
      defaultResult()
    }

    val lhsBase = visit(ctx.primaryValue())
    val lhsArgs = Option(ctx.indexingArgumentList()).map(_.arguments).getOrElse(List()).map(visit)
    val rhs     = visit(ctx.operatorExpression())

    s"$lhsBase[${lhsArgs.mkString(",")}] $op ${rhs}"
  }

  override def visitBracketedArrayLiteral(ctx: RubyParser.BracketedArrayLiteralContext): String = {
    val args = Option(ctx.indexingArgumentList()).map(_.arguments).getOrElse(List()).map(visit).mkString(",")
    s"${ctx.LBRACK.getText}$args${ctx.RBRACK.getText}"
  }

  override def visitQuotedNonExpandedStringArrayLiteral(
    ctx: RubyParser.QuotedNonExpandedStringArrayLiteralContext
  ): String = {
    val ctxElements = Option(ctx.quotedNonExpandedArrayElementList())

    val elements = ctxElements
      .map(_.elements)
      .getOrElse(List())
      .map(_.getText)

    val sep =
      if elements.nonEmpty then
        ctxElements
          .map(_.NON_EXPANDED_ARRAY_ITEM_SEPARATOR().asScala)
          .getOrElse(List())
          .map(_.getText)
          .headOption
          .getOrElse("")
      else ""

    val elementsString = elements.mkString(sep)

    s"${ctx.QUOTED_NON_EXPANDED_STRING_ARRAY_LITERAL_START()}$elementsString${ctx.QUOTED_NON_EXPANDED_STRING_ARRAY_LITERAL_END()}"
  }

  override def visitQuotedNonExpandedSymbolArrayLiteral(
    ctx: RubyParser.QuotedNonExpandedSymbolArrayLiteralContext
  ): String = {
    val ctxElements = Option(ctx.quotedNonExpandedArrayElementList())

    val elements = ctxElements
      .map(_.elements)
      .getOrElse(List())
      .map(_.getText)

    val sep =
      if elements.nonEmpty then
        ctxElements
          .map(_.NON_EXPANDED_ARRAY_ITEM_SEPARATOR().asScala)
          .getOrElse(List())
          .map(_.getText)
          .headOption
          .getOrElse("")
      else ""

    val elementsString = elements.mkString(sep)

    s"${ctx.QUOTED_NON_EXPANDED_SYMBOL_ARRAY_LITERAL_START.getText}$elementsString${ctx.QUOTED_NON_EXPANDED_SYMBOL_ARRAY_LITERAL_END.getText}"
  }

  override def visitRangeExpression(ctx: RubyParser.RangeExpressionContext): String = {
    val lowerBound = visit(ctx.primaryValue(0))
    val upperBound = visit(ctx.primaryValue(1))
    val op         = visit(ctx.rangeOperator())

    s"$lowerBound$op$upperBound"
  }

  override def visitRangeOperator(ctx: RubyParser.RangeOperatorContext): String = {
    ctx.getText
  }

  override def visitHashLiteral(ctx: RubyParser.HashLiteralContext): String = {
    val outputSb  = new StringBuilder(ctx.LCURLY.getText)
    val assocList = Option(ctx.associationList()).map(_.associations).getOrElse(List()).map(visit).mkString(",")
    if assocList != "" then outputSb.append(s"$assocList")
    outputSb.append(ctx.RCURLY.getText)
    outputSb.toString
  }

  override def visitAssociationElement(ctx: RubyParser.AssociationElementContext): String = {
    val assocOp = Option(ctx.COLON()) match {
      case Some(colon) => ":"
      case None        => "=>"
    }

    val opExpression = visit(ctx.operatorExpression())

    ctx.associationKey().getText match {
      case "if" =>
        s"${ctx.associationKey().getText}$assocOp $opExpression"
      case _ =>
        val assocKey = visit(ctx.associationKey())
        s"$assocKey$assocOp $opExpression"
    }
  }

  override def visitAssociationHashArgument(ctx: RubyParser.AssociationHashArgumentContext): String = {
    val identifierName = Option(ctx.LOCAL_VARIABLE_IDENTIFIER()).map(_.getText)

    val body = identifierName match {
      case Some(identifierName) => identifierName
      case None =>
        if ctx.LPAREN() == null then visit(ctx.methodCallsWithParentheses())
        else visit(ctx.methodInvocationWithoutParentheses())
    }

    s"${ctx.STAR2().getText}$body"
  }

  override def visitModuleDefinition(ctx: RubyParser.ModuleDefinitionContext): String = {
    val outputSb = new StringBuilder(ctx.MODULE.getText)

    val (nonFieldStmts, fields) = rubyNodeCreator.genInitFieldStmts(ctx.bodyStatement())

    val moduleName = visit(ctx.classPath())

    outputSb.append(s" $moduleName$ls")
    if fields.nonEmpty then outputSb.append(fields.mkString(ls))

    outputSb.append(nonFieldStmts.span.text).append(s"$ls${ctx.END.getText}").toString
  }

  override def visitSingletonClassDefinition(ctx: RubyParser.SingletonClassDefinitionContext): String = {
    val outputSb = new StringBuilder()

    val baseClass = Option(ctx.commandOrPrimaryValueClass()).map(visit)
    val body      = visit(ctx.bodyStatement())

    baseClass match {
      case Some(baseClass) if baseClass == "self" =>
        outputSb.append(ctx.CLASS.getText).append(s" << $baseClass.${freshClassName()}")
        if body != "" then outputSb.append(s"$ls$body")
        outputSb.append(s"$ls${ctx.END.getText}")
        outputSb.toString
      case Some(baseClass) =>
        outputSb.append(ctx.CLASS.getText).append(s" << $baseClass")
        if body != "" then outputSb.append(s"$ls$body")
        outputSb.append(s"$ls${ctx.END.getText}").toString
      case None =>
        s"${ctx.CLASS.getText} ${freshClassName()}$ls$body$ls${ctx.END.getText}"
    }
  }

  override def visitClassDefinition(ctx: RubyParser.ClassDefinitionContext): String = {
    val (nonFieldStmts, fields) = rubyNodeCreator.genInitFieldStmts(ctx.bodyStatement())

    val stmts = rubyNodeCreator.lowerAliasStatementsToMethods(nonFieldStmts)

    val classBody = rubyNodeCreator.filterNonAllowedTypeDeclChildren(stmts)
    val className = visit(ctx.classPath())

    s"class $className$ls${classBody.span.text}${ls}end"
  }

  override def visitMethodDefinition(ctx: RubyParser.MethodDefinitionContext): String = {
    val outputSb = new StringBuilder(s"${ctx.DEF.getText} ${ctx.definedMethodName.getText}")

    val params = Option(ctx.methodParameterPart().parameterList()).fold(List())(_.parameters).map(visit)
    if params.nonEmpty then outputSb.append(s"(${params.mkString(",")})")

    val methodBody = visit(ctx.bodyStatement())
    if methodBody != "" then outputSb.append(s"$ls$methodBody")

    outputSb.append(s"$ls${ctx.END.getText}").toString
  }

  override def visitEndlessMethodDefinition(ctx: RubyParser.EndlessMethodDefinitionContext): String = {
    val outputSb = new StringBuilder(s"${ctx.DEF.getText} ${ctx.definedMethodName.getText}")

    val params = Option(ctx.parameterList()).fold(List())(_.parameters).map(visit)
    if params.nonEmpty then outputSb.append(s"${ctx.LPAREN.getText}${params.mkString(",")}${ctx.RPAREN.getText}")

    outputSb.append(s" ${ctx.EQ.getText}")
    val body = visit(ctx.statement())
    if body != "" then outputSb.append(s" $body")

    outputSb.toString
  }

  override def visitSingletonMethodDefinition(ctx: RubyParser.SingletonMethodDefinitionContext): String = {
    val target     = visit(ctx.singletonObject())
    val op         = ctx.op.getText
    val methodName = ctx.definedMethodName().getText
    val params = Option(ctx.methodParameterPart().parameterList()).fold(List())(_.parameters).map(visit).mkString(",")
    val body   = visit(ctx.bodyStatement())

    if Option(ctx.methodParameterPart().LPAREN()).isDefined then
      s"${ctx.DEF.getText} $target$op$methodName ($params)$ls$body$ls${ctx.END.getText}"
    else s"${ctx.DEF.getText} $target$op$methodName $params$ls$body$ls${ctx.END.getText}"
  }

  override def visitProcParameter(ctx: RubyParser.ProcParameterContext): String = {
    val identName =
      Option(ctx.procParameterName()).map(_.LOCAL_VARIABLE_IDENTIFIER()).map(_.getText).getOrElse(ctx.getText)
    s"${ctx.AMP().getText}$identName"
  }

  override def visitHashParameter(ctx: RubyParser.HashParameterContext): String = {
    val identName = Option(ctx.LOCAL_VARIABLE_IDENTIFIER()).map(_.getText).getOrElse(ctx.getText)
    s"${ctx.STAR2().getText}$identName"
  }

  override def visitArrayParameter(ctx: RubyParser.ArrayParameterContext): String = {
    val identName = Option(ctx.LOCAL_VARIABLE_IDENTIFIER()).map(_.getText).getOrElse(ctx.getText)
    s"${ctx.STAR.getText}$identName"
  }

  override def visitOptionalParameter(ctx: RubyParser.OptionalParameterContext): String = {
    val paramName = ctx.optionalParameterName().LOCAL_VARIABLE_IDENTIFIER().getText
    val value     = visit(ctx.operatorExpression())
    val op =
      if Option(ctx.COLON()).isDefined then ctx.COLON().getText
      else ctx.EQ().getText

    s"$paramName$op$value"
  }

  override def visitMandatoryParameter(ctx: RubyParser.MandatoryParameterContext): String = {
    val paramName = ctx.LOCAL_VARIABLE_IDENTIFIER().getText
    val op = Option(ctx.COLON) match {
      case Some(colon) => ctx.COLON.getText
      case None        => ""
    }

    s"$paramName$op"
  }

  override def visitVariableLeftHandSide(ctx: RubyParser.VariableLeftHandSideContext): String = {
    s"${ctx.getText}"
  }

  override def visitBodyStatement(ctx: RubyParser.BodyStatementContext): String = {
    val body         = visit(ctx.compoundStatement())
    val rescueClause = Option(ctx.rescueClause.asScala).fold(List())(_.map(visit))
    val elseClause   = Option(ctx.elseClause()).map(visit).getOrElse("")
    val ensureClause = Option(ctx.ensureClause).map(visit).getOrElse("")

    if (rescueClause.isEmpty && elseClause.isEmpty && ensureClause.isEmpty) {
      body
    } else {
      val outputSb = new StringBuilder(body)
      if rescueClause.nonEmpty then outputSb.append(s"$ls${rescueClause.mkString(ls)}")
      if elseClause.nonEmpty then outputSb.append(s"$elseClause$ls")
      if ensureClause.nonEmpty then outputSb.append(s"$ensureClause")

      outputSb.toString
    }
  }

  override def visitExceptionClassList(ctx: RubyParser.ExceptionClassListContext): String = {
    Option(ctx.multipleRightHandSide()).map(visitMultipleRightHandSide).getOrElse(visit(ctx.operatorExpression()))
  }

  override def visitRescueClause(ctx: RubyParser.RescueClauseContext): String = {
    val exceptionClassList = Option(ctx.exceptionClassList).map(visit).getOrElse("")
    val variables          = Option(ctx.exceptionVariableAssignment).map(visit).getOrElse("")
    val thenClause         = visit(ctx.thenClause)

    val thenKeyword =
      if Option(ctx.thenClause().THEN()).isDefined then s" ${ctx.thenClause().THEN().getText}"
      else ""

    s"${ctx.RESCUE().getText} $exceptionClassList => $variables $thenKeyword $thenClause".strip()
  }

  override def visitEnsureClause(ctx: RubyParser.EnsureClauseContext): String = {
    val stmt = visit(ctx.compoundStatement)
    s"${ctx.ENSURE().getText}$ls$stmt"
  }

  override def visitCaseWithExpression(ctx: RubyParser.CaseWithExpressionContext): String = {
    val outputSb = new StringBuilder(ctx.CASE.getText)

    val expression = Option(ctx.expressionOrCommand).map(visit)
    if expression.isDefined then outputSb.append(s" ${expression.get}")

    val whenClauses = Option(ctx.whenClause().asScala).fold(List())(_.map(visit))
    if whenClauses.nonEmpty then outputSb.append(s"$ls${whenClauses.mkString}")

    val elseClause = Option(ctx.elseClause()).map(visit)
    if elseClause.isDefined then outputSb.append(s"${elseClause.get}$ls")

    outputSb.append(s"${ctx.END.getText}").toString
  }

  override def visitCaseWithoutExpression(ctx: RubyParser.CaseWithoutExpressionContext): String = {
    val whenClauses = Option(ctx.whenClause().asScala).fold(List())(_.map(visit)).mkString(ls)
    val elseClause  = Option(ctx.elseClause()).map(visit)

    val op =
      if Option(ctx.SEMI()).isDefined then ";"
      else ls
    s"${ctx.CASE().getText}$op$whenClauses$elseClause"
  }

  override def visitWhenClause(ctx: RubyParser.WhenClauseContext): String = {
    val outputSb = new StringBuilder(ctx.WHEN.getText)

    val whenArgs = ctx.whenArgument()
    val matchArgs =
      Option(whenArgs.operatorExpressionList()).iterator.flatMap(_.operatorExpression().asScala).map(visit)
    val matchSplatArg = Option(whenArgs.splattingArgument()).map(visit)
    val thenClause    = visit(ctx.thenClause())

    if matchArgs.nonEmpty then
      val matchArgsStr = matchArgs.mkString(",")
      outputSb.append(s" $matchArgsStr")

      val matchSplatArgStr =
        if matchSplatArg.isDefined then outputSb.append(s", $matchSplatArg")

    if Option(ctx.thenClause().THEN).isDefined then outputSb.append(s" ${ctx.thenClause.THEN.getText}")
    if thenClause != "" then outputSb.append(s"$ls$thenClause")

    outputSb.append(ls).toString
  }

  override def visitAssociationKey(ctx: RubyParser.AssociationKeyContext): String = {
    Option(ctx.operatorExpression()) match {
      case Some(ctx) => visit(ctx)
      case None      => ctx.getText
    }
  }

  override def visitAliasStatement(ctx: RubyParser.AliasStatementContext): String = {
    s"${ctx.ALIAS.getText} ${ctx.oldName.getText} ${ctx.newName.getText}"
  }

  override def visitBreakWithoutArguments(ctx: RubyParser.BreakWithoutArgumentsContext): String = {
    ctx.BREAK.getText
  }
}
