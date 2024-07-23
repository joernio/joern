package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.astcreation.RubyIntermediateAst.{Block, *}
import io.joern.rubysrc2cpg.parser.AntlrContextHelpers.*
import io.joern.rubysrc2cpg.parser.RubyParser.{
  CommandWithDoBlockContext,
  ConstantVariableReferenceContext,
  MethodCallExpressionContext
}
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.rubysrc2cpg.passes.Defines.getBuiltInType
import io.joern.rubysrc2cpg.utils.FreshNameGenerator
import io.joern.x2cpg.Defines as XDefines
import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.tree.{ParseTree, RuleNode}
import org.slf4j.LoggerFactory

import scala.jdk.CollectionConverters.*

class AstPrinter extends RubyParserBaseVisitor[String] {
  private val ls = "\n"

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
    val condition = visit(ctx.expressionOrCommand)
    val body      = visit(ctx.doClause())

    s"while $condition $body\nend"
  }

  override def visitUntilExpression(ctx: RubyParser.UntilExpressionContext): String = {
    val condition = visit(ctx.expressionOrCommand())
    val body      = visit(ctx.doClause())

    s"until $condition $body${ls}end"
  }

  override def visitBeginEndExpression(ctx: RubyParser.BeginEndExpressionContext): String = {
    visit(ctx.bodyStatement())
  }

  override def visitIfExpression(ctx: RubyParser.IfExpressionContext): String = {
    val condition = visit(ctx.expressionOrCommand())
    val thenBody  = visit(ctx.thenClause())
    val elsifs    = ctx.elsifClause().asScala.map(visit).toList
    val elseBody  = Option(ctx.elseClause()).map(visit)

    s"if $condition$ls$thenBody${elsifs.mkString(ls)}$elseBody${ls}end"
  }

  override def visitElsifClause(ctx: RubyParser.ElsifClauseContext): String = {
    val condition = visit(ctx.expressionOrCommand())
    val thenBody  = visit(ctx.thenClause())

    s"elsif$ls$condition$ls$thenBody"
  }

  override def visitElseClause(ctx: RubyParser.ElseClauseContext): String = {
    val elseBody = visit(ctx.compoundStatement())
    s"else$ls$elseBody"
  }

  override def visitUnlessExpression(ctx: RubyParser.UnlessExpressionContext): String = {
    val condition = visit(ctx.expressionOrCommand())
    val thenBody  = visit(ctx.thenClause())
    val elseBody  = Option(ctx.elseClause()).map(visit)

    s"unless $condition$ls$thenBody$elseBody${ls}end"
  }

  override def visitForExpression(ctx: RubyParser.ForExpressionContext): String = {
    val forVariable      = visit(ctx.forVariable())
    val iterableVariable = visit(ctx.commandOrPrimaryValue())
    val doBlock          = visit(ctx.doClause())

    s"for $forVariable in $iterableVariable$doBlock${ls}end"
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

    s"$condition ? $thenBody : $elseBody"
  }

  override def visitReturnMethodInvocationWithoutParentheses(
    ctx: RubyParser.ReturnMethodInvocationWithoutParenthesesContext
  ): String = {
    ctx.primaryValueList().primaryValue().asScala.map(visit).toList.mkString(ls)
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
    s"${ctx.NOT().getText}${visit(ctx.expressionOrCommand())}"
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
    val block     = visit(ctx.doBlock()).asInstanceOf[Block]

    s"$name $arguments $block"
  }

  // TODO: visitPrimaryOperatorExpression

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

  // TODO: Interpolations
  override def visitSingleQuotedStringExpression(ctx: RubyParser.SingleQuotedStringExpressionContext): String = {
    ctx.getText
  }

  override def visitQuotedNonExpandedStringLiteral(ctx: RubyParser.QuotedNonExpandedStringLiteralContext): String = {
    ctx.getText
  }

  // TODO: Interpolatioins
  override def visitDoubleQuotedStringExpression(ctx: RubyParser.DoubleQuotedStringExpressionContext): String = {
    ctx.getText
  }

  // TODO: Interpolations
  override def visitDoubleQuotedSymbolLiteral(ctx: RubyParser.DoubleQuotedSymbolLiteralContext): String = {
    ctx.getText
  }

  // TODO: Interpolations
  override def visitQuotedExpandedStringLiteral(ctx: RubyParser.QuotedExpandedStringLiteralContext): String = {
    ctx.getText
  }

  // TODO: Interpolations
  override def visitRegularExpressionLiteral(ctx: RubyParser.RegularExpressionLiteralContext): String = {
    ctx.getText
  }

  // TODO: Interpolations
  override def visitQuotedExpandedRegularExpressionLiteral(
    ctx: RubyParser.QuotedExpandedRegularExpressionLiteralContext
  ): String = {
    ctx.getText
  }

  override def visitCurlyBracesBlock(ctx: RubyParser.CurlyBracesBlockContext): String = {
    val params = Option(ctx.blockParameter()).fold(List())(_.parameters).map(visit).mkString(",")
    val body   = visit(ctx.compoundStatement())
    s"{ $params $body }"
  }

  override def visitDoBlock(ctx: RubyParser.DoBlockContext): String = {
    val params = Option(ctx.blockParameter()).fold(List())(_.parameters).map(visit).mkString(",")
    val body   = visit(ctx.bodyStatement())

    s"do $params $body end"
  }

  override def visitLocalVariableAssignmentExpression(
    ctx: RubyParser.LocalVariableAssignmentExpressionContext
  ): String = {
    val lhs = visit(ctx.lhs)
    val rhs = visit(ctx.rhs)
    val op  = ctx.assignmentOperator().getText

    s"$lhs $op $rhs"
  }

  // TODO: Implement
  override def visitMultipleAssignmentStatement(ctx: RubyParser.MultipleAssignmentStatementContext): String = {
    ctx.getText
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
    visit(ctx.operatorExpression())
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
          s"${visit(identifierCtx)} (${arguments.mkString(",")})"
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

  // TODO
  override def visitMethodCallWithBlockExpression(ctx: RubyParser.MethodCallWithBlockExpressionContext): String = {
    ctx.methodIdentifier().getText match {
      case Defines.Proc | Defines.Lambda => s"${ctx.methodIdentifier().getText} ${visit(ctx.block())}"
      case Defines.Loop                  => ""
      case _                             => ""
    }
  }

  override def visitLambdaExpression(ctx: RubyParser.LambdaExpressionContext): String = {
    val params = Option(ctx.parameterList()).fold(List())(_.parameters).map(visit).mkString(",")
    val body   = visit(ctx.block())

    val paramList = params match {
      case ""        => ""
      case paramList => s"($paramList)"
    }

    s"-> $paramList $body"
  }

  override def visitMethodCallWithParenthesesExpression(
    ctx: RubyParser.MethodCallWithParenthesesExpressionContext
  ): String = {
    val identifier = visit(ctx.methodIdentifier())
    val args       = ctx.argumentWithParentheses().arguments.map(visit).mkString(",")
    val block = Option(ctx.block()) match {
      case Some(block) => visit(block)
      case None        => ""
    }

    s"$identifier ($args) $block"
  }

  override def visitYieldExpression(ctx: RubyParser.YieldExpressionContext): String = {
    val args = Option(ctx.argumentWithParentheses()).iterator.flatMap(_.arguments).map(visit).mkString(",")
    s"yield ($args)"
  }

  override def visitYieldMethodInvocationWithoutParentheses(
    ctx: RubyParser.YieldMethodInvocationWithoutParenthesesContext
  ): String = {
    val args = ctx.primaryValueList().primaryValue().asScala.map(visit).mkString(",")
    s"yield $args"
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

  // TODO
  override def visitMemberAccessExpression(ctx: RubyParser.MemberAccessExpressionContext): String = {
    ""
  }

  override def visitConstantVariableReference(ctx: RubyParser.ConstantVariableReferenceContext): String = {
    s"self::${ctx.CONSTANT_IDENTIFIER().getText}"
  }

  override def visitIndexingAccessExpression(ctx: RubyParser.IndexingAccessExpressionContext): String = {
    val target = visit(ctx.primaryValue())
    val arg    = Option(ctx.indexingArgumentList()).map(_.arguments).getOrElse(List()).map(visit).mkString(",")

    s"$target[$arg]"
  }

  override def visitBracketedArrayLiteral(ctx: RubyParser.BracketedArrayLiteralContext): String = {
    val args = Option(ctx.indexingArgumentList()).map(_.arguments).getOrElse(List()).map(visit).mkString(",")
    s"[$args]"
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
        ctxElements.map(_.NON_EXPANDED_ARRAY_ITEM_SEPARATOR().asScala).getOrElse(List()).map(_.getText).head
      else ""

    val elementsString = elements.mkString(sep)

    s"${ctx.QUOTED_NON_EXPANDED_STRING_ARRAY_LITERAL_START()} $elements ${ctx.QUOTED_NON_EXPANDED_STRING_ARRAY_LITERAL_END()}"
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
    val assocList = Option(ctx.associationList()).map(_.associations).getOrElse(List()).map(visit).mkString(",")
    s"{ $assocList }"
  }

  override def visitAssociationElement(ctx: RubyParser.AssociationElementContext): String = {
    val assocOp = Option(ctx.COLON()) match {
      case Some(colon) => ":"
      case None        => "=>"
    }

    val opExpression = visit(ctx.operatorExpression())

    ctx.associationKey().getText match {
      case "if" =>
        s"${ctx.associationKey().getText} $assocOp $opExpression"
      case _ =>
        val assocKey = visit(ctx.associationKey())
        s"$assocKey $assocOp $opExpression"
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

  // TODO
  override def visitModuleDefinition(ctx: RubyParser.ModuleDefinitionContext): String = {
    ""
  }

  // TODO
  override def visitSingletonClassDefinition(ctx: RubyParser.SingletonClassDefinitionContext): String = {
    ""
  }

  // TODO
  override def visitClassDefinition(ctx: RubyParser.ClassDefinitionContext): String = {
    ""
  }

  override def visitMethodDefinition(ctx: RubyParser.MethodDefinitionContext): String = {
    val params     = Option(ctx.methodParameterPart().parameterList()).fold(List())(_.parameters).map(visit)
    val methodBody = visit(ctx.bodyStatement())

    if params.nonEmpty then s"def ${ctx.definedMethodName().getText} (${params.mkString(",")})$ls$methodBody${ls}end"
    else s"def ${ctx.definedMethodName().getText}$ls$methodBody${ls}end"
  }

  override def visitEndlessMethodDefinition(ctx: RubyParser.EndlessMethodDefinitionContext): String = {
    val body = visit(ctx.statement())
    s"def ${ctx.definedMethodName().getText}$ls$body"
  }

  override def visitSingletonMethodDefinition(ctx: RubyParser.SingletonMethodDefinitionContext): String = {
    val target     = visit(ctx.singletonObject())
    val op         = ctx.op.getText
    val methodName = ctx.definedMethodName().getText
    val params = Option(ctx.methodParameterPart().parameterList()).fold(List())(_.parameters).map(visit).mkString(",")
    val body   = visit(ctx.bodyStatement())

    if Option(ctx.methodParameterPart().LPAREN()).isDefined then s"def $target$op$methodName ($params)$ls$body${ls}end"
    else s"def $target$op$methodName $params$ls$body${ls}end"
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
    s"$paramName"
  }

  override def visitVariableLeftHandSide(ctx: RubyParser.VariableLeftHandSideContext): String = {
    s"${ctx.getText}"
  }

  // TODO
  override def visitBodyStatement(ctx: RubyParser.BodyStatementContext): String = {
    val body         = visit(ctx.compoundStatement())
    val rescueClause = Option(ctx.rescueClause.asScala).fold(List())(_.map(visit)).mkString(ls)
    ""
  }

  override def visitExceptionClassList(ctx: RubyParser.ExceptionClassListContext): String = {
    Option(ctx.multipleRightHandSide()).map(visitMultipleRightHandSide).getOrElse(visit(ctx.operatorExpression()))
  }

  override def visitRescueClause(ctx: RubyParser.RescueClauseContext): String = {
    val exceptionClassList = Option(ctx.exceptionClassList).map(visit)
    val variables          = Option(ctx.exceptionVariableAssignment).map(visit)
    val thenClause         = visit(ctx.thenClause)

    val thenKeyword =
      if Option(ctx.thenClause().THEN()).isDefined then s"${ctx.thenClause().THEN().getText}"
      else ""

    s"${ctx.RESCUE().getText} $exceptionClassList $variables $thenKeyword $thenClause"
  }

  override def visitEnsureClause(ctx: RubyParser.EnsureClauseContext): String = {
    val stmt = visit(ctx.compoundStatement)
    s"${ctx.ENSURE().getText}$stmt"
  }

  override def visitCaseWithExpression(ctx: RubyParser.CaseWithExpressionContext): String = {
    val expression  = Option(ctx.expressionOrCommand).map(visit)
    val whenClauses = Option(ctx.whenClause().asScala).fold(List())(_.map(visit)).mkString(ls)
    val elseClause  = Option(ctx.elseClause()).map(visit)
    val op =
      if Option(ctx.SEMI()).isDefined then ";"
      else ls

    s"${ctx.CASE().getText}$ls$expression$op$whenClauses$elseClause${ctx.END().getText}"
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
    val whenArgs = ctx.whenArgument()
    val matchArgs =
      Option(whenArgs.operatorExpressionList()).iterator.flatMap(_.operatorExpression().asScala).map(visit)
    val matchSplatArg = Option(whenArgs.splattingArgument()).map(visit)
    val thenClause    = visit(ctx.thenClause())

    if matchArgs.nonEmpty then
      val matchArgsStr = matchArgs.mkString(",")
      val matchSplatArgStr =
        if matchSplatArg.isDefined then s", $matchSplatArg"
        else ""
      s"${ctx.WHEN.getText} $matchArgsStr$matchSplatArgStr"
    else s"${ctx.WHEN.getText} $matchSplatArg"
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
