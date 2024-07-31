package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.astcreation.RubyIntermediateAst.{ProcedureDeclaration, TextSpan, TypeDeclaration}
import io.joern.rubysrc2cpg.parser.RubyParser.*
import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.misc.Interval
import org.slf4j.LoggerFactory

import scala.jdk.CollectionConverters.*

object AntlrContextHelpers {

  private val logger = LoggerFactory.getLogger(getClass)

  sealed implicit class ParserRuleContextHelper(ctx: ParserRuleContext) {
    def toTextSpan: TextSpan = {
      // The stopIndex could precede startIndex for rules that do not consume anything, cf. `getStop`.
      // We need to make sure this doesn't happen when building the `text` field.
      val startIndex = ctx.getStart.getStartIndex
      val stopIndex  = math.max(startIndex, ctx.getStop.getStopIndex)

      val offset = ctx match {
        case x: MethodDefinitionContext => Option(ctx.start.getStartIndex, ctx.stop.getStopIndex + 1)
        case x: ClassDefinitionContext  => Option(ctx.start.getStartIndex, ctx.stop.getStopIndex + 1)
        case x: ModuleDefinitionContext => Option(ctx.start.getStartIndex, ctx.stop.getStopIndex + 1)
        case _                          => None
      }

      TextSpan(
        line = Option(ctx.getStart.getLine),
        column = Option(ctx.getStart.getCharPositionInLine),
        lineEnd = Option(ctx.getStop.getLine),
        columnEnd = Option(ctx.getStop.getCharPositionInLine),
        offset = offset,
        text = ctx.getStart.getInputStream.getText(new Interval(startIndex, stopIndex))
      )
    }

    /** @return
      *   true if this token's text is the same as a keyword, false if otherwise.
      */
    def isKeyword: Boolean = {
      // See RubyParser for why the bounds are used
      val minBound = 19
      val maxBound = 56
      val typ      = ctx.start.getType
      typ >= minBound && typ <= maxBound
    }
  }

  sealed implicit class CompoundStatementContextHelper(ctx: CompoundStatementContext) {
    def getStatements: List[ParserRuleContext] =
      Option(ctx.statements()).map(_.statement().asScala.toList).getOrElse(List())
  }

  sealed implicit class NumericLiteralContextHelper(ctx: NumericLiteralContext) {
    def hasSign: Boolean = Option(ctx.sign).isDefined
  }

  sealed implicit class SingleOrDoubleQuotedStringContextHelper(ctx: SingleOrDoubleQuotedStringContext) {
    def isInterpolated: Boolean = Option(ctx.doubleQuotedString()).exists(_.isInterpolated)
  }

  sealed implicit class DoubleQuotedStringContextHelper(ctx: DoubleQuotedStringContext) {
    def interpolations: List[ParserRuleContext] = ctx
      .doubleQuotedStringContent()
      .asScala
      .filter(ctx => Option(ctx.compoundStatement()).isDefined)
      .map(ctx => ctx.compoundStatement())
      .toList
    def isInterpolated: Boolean = interpolations.nonEmpty
  }

  sealed implicit class QuotedExpandedStringLiteralContextHelper(ctx: QuotedExpandedStringLiteralContext) {
    def interpolations: List[ParserRuleContext] = ctx
      .quotedExpandedLiteralStringContent()
      .asScala
      .filter(ctx => Option(ctx.compoundStatement()).isDefined)
      .map(ctx => ctx.compoundStatement())
      .toList
    def isInterpolated: Boolean = interpolations.nonEmpty
  }

  sealed implicit class DoubleQuotedStringExpressionContextHelper(ctx: DoubleQuotedStringExpressionContext) {
    def interpolations: List[ParserRuleContext] = ctx.doubleQuotedString().interpolations ++ ctx
      .singleOrDoubleQuotedString()
      .asScala
      .filter(_.isInterpolated)
      .flatMap(_.doubleQuotedString().interpolations)
      .toList

    def concatenations: List[SingleOrDoubleQuotedStringContext] = ctx.singleOrDoubleQuotedString().asScala.toList
    def isInterpolated: Boolean = ctx.doubleQuotedString().isInterpolated || concatenations.exists(_.isInterpolated)
  }

  sealed implicit class DoubleQuotedSymbolExpressionContextHelper(ctx: DoubleQuotedSymbolLiteralContext) {
    def interpolations: List[ParserRuleContext] = ctx.doubleQuotedString().interpolations ++ (ctx
      .doubleQuotedString() :: Nil)
      .filter(_.isInterpolated)
      .flatMap(_.interpolations)

    def concatenations: List[DoubleQuotedStringContext] = ctx.doubleQuotedString() :: Nil
    def isInterpolated: Boolean = ctx.doubleQuotedString().isInterpolated || concatenations.exists(_.isInterpolated)
  }

  sealed implicit class SingleQuotedStringExpressionContextHelper(ctx: SingleQuotedStringExpressionContext) {
    def concatenations: List[SingleOrDoubleQuotedStringContext] = ctx.singleOrDoubleQuotedString().asScala.toList
    def isInterpolated: Boolean                                 = concatenations.exists(_.isInterpolated)
    def interpolations: List[ParserRuleContext] =
      concatenations.filter(_.isInterpolated).flatMap(_.doubleQuotedString().interpolations)
  }

  sealed implicit class RegularExpressionLiteralContextHelper(ctx: RegularExpressionLiteralContext) {
    def isStatic: Boolean  = !isDynamic
    def isDynamic: Boolean = interpolations.nonEmpty

    def interpolations: List[ParserRuleContext] = ctx
      .regexpLiteralContent()
      .asScala
      .filter(ctx => Option(ctx.compoundStatement()).isDefined)
      .map(ctx => ctx.compoundStatement())
      .toList
  }

  sealed implicit class QuotedExpandedRegularExpressionLiteralContextHelper(
    ctx: QuotedExpandedRegularExpressionLiteralContext
  ) {

    def isStatic: Boolean  = !isDynamic
    def isDynamic: Boolean = interpolations.nonEmpty

    def interpolations: List[ParserRuleContext] = ctx
      .quotedExpandedLiteralStringContent()
      .asScala
      .filter(ctx => Option(ctx.compoundStatement()).isDefined)
      .map(ctx => ctx.compoundStatement())
      .toList

  }

  sealed implicit class CurlyBracesBlockContextHelper(ctx: CurlyBracesBlockContext) {
    def parameters: List[ParserRuleContext] = Option(ctx.blockParameter()).map(_.parameters).getOrElse(List())
  }

  sealed implicit class BlockParameterContextHelper(ctx: BlockParameterContext) {
    def parameters: List[ParserRuleContext] = Option(ctx.parameterList()).map(_.parameters).getOrElse(List())
  }

  sealed implicit class CommandArgumentContextHelper(ctx: CommandArgumentContext) {
    def arguments: List[ParserRuleContext] = ctx match {
      case ctx: CommandCommandArgumentListContext         => ctx.command() :: Nil
      case ctx: CommandArgumentCommandArgumentListContext => ctx.commandArgumentList().elements
      case ctx                                            => Nil
    }
  }

  sealed implicit class CommandArgumentListContextHelper(ctx: CommandArgumentListContext) {
    def elements: List[ParserRuleContext] = {
      val primaryValues = Option(ctx.primaryValueList()).map(_.primaryValue().asScala.toList).getOrElse(List())
      val associations  = Option(ctx.associationList()).map(_.association().asScala.toList).getOrElse(List())
      primaryValues ++ associations
    }
  }

  sealed implicit class ModifierStatementContextHelpers(ctx: ModifierStatementContext) {
    def isUnless: Boolean = Option(ctx.statementModifier().UNLESS()).isDefined
    def isIf: Boolean     = Option(ctx.statementModifier().IF()).isDefined
  }

  sealed implicit class QuotedExpandedArrayElementListContextHelper(ctx: QuotedExpandedArrayElementListContext) {
    def elements: List[ParserRuleContext] = ctx.quotedExpandedArrayElement.asScala.toList
  }

  sealed implicit class QuotedExpandedArrayElementContextHelper(ctx: QuotedExpandedArrayElementContext) {
    def interpolations: List[ParserRuleContext] = ctx
      .quotedExpandedArrayElementContent()
      .asScala
      .filter(x => Option(x.compoundStatement()).isDefined)
      .map(_.compoundStatement())
      .toList
    def hasInterpolation: Boolean =
      ctx.interpolations.nonEmpty
  }

  sealed implicit class QuotedNonExpandedArrayElementListContextHelper(ctx: QuotedNonExpandedArrayElementListContext) {
    def elements: List[ParserRuleContext] = ctx.quotedNonExpandedArrayElementContent().asScala.toList
  }

  sealed implicit class AssociationListContextHelper(ctx: AssociationListContext) {
    def associations: List[ParserRuleContext] = ctx.association().asScala.toList
  }

  sealed implicit class MethodIdentifierContextHelper(ctx: MethodIdentifierContext) {
    def isAttrDeclaration: Boolean = Set("attr_reader", "attr_writer", "attr_accessor").contains(ctx.getText)
  }

  sealed implicit class MandatoryOrOptionalParameterListContextHelper(ctx: MandatoryOrOptionalParameterListContext) {
    def parameters: List[ParserRuleContext] = ctx.mandatoryOrOptionalParameter().asScala.toList
  }

  sealed implicit class MethodParameterPartContextHelper(ctx: MethodParameterPartContext) {
    def parameters: List[ParserRuleContext] = Option(ctx.parameterList()).map(_.parameters).getOrElse(List())
  }

  sealed implicit class ParameterListContextHelper(ctx: ParameterListContext) {
    def parameters: List[ParserRuleContext] = {
      val mandatoryOrOptionals = Option(ctx.mandatoryOrOptionalParameterList()).map(_.parameters).getOrElse(List())
      val arrayParameter       = Option(ctx.arrayParameter()).toList
      val hashParameter        = Option(ctx.hashParameter()).toList
      val procParameter        = Option(ctx.procParameter()).toList
      mandatoryOrOptionals ++ arrayParameter ++ hashParameter ++ procParameter
    }
  }

  sealed implicit class IndexingArgumentListContextHelper(ctx: IndexingArgumentListContext) {
    def arguments: List[ParserRuleContext] = ctx match
      case ctx: CommandIndexingArgumentListContext => List(ctx.command())
      case ctx: OperatorExpressionListIndexingArgumentListContext =>
        ctx.operatorExpressionList().operatorExpression().asScala.toList
      case ctx: AssociationListIndexingArgumentListContext   => ctx.associationList().associations
      case ctx: SplattingArgumentIndexingArgumentListContext => ctx.splattingArgument() :: Nil
      case ctx: OperatorExpressionListWithSplattingArgumentIndexingArgumentListContext => ctx.splattingArgument() :: Nil
      case ctx =>
        logger.warn(s"IndexingArgumentListContextHelper - Unsupported argument type ${ctx.getClass}")
        List()
  }

  sealed implicit class ArgumentWithParenthesesContextHelper(ctx: ArgumentWithParenthesesContext) {
    def arguments: List[ParserRuleContext] = ctx match
      case _: EmptyArgumentWithParenthesesContext          => List()
      case ctx: ArgumentListArgumentWithParenthesesContext => ctx.argumentList().elements
      case ctx =>
        logger.warn(s"ArgumentWithParenthesesContextHelper - Unsupported argument type ${ctx.getClass}")
        List()
  }

  sealed implicit class ArgumentListContextHelper(ctx: ArgumentListContext) {
    def elements: List[ParserRuleContext] = ctx match
      case ctx: OperatorsArgumentListContext =>
        val operatorExpressions = ctx.operatorExpressionList().operatorExpression().asScala.toList
        val associations        = Option(ctx.associationList()).fold(List())(_.association().asScala)
        val splatting           = Option(ctx.splattingArgument()).toList
        val block               = Option(ctx.blockArgument()).toList
        operatorExpressions ++ associations ++ splatting ++ block
      case ctx: AssociationsArgumentListContext =>
        Option(ctx.associationList()).map(_.associations).getOrElse(List.empty)
      case ctx: SplattingArgumentArgumentListContext =>
        Option(ctx.splattingArgument()).toList ++ Option(ctx.blockArgument()).toList ++ Option(
          ctx.operatorExpressionList()
        ).toList
      case ctx: BlockArgumentArgumentListContext =>
        Option(ctx.blockArgument()).toList
      case ctx =>
        logger.warn(s"ArgumentListContextHelper - Unsupported element type ${ctx.getClass.getSimpleName}")
        List()
  }

  sealed implicit class CommandWithDoBlockContextHelper(ctx: CommandWithDoBlockContext) {
    def arguments: List[ParserRuleContext] = Option(ctx.argumentList()).map(_.elements).getOrElse(Nil)
  }
}
