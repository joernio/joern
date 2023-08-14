package io.joern.rubysrc2cpg.astcreation

import io.joern.rubysrc2cpg.parser.RubyParser
import io.joern.rubysrc2cpg.parser.RubyParser.*
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.rubysrc2cpg.passes.Defines.getBuiltInType
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.NewCall
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import org.antlr.v4.runtime.ParserRuleContext

import scala.jdk.CollectionConverters.CollectionHasAsScala

trait AstForPrimitivesCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def astForNilLiteral(ctx: RubyParser.NilPseudoVariableIdentifierContext): Ast =
    Ast(literalNode(ctx, ctx.getText, Defines.NilClass))

  protected def astForTrueLiteral(ctx: RubyParser.TruePseudoVariableIdentifierContext): Ast =
    Ast(literalNode(ctx, ctx.getText, Defines.TrueClass))

  protected def astForFalseLiteral(ctx: RubyParser.FalsePseudoVariableIdentifierContext): Ast =
    Ast(literalNode(ctx, ctx.getText, Defines.FalseClass))

  protected def astForSelfPseudoIdentifier(ctx: RubyParser.SelfPseudoVariableIdentifierContext): Ast =
    Ast(createIdentifierWithScope(ctx, ctx.getText, ctx.getText, Defines.Object))

  protected def astForFilePseudoIdentifier(ctx: RubyParser.FilePseudoVariableIdentifierContext): Ast =
    Ast(createIdentifierWithScope(ctx, ctx.getText, ctx.getText, getBuiltInType(Defines.String)))

  protected def astForLinePseudoIdentifier(ctx: RubyParser.LinePseudoVariableIdentifierContext): Ast =
    Ast(createIdentifierWithScope(ctx, ctx.getText, ctx.getText, getBuiltInType(Defines.Integer)))

  protected def astForEncodingPseudoIdentifier(ctx: RubyParser.EncodingPseudoVariableIdentifierContext): Ast =
    Ast(createIdentifierWithScope(ctx, ctx.getText, ctx.getText, Defines.Encoding))

  protected def astForNumericLiteral(ctx: RubyParser.NumericLiteralContext): Ast = {
    val numericTypeName =
      if (isFloatLiteral(ctx.unsignedNumericLiteral)) getBuiltInType(Defines.Float) else getBuiltInType(Defines.Integer)
    Ast(literalNode(ctx, ctx.getText, numericTypeName))
  }

  protected def astForSymbolLiteral(ctx: RubyParser.SymbolContext): Ast =
    Ast(literalNode(ctx, ctx.getText, Defines.Symbol))

  protected def astForSingleQuotedStringLiteral(ctx: RubyParser.SingleQuotedStringLiteralContext): Ast =
    Ast(literalNode(ctx, ctx.getText, getBuiltInType(Defines.String)))

  protected def astForDoubleQuotedStringLiteral(ctx: RubyParser.DoubleQuotedStringLiteralContext): Ast =
    Ast(literalNode(ctx, ctx.getText, getBuiltInType(Defines.String)))

  protected def astForRegularExpressionLiteral(ctx: RubyParser.RegularExpressionLiteralContext): Ast =
    Ast(literalNode(ctx, ctx.getText, Defines.Regexp))

  protected def astForNonExpandedQuotedRegularExpressionLiteral(
    ctx: RubyParser.NonExpandedQuotedRegularExpressionLiteralContext
  ): Ast =
    Ast(literalNode(ctx, ctx.getText, Defines.Regexp))

  private def isFloatLiteral(ctx: RubyParser.UnsignedNumericLiteralContext): Boolean =
    Option(ctx.FLOAT_LITERAL_WITH_EXPONENT).isDefined || Option(ctx.FLOAT_LITERAL_WITHOUT_EXPONENT).isDefined

  // TODO: Return Ast instead of Seq[Ast]
  protected def astForArrayLiteral(ctx: ArrayConstructorContext): Seq[Ast] = ctx match
    case ctx: BracketedArrayConstructorContext         => astForBracketedArrayConstructor(ctx)
    case ctx: NonExpandedWordArrayConstructorContext   => astForNonExpandedWordArrayConstructor(ctx)
    case ctx: NonExpandedSymbolArrayConstructorContext => astForNonExpandedSymbolArrayConstructor(ctx)

  private def astForBracketedArrayConstructor(ctx: BracketedArrayConstructorContext): Seq[Ast] = {
    Option(ctx.indexingArguments)
      .map(astForIndexingArgumentsContext)
      .getOrElse(Seq(astForEmptyArrayInitializer(ctx)))
  }

  private def astForEmptyArrayInitializer(ctx: ParserRuleContext): Ast = {
    Ast(
      callNode(ctx, ctx.getText, Operators.arrayInitializer, Operators.arrayInitializer, DispatchTypes.STATIC_DISPATCH)
    )
  }

  private def astForNonExpandedWordArrayConstructor(ctx: NonExpandedWordArrayConstructorContext): Seq[Ast] = {
    Option(ctx.nonExpandedArrayElements)
      .map(astForNonExpandedArrayElements(_, astForNonExpandedWordArrayElement))
      .getOrElse(Seq(astForEmptyArrayInitializer(ctx)))
  }

  private def astForNonExpandedWordArrayElement(ctx: NonExpandedArrayElementContext): Ast = {
    Ast(literalNode(ctx, ctx.getText, Defines.String, List(Defines.String)))
  }

  private def astForNonExpandedSymbolArrayConstructor(ctx: NonExpandedSymbolArrayConstructorContext): Seq[Ast] = {
    Option(ctx.nonExpandedArrayElements)
      .map(astForNonExpandedArrayElements(_, astForNonExpandedSymbolArrayElement))
      .getOrElse(Seq(astForEmptyArrayInitializer(ctx)))
  }

  private def astForNonExpandedArrayElements(
    ctx: NonExpandedArrayElementsContext,
    astForNonExpandedArrayElement: NonExpandedArrayElementContext => Ast
  ): Seq[Ast] = {
    ctx.nonExpandedArrayElement.asScala.map(astForNonExpandedArrayElement).toSeq
  }

  private def astForNonExpandedSymbolArrayElement(ctx: NonExpandedArrayElementContext): Ast = {
    Ast(literalNode(ctx, ctx.getText, Defines.Symbol))
  }
}
