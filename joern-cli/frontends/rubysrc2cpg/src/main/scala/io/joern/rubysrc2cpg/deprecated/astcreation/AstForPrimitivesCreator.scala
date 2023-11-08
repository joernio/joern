package io.joern.rubysrc2cpg.deprecated.astcreation

import io.joern.rubysrc2cpg.deprecated.parser.DeprecatedRubyParser
import io.joern.rubysrc2cpg.deprecated.parser.DeprecatedRubyParser.*
import io.joern.rubysrc2cpg.deprecated.passes.Defines
import io.joern.rubysrc2cpg.deprecated.passes.Defines.getBuiltInType
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.NewCall
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import org.antlr.v4.runtime.ParserRuleContext

import scala.jdk.CollectionConverters.CollectionHasAsScala

trait AstForPrimitivesCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def astForNilLiteral(ctx: DeprecatedRubyParser.NilPseudoVariableIdentifierContext): Ast =
    Ast(literalNode(ctx, code(ctx), Defines.NilClass))

  protected def astForTrueLiteral(ctx: DeprecatedRubyParser.TruePseudoVariableIdentifierContext): Ast =
    Ast(literalNode(ctx, code(ctx), Defines.TrueClass))

  protected def astForFalseLiteral(ctx: DeprecatedRubyParser.FalsePseudoVariableIdentifierContext): Ast =
    Ast(literalNode(ctx, code(ctx), Defines.FalseClass))

  protected def astForSelfPseudoIdentifier(ctx: DeprecatedRubyParser.SelfPseudoVariableIdentifierContext): Ast =
    Ast(createIdentifierWithScope(ctx, code(ctx), code(ctx), Defines.Object))

  protected def astForFilePseudoIdentifier(ctx: DeprecatedRubyParser.FilePseudoVariableIdentifierContext): Ast =
    Ast(createIdentifierWithScope(ctx, code(ctx), code(ctx), getBuiltInType(Defines.String)))

  protected def astForLinePseudoIdentifier(ctx: DeprecatedRubyParser.LinePseudoVariableIdentifierContext): Ast =
    Ast(createIdentifierWithScope(ctx, code(ctx), code(ctx), getBuiltInType(Defines.Integer)))

  protected def astForEncodingPseudoIdentifier(ctx: DeprecatedRubyParser.EncodingPseudoVariableIdentifierContext): Ast =
    Ast(createIdentifierWithScope(ctx, code(ctx), code(ctx), Defines.Encoding))

  protected def astForNumericLiteral(ctx: DeprecatedRubyParser.NumericLiteralContext): Ast = {
    val numericTypeName =
      if (isFloatLiteral(ctx.unsignedNumericLiteral)) getBuiltInType(Defines.Float) else getBuiltInType(Defines.Integer)
    Ast(literalNode(ctx, code(ctx), numericTypeName))
  }

  protected def astForSymbolLiteral(ctx: DeprecatedRubyParser.SymbolContext): Ast =
    Ast(literalNode(ctx, code(ctx), Defines.Symbol))

  protected def astForSingleQuotedStringLiteral(ctx: DeprecatedRubyParser.SingleQuotedStringLiteralContext): Ast =
    Ast(literalNode(ctx, code(ctx), getBuiltInType(Defines.String)))

  protected def astForDoubleQuotedStringLiteral(ctx: DeprecatedRubyParser.DoubleQuotedStringLiteralContext): Ast =
    Ast(literalNode(ctx, code(ctx), getBuiltInType(Defines.String)))

  protected def astForRegularExpressionLiteral(ctx: DeprecatedRubyParser.RegularExpressionLiteralContext): Ast =
    Ast(literalNode(ctx, code(ctx), Defines.Regexp))

  private def isFloatLiteral(ctx: DeprecatedRubyParser.UnsignedNumericLiteralContext): Boolean =
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
    Ast(callNode(ctx, code(ctx), Operators.arrayInitializer, Operators.arrayInitializer, DispatchTypes.STATIC_DISPATCH))
  }

  private def astForNonExpandedWordArrayConstructor(ctx: NonExpandedWordArrayConstructorContext): Seq[Ast] = {
    Option(ctx.nonExpandedArrayElements)
      .map(astForNonExpandedArrayElements(_, astForNonExpandedWordArrayElement))
      .getOrElse(Seq(astForEmptyArrayInitializer(ctx)))
  }

  private def astForNonExpandedWordArrayElement(ctx: NonExpandedArrayElementContext): Ast = {
    Ast(literalNode(ctx, code(ctx), Defines.String, List(Defines.String)))
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
    Ast(literalNode(ctx, code(ctx), Defines.Symbol))
  }
}
