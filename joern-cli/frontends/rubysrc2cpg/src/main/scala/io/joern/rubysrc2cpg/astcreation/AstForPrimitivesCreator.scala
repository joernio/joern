package io.joern.rubysrc2cpg.astcreation

import io.joern.rubysrc2cpg.parser.RubyParser
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.x2cpg.Ast

trait AstForPrimitivesCreator { this: AstCreator =>

  protected def astForNilLiteral(ctx: RubyParser.NilPseudoVariableIdentifierContext): Ast =
    Ast(literalNode(ctx, ctx.getText, Defines.NilClass))

  protected def astForTrueLiteral(ctx: RubyParser.TruePseudoVariableIdentifierContext): Ast =
    Ast(literalNode(ctx, ctx.getText, Defines.TrueClass))

  protected def astForFalseLiteral(ctx: RubyParser.FalsePseudoVariableIdentifierContext): Ast =
    Ast(literalNode(ctx, ctx.getText, Defines.FalseClass))

  protected def astForSelfPseudoIdentifier(ctx: RubyParser.SelfPseudoVariableIdentifierContext): Ast =
    Ast(createIdentifierWithScope(ctx, ctx.getText, ctx.getText, Defines.Object))

  protected def astForFilePseudoIdentifier(ctx: RubyParser.FilePseudoVariableIdentifierContext): Ast =
    Ast(createIdentifierWithScope(ctx, ctx.getText, ctx.getText, Defines.String))

  protected def astForLinePseudoIdentifier(ctx: RubyParser.LinePseudoVariableIdentifierContext): Ast =
    Ast(createIdentifierWithScope(ctx, ctx.getText, ctx.getText, Defines.Integer))

  protected def astForEncodingPseudoIdentifier(ctx: RubyParser.EncodingPseudoVariableIdentifierContext): Ast =
    Ast(createIdentifierWithScope(ctx, ctx.getText, ctx.getText, Defines.Encoding))

  protected def astForNumericLiteral(ctx: RubyParser.NumericLiteralContext): Ast = {
    val numericTypeName = if (isFloatLiteral(ctx.unsignedNumericLiteral)) Defines.Float else Defines.Integer
    Ast(literalNode(ctx, ctx.getText, numericTypeName))
  }

  protected def astForSymbolLiteral(ctx: RubyParser.SymbolContext): Ast =
    Ast(literalNode(ctx, ctx.getText, Defines.Symbol))

  protected def astForSingleQuotedStringLiteral(ctx: RubyParser.SingleQuotedStringLiteralContext): Ast =
    Ast(literalNode(ctx, ctx.getText, Defines.String))

  protected def astForDoubleQuotedStringLiteral(ctx: RubyParser.DoubleQuotedStringLiteralContext): Ast =
    Ast(literalNode(ctx, ctx.getText, Defines.String))

  protected def astForRegularExpressionLiteral(ctx: RubyParser.RegularExpressionLiteralContext): Ast =
    Ast(literalNode(ctx, ctx.getText, Defines.Regexp))

  private def isFloatLiteral(ctx: RubyParser.UnsignedNumericLiteralContext): Boolean =
    Option(ctx.FLOAT_LITERAL_WITH_EXPONENT).isDefined || Option(ctx.FLOAT_LITERAL_WITHOUT_EXPONENT).isDefined

}
