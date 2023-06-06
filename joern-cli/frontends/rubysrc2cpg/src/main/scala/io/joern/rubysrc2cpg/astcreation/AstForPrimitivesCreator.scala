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
}
