package io.joern.rubysrc2cpg.astcreation

import io.joern.rubysrc2cpg.parser.RubyParser.{ArgumentsContext, BlockArgumentTypeArgumentsContext, BlockExprAssocTypeArgumentsContext, BlockSplattingExprAssocTypeArgumentsContext, BlockSplattingTypeArgumentsContext, CommandTypeArgumentsContext}
import io.joern.x2cpg.Ast

trait AstForDeclarationsCreator { this: AstCreator =>
  
  // TODO: Return Ast instead of Seq[Ast]
  protected def astForArguments(ctx: ArgumentsContext): Seq[Ast] = ctx match {
    case ctx: BlockArgumentTypeArgumentsContext => astForBlockArgumentTypeArgumentsContext(ctx)
    case ctx: BlockSplattingTypeArgumentsContext => astForBlockSplattingTypeArgumentsContext(ctx)
    case ctx: BlockSplattingExprAssocTypeArgumentsContext => astForBlockSplattingExprAssocTypeArgumentsContext(ctx)
    case ctx: BlockExprAssocTypeArgumentsContext => astForBlockExprAssocTypeArgumentsContext(ctx)
    case ctx: CommandTypeArgumentsContext => astForCommand(ctx.command)
  }
}
