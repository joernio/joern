package io.joern.rubysrc2cpg.astcreation

import io.joern.rubysrc2cpg.parser.RubyParser.*
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.nodes.NewJumpTarget
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, DispatchTypes, ModifierTypes, Operators}
import org.antlr.v4.runtime.ParserRuleContext
import scala.jdk.CollectionConverters.CollectionHasAsScala

trait AstForDeclarationsCreator { this: AstCreator =>

  protected def astForArguments(ctx: ArgumentsContext): Seq[Ast] = {
    ctx.argument().asScala.flatMap(astForArgument).toSeq
  }

  protected def astForArgument(ctx: ArgumentContext): Seq[Ast] = ctx match {
    case ctx: BlockArgumentArgumentContext     => astForBlockArgumentContext(ctx.blockArgument)
    case ctx: SplattingArgumentArgumentContext => astForExpressionOrCommand(ctx.splattingArgument.expressionOrCommand)
    case ctx: ExpressionArgumentContext        => astForExpressionContext(ctx.expression)
    case ctx: AssociationArgumentContext       => astForAssociationContext(ctx.association)
    case ctx: CommandArgumentContext           => astForCommand(ctx.command)
  }
}
