package io.joern.rubysrc2cpg.deprecated.astcreation

import io.joern.rubysrc2cpg.deprecated.parser.DeprecatedRubyParser.*
import io.joern.rubysrc2cpg.deprecated.passes.Defines
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.nodes.{NewJumpTarget, NewLiteral}
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, DispatchTypes, ModifierTypes, Operators}
import org.antlr.v4.runtime.ParserRuleContext
import org.slf4j.LoggerFactory

import scala.jdk.CollectionConverters.CollectionHasAsScala

trait AstForDeclarationsCreator { this: AstCreator =>

  private val logger = LoggerFactory.getLogger(this.getClass)

  protected def astForArguments(ctx: ArgumentsContext): Seq[Ast] = {
    ctx.argument().asScala.flatMap(astForArgument).toSeq
  }

  protected def astForArgument(ctx: ArgumentContext): Seq[Ast] = {
    ctx match {
      case ctx: BlockArgumentArgumentContext     => astForExpressionContext(ctx.blockArgument.expression)
      case ctx: SplattingArgumentArgumentContext => astForExpressionOrCommand(ctx.splattingArgument.expressionOrCommand)
      case ctx: ExpressionArgumentContext        => astForExpressionContext(ctx.expression)
      case ctx: AssociationArgumentContext       => astForAssociationContext(ctx.association)
      case ctx: CommandArgumentContext           => astForCommand(ctx.command)
      case ctx: HereDocArgumentContext           => astForHereDocArgument(ctx)
      case _ =>
        logger.error(s"astForArgument() $relativeFilename, ${ctx.getText} All contexts mismatched.")
        Seq()
    }
  }
}
