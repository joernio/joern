package io.joern.rubysrc2cpg.astcreation

import io.joern.rubysrc2cpg.parser.RubyParser.{
  BreakArgsInvocationWithoutParenthesesContext,
  CaseExpressionPrimaryContext,
  JumpExpressionPrimaryContext,
  NextArgsInvocationWithoutParenthesesContext,
  WhenArgumentContext
}
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.codepropertygraph.generated.nodes.{NewControlStructure, NewReturn}

import scala.jdk.CollectionConverters.*
trait AstForControlStructuresCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def astForWhenArgumentContext(ctx: WhenArgumentContext): Seq[Ast] = {
    val expAsts =
      ctx
        .expressions()
        .expression()
        .asScala
        .flatMap(astForExpressionContext)
        .toList

    if (ctx.splattingArgument() != null) {
      expAsts ++ astForExpressionOrCommand(ctx.splattingArgument().expressionOrCommand())
    } else {
      expAsts
    }
  }

  protected def astForCaseExpressionPrimaryContext(ctx: CaseExpressionPrimaryContext): Seq[Ast] = {
    val code = s"case ${Option(ctx.caseExpression().expressionOrCommand).map(_.getText).getOrElse("")}".stripTrailing()
    val switchNode = controlStructureNode(ctx, ControlStructureTypes.SWITCH, code)
    val conditionAst = Option(ctx.caseExpression().expressionOrCommand()).toList
      .flatMap(astForExpressionOrCommand)
      .headOption

    val whenThenAstsList = ctx
      .caseExpression()
      .whenClause()
      .asScala
      .flatMap(wh => {
        val whenNode =
          jumpTargetNode(wh, "case", s"case ${wh.getText}", Option(wh.getClass.getSimpleName))

        val whenACondAsts = astForWhenArgumentContext(wh.whenArgument())
        val thenAsts = astForCompoundStatement(
          wh.thenClause().compoundStatement(),
          isMethodBody = true,
          canConsiderAsLeaf = false
        ) ++ Seq(Ast(NewControlStructure().controlStructureType(ControlStructureTypes.BREAK)))
        Seq(Ast(whenNode)) ++ whenACondAsts ++ thenAsts
      })
      .toList

    val stmtAsts = whenThenAstsList ++ (Option(ctx.caseExpression().elseClause()) match
      case Some(elseClause) =>
        Ast(
          // name = "default" for behaviour determined by CfgCreator.cfgForJumpTarget
          jumpTargetNode(elseClause, "default", "else", Option(elseClause.getClass.getSimpleName))
        ) +: astForCompoundStatement(elseClause.compoundStatement(), isMethodBody = true, canConsiderAsLeaf = false)
      case None => Seq.empty[Ast]
    )
    val block = blockNode(ctx.caseExpression())
    Seq(controlStructureAst(switchNode, conditionAst, Seq(Ast(block).withChildren(stmtAsts))))
  }

  protected def astsForNextArgsInvocation(ctx: NextArgsInvocationWithoutParenthesesContext) = {
    /*
     * While this is a `CONTINUE` for now, if we detect that this is the LHS of an `IF` then this becomes a `RETURN`
     */
    val node = NewControlStructure()
      .controlStructureType(ControlStructureTypes.CONTINUE)
      .lineNumber(ctx.NEXT().getSymbol.getLine)
      .columnNumber(ctx.NEXT().getSymbol.getCharPositionInLine)
      .code(Defines.ModifierNext)
    Seq(
      Ast(node)
        .withChildren(astForArguments(ctx.arguments()))
    )
  }

  protected def astForBreakArgsInvocation(ctx: BreakArgsInvocationWithoutParenthesesContext) = {
    val args = ctx.arguments()
    Option(args) match {
      case Some(args) =>
        /*
         * This is break with args inside a block. The argument passed to break will be returned by the bloc
         * Model this as a return since this is effectively a  return
         */
        val retNode = NewReturn()
          .code(text(ctx))
          .lineNumber(ctx.BREAK().getSymbol().getLine)
          .columnNumber(ctx.BREAK().getSymbol().getCharPositionInLine)
        val argAst = astForArguments(args)
        Seq(returnAst(retNode, argAst))
      case None =>
        val node = NewControlStructure()
          .controlStructureType(ControlStructureTypes.BREAK)
          .lineNumber(ctx.BREAK().getSymbol.getLine)
          .columnNumber(ctx.BREAK().getSymbol.getCharPositionInLine)
          .code(text(ctx))
        Seq(
          Ast(node)
            .withChildren(astForArguments(ctx.arguments()))
        )
    }
  }

  protected def astForJumpExpressionPrimaryContext(ctx: JumpExpressionPrimaryContext): Seq[Ast] = {
    if (ctx.jumpExpression().BREAK() != null) {
      val node = NewControlStructure()
        .controlStructureType(ControlStructureTypes.BREAK)
        .lineNumber(ctx.jumpExpression().BREAK().getSymbol.getLine)
        .columnNumber(ctx.jumpExpression().BREAK().getSymbol.getCharPositionInLine)
        .code(text(ctx))
      Seq(Ast(node))
    } else if (ctx.jumpExpression().NEXT() != null) {
      val node = NewControlStructure()
        .controlStructureType(ControlStructureTypes.CONTINUE)
        .lineNumber(ctx.jumpExpression().NEXT().getSymbol.getLine)
        .columnNumber(ctx.jumpExpression().NEXT().getSymbol.getCharPositionInLine)
        .code(Defines.ModifierNext)
      Seq(Ast(node))
    } else if (ctx.jumpExpression().REDO() != null) {
      val node = NewControlStructure()
        .controlStructureType(ControlStructureTypes.CONTINUE)
        .lineNumber(ctx.jumpExpression().REDO().getSymbol.getLine)
        .columnNumber(ctx.jumpExpression().REDO().getSymbol.getCharPositionInLine)
        .code(Defines.ModifierRedo)
      Seq(Ast(node))
    } else if (ctx.jumpExpression().RETRY() != null) {
      val node = NewControlStructure()
        .controlStructureType(ControlStructureTypes.CONTINUE)
        .lineNumber(ctx.jumpExpression().RETRY().getSymbol.getLine)
        .columnNumber(ctx.jumpExpression().RETRY().getSymbol.getCharPositionInLine)
        .code(Defines.ModifierRetry)
      Seq(Ast(node))
    } else {
      Seq(Ast())
    }
  }

}
