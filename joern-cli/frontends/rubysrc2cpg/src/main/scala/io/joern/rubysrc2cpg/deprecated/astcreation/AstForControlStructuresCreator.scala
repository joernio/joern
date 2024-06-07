package io.joern.rubysrc2cpg.deprecated.astcreation

import io.joern.rubysrc2cpg.deprecated.parser.DeprecatedRubyParser.*
import io.joern.rubysrc2cpg.deprecated.passes.Defines
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.codepropertygraph.generated.nodes.NewControlStructure

import scala.jdk.CollectionConverters.*
trait AstForControlStructuresCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  private def astForWhenArgumentContext(ctx: WhenArgumentContext): Seq[Ast] = {
    val expAsts =
      ctx.expressions.expression.asScala
        .flatMap(astForExpressionContext)
        .toList

    if (ctx.splattingArgument != null) {
      expAsts ++ astForExpressionOrCommand(ctx.splattingArgument().expressionOrCommand())
    } else {
      expAsts
    }
  }

  protected def astForCaseExpressionPrimaryContext(ctx: CaseExpressionPrimaryContext): Seq[Ast] = {
    val codeString = s"case ${Option(ctx.caseExpression().expressionOrCommand).map(code).getOrElse("")}".stripTrailing()
    val switchNode = controlStructureNode(ctx, ControlStructureTypes.SWITCH, codeString)
    val conditionAst = Option(ctx.caseExpression().expressionOrCommand()).toList
      .flatMap(astForExpressionOrCommand)
      .headOption

    val whenThenAstsList = ctx
      .caseExpression()
      .whenClause()
      .asScala
      .flatMap(wh => {
        val whenNode =
          jumpTargetNode(wh, "case", s"case ${code(wh)}", Option(wh.getClass.getSimpleName))

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

  protected def astForNextArgsInvocation(ctx: NextArgsInvocationWithoutParenthesesContext): Seq[Ast] = {
    /*
     * While this is a `CONTINUE` for now, if we detect that this is the LHS of an `IF` then this becomes a `RETURN`
     */
    Seq(
      astForControlStructure(
        ctx.getClass.getSimpleName,
        ctx.NEXT(),
        ControlStructureTypes.CONTINUE,
        Defines.ModifierNext
      ).withChildren(astForArguments(ctx.arguments()))
    )
  }

  protected def astForBreakArgsInvocation(ctx: BreakArgsInvocationWithoutParenthesesContext): Seq[Ast] = {
    Option(ctx.arguments()) match {
      case Some(args) =>
        /*
         * This is break with args inside a block. The argument passed to break will be returned by the bloc
         * Model this as a return since this is effectively a  return
         */
        val retNode = returnNode(ctx.BREAK(), code(ctx))
        val argAst  = astForArguments(args)
        Seq(returnAst(retNode, argAst))
      case None =>
        Seq(
          astForControlStructure(ctx.getClass.getSimpleName, ctx.BREAK(), ControlStructureTypes.BREAK, code(ctx))
            .withChildren(astForArguments(ctx.arguments))
        )
    }
  }

  protected def astForJumpExpressionPrimaryContext(ctx: JumpExpressionPrimaryContext): Seq[Ast] = {
    val parserTypeName = ctx.getClass.getSimpleName
    val controlStructureAst = ctx.jumpExpression() match
      case expr if expr.BREAK() != null =>
        astForControlStructure(parserTypeName, expr.BREAK(), ControlStructureTypes.BREAK, code(ctx))
      case expr if expr.NEXT() != null =>
        astForControlStructure(parserTypeName, expr.NEXT(), ControlStructureTypes.CONTINUE, Defines.ModifierNext)
      case expr if expr.REDO() != null =>
        astForControlStructure(parserTypeName, expr.REDO(), ControlStructureTypes.CONTINUE, Defines.ModifierRedo)
      case expr if expr.RETRY() != null =>
        astForControlStructure(parserTypeName, expr.RETRY(), ControlStructureTypes.CONTINUE, Defines.ModifierRetry)
      case _ =>
        Ast()
    Seq(controlStructureAst)
  }

  protected def astForRescueClause(ctx: BodyStatementContext): Ast = {
    val compoundStatementAsts = astForCompoundStatement(ctx.compoundStatement)
    val elseClauseAsts = Option(ctx.elseClause) match
      case Some(ctx) => astForCompoundStatement(ctx.compoundStatement)
      case None      => Seq.empty

    /*
     * TODO Conversion of last statement to return AST is needed here
     * This can be done after the data flow engine issue with return from a try block is fixed
     */
    val tryBodyAsts = compoundStatementAsts ++ elseClauseAsts
    val tryBodyAst  = blockAst(blockNode(ctx), tryBodyAsts.toList)

    val finallyAst = Option(ctx.ensureClause) match
      case Some(ctx) => astForCompoundStatement(ctx.compoundStatement).headOption
      case None      => None

    val catchAsts = ctx.rescueClause.asScala
      .map(astForRescueClauseContext)
      .toSeq

    val tryNode = controlStructureNode(ctx, ControlStructureTypes.TRY, "try")
    tryCatchAstWithOrder(tryNode, tryBodyAst, catchAsts, finallyAst)
  }

}
