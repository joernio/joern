package io.joern.rubysrc2cpg.astcreation

import io.joern.rubysrc2cpg.parser.RubyParser.*
import io.joern.rubysrc2cpg.passes.Defines
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
        val retNode = returnNode(ctx.BREAK(), text(ctx))
        val argAst  = astForArguments(args)
        Seq(returnAst(retNode, argAst))
      case None =>
        Seq(
          astForControlStructure(ctx.getClass.getSimpleName, ctx.BREAK(), ControlStructureTypes.BREAK, text(ctx))
            .withChildren(astForArguments(ctx.arguments))
        )
    }
  }

  protected def astForJumpExpressionPrimaryContext(ctx: JumpExpressionPrimaryContext): Seq[Ast] = {
    val parserTypeName = ctx.getClass.getSimpleName
    val controlStructureAst = ctx.jumpExpression() match
      case expr if expr.BREAK() != null =>
        astForControlStructure(parserTypeName, expr.BREAK(), ControlStructureTypes.BREAK, text(ctx))
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
    tryCatchAst(tryNode, tryBodyAst, catchAsts, finallyAst)
  }

  protected def astForUntilExpression(ctx: UntilExpressionContext): Seq[Ast] = {
    val (boilerplate, exprAst) = astForExpressionOrCommand(ctx.expressionOrCommand()).partitionClosureFromExpr
    val bodyAst                = astForCompoundStatement(ctx.doClause().compoundStatement())
    // TODO: testAst should be negated if it's going to be modelled as a while stmt.
    boilerplate :+ whileAst(exprAst, bodyAst, Some(text(ctx)), line(ctx), column(ctx))
  }

  protected def astForForExpression(ctx: ForExpressionContext): Seq[Ast] = {
    val forVarAst                 = astForForVariableContext(ctx.forVariable())
    val (boilerplate, forExprAst) = astForExpressionOrCommand(ctx.expressionOrCommand()).partitionClosureFromExpr
    val forBodyAst                = astForCompoundStatement(ctx.doClause().compoundStatement())
    // TODO: for X in Y is not properly modelled by while Y
    val forRootAst = whileAst(forExprAst, forBodyAst, Some(text(ctx)), line(ctx), column(ctx))
    boilerplate :+ forVarAst.headOption.map(forRootAst.withChild).getOrElse(forRootAst)
  }

  private def astForForVariableContext(ctx: ForVariableContext): Seq[Ast] = {
    if (ctx.singleLeftHandSide() != null) {
      astForSingleLeftHandSideContext(ctx.singleLeftHandSide())
    } else if (ctx.multipleLeftHandSide() != null) {
      astForMultipleLeftHandSideContext(ctx.multipleLeftHandSide())
    } else {
      Seq(Ast())
    }
  }

  protected def astForWhileExpression(ctx: WhileExpressionContext): Seq[Ast] = {
    val (boilerplate, exprAst) = astForExpressionOrCommand(ctx.expressionOrCommand()).partitionClosureFromExpr
    val bodyAst                = astForCompoundStatement(ctx.doClause().compoundStatement())
    boilerplate :+ whileAst(exprAst, bodyAst, Some(text(ctx)), line(ctx), column(ctx))
  }

  protected def astForIfExpression(ctx: IfExpressionContext): Seq[Ast] = {
    val (boilerplate, exprAst) = astForExpressionOrCommand(ctx.expressionOrCommand()).partitionClosureFromExpr
    val thenAst                = astForCompoundStatement(ctx.thenClause().compoundStatement())
    val elsifAsts              = Option(ctx.elsifClause).map(_.asScala).getOrElse(Seq()).flatMap(astForElsifClause)
    val elseAst = Option(ctx.elseClause()).map(ctx => astForCompoundStatement(ctx.compoundStatement())).getOrElse(Seq())
    val ifNode  = controlStructureNode(ctx, ControlStructureTypes.IF, text(ctx))
    boilerplate :+ controlStructureAst(ifNode, exprAst)
      .withChildren(thenAst)
      .withChildren(elsifAsts.toSeq)
      .withChildren(elseAst)
  }

  private def astForElsifClause(ctx: ElsifClauseContext): Seq[Ast] = {
    val ifNode                 = controlStructureNode(ctx, ControlStructureTypes.IF, text(ctx))
    val (boilerplate, exprAst) = astForExpressionOrCommand(ctx.expressionOrCommand()).partitionClosureFromExpr
    val bodyAst                = astForCompoundStatement(ctx.thenClause().compoundStatement())
    boilerplate :+ controlStructureAst(ifNode, exprAst, bodyAst)
  }

}
