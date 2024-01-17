package io.joern.swiftsrc2cpg.astcreation

import io.joern.swiftsrc2cpg.astcreation.AstCreatorHelper.OptionSafeAst
import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.*
import io.joern.x2cpg.Ast
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.datastructures.Stack.*
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.codepropertygraph.generated.nodes.NewJumpLabel

trait AstForStmtSyntaxCreator(implicit withSchemaValidation: ValidationMode) {
  this: AstCreator =>

  private def astForBreakStmtSyntax(node: BreakStmtSyntax): Ast = {
    val labelAst = node.label
      .map(l =>
        val labelCode = code(l)
        Ast(
          NewJumpLabel()
            .parserTypeName(node.toString)
            .name(labelCode)
            .code(labelCode)
            .lineNumber(line(node))
            .columnNumber(column(node))
            .order(1)
        )
      )
      .getOrElse(Ast())
    Ast(controlStructureNode(node, ControlStructureTypes.BREAK, code(node))).withChild(labelAst)
  }

  private def astForContinueStmtSyntax(node: ContinueStmtSyntax): Ast = {
    val labelAst = node.label
      .map(l =>
        val labelCode = code(l)
        Ast(
          NewJumpLabel()
            .parserTypeName(node.toString)
            .name(labelCode)
            .code(labelCode)
            .lineNumber(line(node))
            .columnNumber(column(node))
            .order(1)
        )
      )
      .getOrElse(Ast())
    Ast(controlStructureNode(node, ControlStructureTypes.CONTINUE, code(node))).withChild(labelAst)
  }

  private def astForDeferStmtSyntax(node: DeferStmtSyntax): Ast = {
    astForNode(node.body)
  }

  private def astForDiscardStmtSyntax(node: DiscardStmtSyntax): Ast = notHandledYet(node)

  private def astForDoStmtSyntax(node: DoStmtSyntax): Ast = {
    val tryNode  = controlStructureNode(node, ControlStructureTypes.TRY, code(node))
    val bodyAst  = astForNode(node.body)
    val catchAst = astForNode(node.catchClauses)
    // The semantics of try statement children is defined by their order value.
    // Thus we set the here explicitly and do not rely on the usual consecutive
    // ordering.
    setOrderExplicitly(bodyAst, 1)
    setOrderExplicitly(catchAst, 2)
    Ast(tryNode).withChildren(List(bodyAst, catchAst))
  }

  private def astForExpressionStmtSyntax(node: ExpressionStmtSyntax): Ast = {
    astForNodeWithFunctionReference(node.expression)
  }

  private def astForFallThroughStmtSyntax(node: FallThroughStmtSyntax): Ast = {
    Ast(controlStructureNode(node, ControlStructureTypes.CONTINUE, code(node)))
  }

  private def astForForStmtSyntax(node: ForStmtSyntax): Ast     = notHandledYet(node)
  private def astForGuardStmtSyntax(node: GuardStmtSyntax): Ast = notHandledYet(node)

  private def astForLabeledStmtSyntax(node: LabeledStmtSyntax): Ast = {
    val labeledNode = jumpTargetNode(node, code(node.label), code(node))

    val blockNode_ = blockNode(node)
    scope.pushNewBlockScope(blockNode_)
    localAstParentStack.push(blockNode_)
    val bodyAst = astForNodeWithFunctionReference(node.statement)
    scope.popScope()
    localAstParentStack.pop()

    val labelAsts = List(Ast(labeledNode), bodyAst)
    setArgumentIndices(labelAsts)
    blockAst(blockNode_, labelAsts)
  }

  private def astForMissingStmtSyntax(node: MissingStmtSyntax): Ast = notHandledYet(node)

  private def astForRepeatStmtSyntax(node: RepeatStmtSyntax): Ast = {
    val code = this.code(node)
    // In Swift, a repeat-while loop is semantically the same as a C do-while loop
    val doNode       = controlStructureNode(node, ControlStructureTypes.DO, code)
    val conditionAst = astForNodeWithFunctionReference(node.condition)
    val bodyAst      = astForNode(node.body)
    setOrderExplicitly(conditionAst, 1)
    setOrderExplicitly(bodyAst, 2)
    controlStructureAst(doNode, Some(conditionAst), Seq(bodyAst), placeConditionLast = true)
  }

  private def astForReturnStmtSyntax(node: ReturnStmtSyntax): Ast = {
    val cpgReturn = returnNode(node, code(node))
    node.expression match {
      case Some(value) =>
        val expr = astForNodeWithFunctionReference(value)
        Ast(cpgReturn).withChild(expr).withArgEdge(cpgReturn, expr.root)
      case None =>
        Ast(cpgReturn)
    }

  }

  private def astForThenStmtSyntax(node: ThenStmtSyntax): Ast   = notHandledYet(node)
  private def astForThrowStmtSyntax(node: ThrowStmtSyntax): Ast = notHandledYet(node)

  private def astForWhileStmtSyntax(node: WhileStmtSyntax): Ast = {
    val code         = this.code(node)
    val conditionAst = astForNodeWithFunctionReference(node.conditions)
    val bodyAst      = astForNode(node.body)
    setOrderExplicitly(conditionAst, 1)
    setOrderExplicitly(bodyAst, 2)
    whileAst(Some(conditionAst), Seq(bodyAst), code = Some(code), lineNumber = line(node), columnNumber = column(node))
  }

  private def astForYieldStmtSyntax(node: YieldStmtSyntax): Ast = notHandledYet(node)

  protected def astForStmtSyntax(stmtSyntax: StmtSyntax): Ast = stmtSyntax match {
    case node: BreakStmtSyntax       => astForBreakStmtSyntax(node)
    case node: ContinueStmtSyntax    => astForContinueStmtSyntax(node)
    case node: DeferStmtSyntax       => astForDeferStmtSyntax(node)
    case node: DiscardStmtSyntax     => astForDiscardStmtSyntax(node)
    case node: DoStmtSyntax          => astForDoStmtSyntax(node)
    case node: ExpressionStmtSyntax  => astForExpressionStmtSyntax(node)
    case node: FallThroughStmtSyntax => astForFallThroughStmtSyntax(node)
    case node: ForStmtSyntax         => astForForStmtSyntax(node)
    case node: GuardStmtSyntax       => astForGuardStmtSyntax(node)
    case node: LabeledStmtSyntax     => astForLabeledStmtSyntax(node)
    case node: MissingStmtSyntax     => astForMissingStmtSyntax(node)
    case node: RepeatStmtSyntax      => astForRepeatStmtSyntax(node)
    case node: ReturnStmtSyntax      => astForReturnStmtSyntax(node)
    case node: ThenStmtSyntax        => astForThenStmtSyntax(node)
    case node: ThrowStmtSyntax       => astForThrowStmtSyntax(node)
    case node: WhileStmtSyntax       => astForWhileStmtSyntax(node)
    case node: YieldStmtSyntax       => astForYieldStmtSyntax(node)
  }

}
