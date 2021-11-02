package io.shiftleft.c2cpg.astcreation

import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.codepropertygraph.generated.nodes.{NewBlock, NewReturn}
import io.shiftleft.x2cpg.Ast
import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.core.dom.ast.cpp._
import org.eclipse.cdt.core.dom.ast.gnu.IGNUASTGotoStatement
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTNamespaceAlias

trait AstForStatementsCreator {

  this: AstCreator =>

  protected def astForBlockStatement(blockStmt: IASTCompoundStatement, order: Int): Ast = {
    val cpgBlock = NewBlock()
      .order(order)
      .argumentIndex(order)
      .typeFullName(registerType(Defines.voidTypeName))
      .lineNumber(line(blockStmt))
      .columnNumber(column(blockStmt))

    scope.pushNewScope(cpgBlock)
    var currOrder = 1
    val childAsts = blockStmt.getStatements.flatMap { stmt =>
      val r = astsForStatement(stmt, currOrder)
      currOrder = currOrder + r.length
      r
    }

    val blockAst = Ast(cpgBlock).withChildren(childAsts.toIndexedSeq)
    scope.popScope()
    blockAst
  }

  private def astsForDeclarationStatement(decl: IASTDeclarationStatement, order: Int): Seq[Ast] =
    decl.getDeclaration match {
      case simplDecl: IASTSimpleDeclaration
          if simplDecl.getDeclarators.headOption.exists(_.isInstanceOf[IASTFunctionDeclarator]) =>
        Seq(astForFunctionDeclarator(simplDecl.getDeclarators.head.asInstanceOf[IASTFunctionDeclarator], order))
      case simplDecl: IASTSimpleDeclaration =>
        val locals =
          withOrder(simplDecl.getDeclarators) { (d, o) =>
            astForDeclarator(simplDecl, d, order + o - 1)
          }
        val calls =
          withOrder(simplDecl.getDeclarators.filter(_.getInitializer != null)) { (d, o) =>
            astForInitializer(d, d.getInitializer, locals.size + order + o - 1)
          }
        locals ++ calls
      case s: ICPPASTStaticAssertDeclaration =>
        Seq(astForStaticAssert(s, order))
      case usingDeclaration: ICPPASTUsingDeclaration =>
        handleUsingDeclaration(usingDeclaration)
        Seq.empty
      case alias: ICPPASTAliasDeclaration => Seq(astForAliasDeclaration(alias, order))
      case func: IASTFunctionDefinition   => Seq(astForFunctionDefinition(func, order))
      case alias: CPPASTNamespaceAlias    => Seq(astForNamespaceAlias(alias, order))
      case _: ICPPASTUsingDirective       => Seq.empty
      case asm: IASTASMDeclaration        => Seq(astForASMDeclaration(asm, order))
      case decl =>
        Seq(astForNode(decl, order))
    }

  private def astForReturnStatement(ret: IASTReturnStatement, order: Int): Ast = {
    val cpgReturn = NewReturn()
      .code(nodeSignature(ret))
      .order(order)
      .argumentIndex(order)
      .lineNumber(line(ret))
      .columnNumber(column(ret))
    val expr = nullSafeAst(ret.getReturnValue, 1)
    val ast = Ast(cpgReturn).withChild(expr)
    expr.root match {
      case Some(r) => ast.withArgEdge(cpgReturn, r)
      case None    => ast
    }
  }

  private def astForBreakStatement(br: IASTBreakStatement, order: Int): Ast = {
    Ast(newControlStructureNode(br, ControlStructureTypes.BREAK, nodeSignature(br), order))
  }

  private def astForContinueStatement(cont: IASTContinueStatement, order: Int): Ast = {
    Ast(newControlStructureNode(cont, ControlStructureTypes.CONTINUE, nodeSignature(cont), order))
  }

  private def astForGotoStatement(goto: IASTGotoStatement, order: Int): Ast = {
    val code = s"goto ${goto.getName.toString};"
    Ast(newControlStructureNode(goto, ControlStructureTypes.GOTO, code, order))
  }

  private def astsForGnuGotoStatement(goto: IGNUASTGotoStatement, order: Int): Seq[Ast] = {
    // This is for GNU GOTO labels as values.
    // See: https://gcc.gnu.org/onlinedocs/gcc/Labels-as-Values.html
    // For such GOTOs we cannot statically determine the target label. As a quick
    // hack we simply put edges to all labels found indicated by *. This might be an over-taint.
    val code = s"goto *;"
    val gotoNode = Ast(newControlStructureNode(goto, ControlStructureTypes.GOTO, code, order))
    val exprNode = nullSafeAst(goto.getLabelNameExpression, order + 1)
    Seq(gotoNode, exprNode)
  }

  private def astsForLabelStatement(label: IASTLabelStatement, order: Int): Seq[Ast] = {
    val cpgLabel = newJumpTarget(label, order)
    val nestedStmts = nullSafeAst(label.getNestedStatement, order + 1)
    Ast(cpgLabel) +: nestedStmts
  }

  private def astForDoStatement(doStmt: IASTDoStatement, order: Int): Ast = {
    val code = nodeSignature(doStmt)

    val doNode = newControlStructureNode(doStmt, ControlStructureTypes.DO, code, order)

    val conditionAst = nullSafeAst(doStmt.getCondition, 2)
    val stmtAsts = nullSafeAst(doStmt.getBody, 1)

    val ast = Ast(doNode)
      .withChild(conditionAst)
      .withChildren(stmtAsts)

    conditionAst.root match {
      case Some(r) =>
        ast.withConditionEdge(doNode, r)
      case None =>
        ast
    }
  }

  private def astForSwitchStatement(switchStmt: IASTSwitchStatement, order: Int): Ast = {
    val code = s"switch(${nullSafeCode(switchStmt.getControllerExpression)})"

    val switchNode = newControlStructureNode(switchStmt, ControlStructureTypes.SWITCH, code, order)

    val conditionAst = nullSafeAst(switchStmt.getControllerExpression, 1)
    val stmtAsts = nullSafeAst(switchStmt.getBody, 2)

    val ast = Ast(switchNode)
      .withChild(conditionAst)
      .withChildren(stmtAsts)

    conditionAst.root match {
      case Some(r) =>
        ast.withConditionEdge(switchNode, r)
      case None =>
        ast
    }
  }

  private def astsForCaseStatement(caseStmt: IASTCaseStatement, order: Int): Seq[Ast] = {
    val labelNode = newJumpTarget(caseStmt, order)
    val stmt = nullSafeAst(caseStmt.getExpression, order)
    Seq(Ast(labelNode), stmt)
  }

  private def astForDefaultStatement(caseStmt: IASTDefaultStatement, order: Int): Ast = {
    Ast(newJumpTarget(caseStmt, order))
  }

  private def astForTryStatement(tryStmt: ICPPASTTryBlockStatement, order: Int): Ast = {
    val cpgTry = newControlStructureNode(tryStmt, ControlStructureTypes.TRY, "try", order)
    val body = nullSafeAst(tryStmt.getTryBody, 1)
    val catches = withOrder(tryStmt.getCatchHandlers) { (c, o) =>
      astsForStatement(c.getCatchBody, o + 1)
    }.flatten
    Ast(cpgTry).withChildren(body).withChildren(catches)
  }

  protected def astsForStatement(statement: IASTStatement, order: Int): Seq[Ast] = {
    val r = statement match {
      case expr: IASTExpressionStatement          => Seq(astForExpression(expr.getExpression, order))
      case block: IASTCompoundStatement           => Seq(astForBlockStatement(block, order))
      case ifStmt: IASTIfStatement                => Seq(astForIf(ifStmt, order))
      case whileStmt: IASTWhileStatement          => Seq(astForWhile(whileStmt, order))
      case forStmt: IASTForStatement              => Seq(astForFor(forStmt, order))
      case forStmt: ICPPASTRangeBasedForStatement => Seq(astForRangedFor(forStmt, order))
      case doStmt: IASTDoStatement                => Seq(astForDoStatement(doStmt, order))
      case switchStmt: IASTSwitchStatement        => Seq(astForSwitchStatement(switchStmt, order))
      case ret: IASTReturnStatement               => Seq(astForReturnStatement(ret, order))
      case br: IASTBreakStatement                 => Seq(astForBreakStatement(br, order))
      case cont: IASTContinueStatement            => Seq(astForContinueStatement(cont, order))
      case goto: IASTGotoStatement                => Seq(astForGotoStatement(goto, order))
      case goto: IGNUASTGotoStatement             => astsForGnuGotoStatement(goto, order)
      case defStmt: IASTDefaultStatement          => Seq(astForDefaultStatement(defStmt, order))
      case tryStmt: ICPPASTTryBlockStatement      => Seq(astForTryStatement(tryStmt, order))
      case caseStmt: IASTCaseStatement            => astsForCaseStatement(caseStmt, order)
      case decl: IASTDeclarationStatement         => astsForDeclarationStatement(decl, order)
      case label: IASTLabelStatement              => astsForLabelStatement(label, order)
      case _: IASTNullStatement                   => Seq.empty
      case _                                      => Seq(astForNode(statement, order))
    }
    r.map(x => asChildOfMacroCall(statement, x, order))
  }

  private def astForFor(forStmt: IASTForStatement, order: Int): Ast = {
    val codeInit = nullSafeCode(forStmt.getInitializerStatement)
    val codeCond = nullSafeCode(forStmt.getConditionExpression)
    val codeIter = nullSafeCode(forStmt.getIterationExpression)

    val code = s"for ($codeInit$codeCond;$codeIter)"
    val forNode = newControlStructureNode(forStmt, ControlStructureTypes.FOR, code, order)

    val initAsts = nullSafeAst(forStmt.getInitializerStatement, 1)

    val continuedOrder = Math.max(initAsts.size, 1)
    val compareAst = nullSafeAst(forStmt.getConditionExpression, continuedOrder + 1)
    val updateAst = nullSafeAst(forStmt.getIterationExpression, continuedOrder + 2)
    val stmtAst = nullSafeAst(forStmt.getBody, continuedOrder + 3)

    val ast = Ast(forNode)
      .withChildren(initAsts)
      .withChild(compareAst)
      .withChild(updateAst)
      .withChildren(stmtAst)

    compareAst.root match {
      case Some(c) =>
        ast.withConditionEdge(forNode, c)
      case None => ast
    }
  }

  private def astForRangedFor(forStmt: ICPPASTRangeBasedForStatement, order: Int): Ast = {
    val codeDecl = nullSafeCode(forStmt.getDeclaration)
    val codeInit = nullSafeCode(forStmt.getInitializerClause)

    val code = s"for ($codeDecl:$codeInit)"
    val forNode = newControlStructureNode(forStmt, ControlStructureTypes.FOR, code, order)

    val initAst = astForNode(forStmt.getInitializerClause, 1)
    val declAst = astsForDeclaration(forStmt.getDeclaration, 2)
    val stmtAst = nullSafeAst(forStmt.getBody, 3)

    Ast(forNode)
      .withChild(initAst)
      .withChildren(declAst)
      .withChildren(stmtAst)
  }

  private def astForWhile(whileStmt: IASTWhileStatement, order: Int): Ast = {
    val code = s"while (${nullSafeCode(whileStmt.getCondition)})"

    val whileNode = newControlStructureNode(whileStmt, ControlStructureTypes.WHILE, code, order)

    val conditionAst = nullSafeAst(whileStmt.getCondition, 1)
    val stmtAsts = nullSafeAst(whileStmt.getBody, 2)

    val ast = Ast(whileNode)
      .withChild(conditionAst)
      .withChildren(stmtAsts)

    conditionAst.root match {
      case Some(r) =>
        ast.withConditionEdge(whileNode, r)
      case None =>
        ast
    }
  }

  private def astForIf(ifStmt: IASTIfStatement, order: Int): Ast = {
    val code = s"if (${nullSafeCode(ifStmt.getConditionExpression)})"
    val ifNode = newControlStructureNode(ifStmt, ControlStructureTypes.IF, code, order)

    val conditionAst = nullSafeAst(ifStmt.getConditionExpression, 1)
    val stmtAsts = nullSafeAst(ifStmt.getThenClause, 2)

    val elseChild = if (ifStmt.getElseClause != null) {
      val elseNode = newControlStructureNode(ifStmt.getElseClause, ControlStructureTypes.ELSE, "else", 3)
      val elseAsts = astsForStatement(ifStmt.getElseClause, 1)
      Ast(elseNode).withChildren(elseAsts)
    } else Ast()

    val ast = Ast(ifNode)
      .withChild(conditionAst)
      .withChildren(stmtAsts)
      .withChild(elseChild)

    conditionAst.root match {
      case Some(r) =>
        ast.withConditionEdge(ifNode, r)
      case None =>
        ast
    }
  }

}
