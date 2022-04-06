package io.joern.c2cpg.astcreation

import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.codepropertygraph.generated.nodes.{NewBlock, NewReturn}
import io.joern.x2cpg.Ast
import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.core.dom.ast.cpp._
import org.eclipse.cdt.core.dom.ast.gnu.IGNUASTGotoStatement
import org.eclipse.cdt.internal.core.dom.parser.c.CASTIfStatement
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTIfStatement
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTNamespaceAlias
import org.eclipse.cdt.internal.core.model.ASTStringUtil

trait AstForStatementsCreator {

  this: AstCreator =>

  import AstCreatorHelper.OptionSafeAst

  protected def astForBlockStatement(blockStmt: IASTCompoundStatement, order: Int): Ast = {
    val cpgBlock = NewBlock()
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
        Seq(astForFunctionDeclarator(simplDecl.getDeclarators.head.asInstanceOf[IASTFunctionDeclarator]))
      case simplDecl: IASTSimpleDeclaration =>
        val locals =
          simplDecl.getDeclarators.map(d => astForDeclarator(simplDecl, d)).toList
        val calls =
          withIndex(simplDecl.getDeclarators.filter(_.getInitializer != null)) { (d, o) =>
            astForInitializer(d, d.getInitializer, locals.size + order + o - 1)
          }
        locals ++ calls
      case s: ICPPASTStaticAssertDeclaration         => Seq(astForStaticAssert(s, order))
      case usingDeclaration: ICPPASTUsingDeclaration => handleUsingDeclaration(usingDeclaration)
      case alias: ICPPASTAliasDeclaration            => Seq(astForAliasDeclaration(alias))
      case func: IASTFunctionDefinition              => Seq(astForFunctionDefinition(func))
      case alias: CPPASTNamespaceAlias               => Seq(astForNamespaceAlias(alias))
      case asm: IASTASMDeclaration                   => Seq(astForASMDeclaration(asm, order))
      case _: ICPPASTUsingDirective                  => Seq.empty
      case decl                                      => Seq(astForNode(decl, order))
    }

  private def astForReturnStatement(ret: IASTReturnStatement, order: Int): Ast = {
    val cpgReturn = NewReturn()
      .code(nodeSignature(ret))
      .argumentIndex(order)
      .lineNumber(line(ret))
      .columnNumber(column(ret))
    val expr = nullSafeAst(ret.getReturnValue, 1)
    Ast(cpgReturn).withChild(expr).withArgEdge(cpgReturn, expr.root)
  }

  private def astForBreakStatement(br: IASTBreakStatement): Ast = {
    Ast(newControlStructureNode(br, ControlStructureTypes.BREAK, nodeSignature(br)))
  }

  private def astForContinueStatement(cont: IASTContinueStatement): Ast = {
    Ast(newControlStructureNode(cont, ControlStructureTypes.CONTINUE, nodeSignature(cont)))
  }

  private def astForGotoStatement(goto: IASTGotoStatement): Ast = {
    val code = s"goto ${ASTStringUtil.getSimpleName(goto.getName)};"
    Ast(newControlStructureNode(goto, ControlStructureTypes.GOTO, code))
  }

  private def astsForGnuGotoStatement(goto: IGNUASTGotoStatement, order: Int): Seq[Ast] = {
    // This is for GNU GOTO labels as values.
    // See: https://gcc.gnu.org/onlinedocs/gcc/Labels-as-Values.html
    // For such GOTOs we cannot statically determine the target label. As a quick
    // hack we simply put edges to all labels found indicated by *. This might be an over-taint.
    val code     = s"goto *;"
    val gotoNode = Ast(newControlStructureNode(goto, ControlStructureTypes.GOTO, code))
    val exprNode = nullSafeAst(goto.getLabelNameExpression, order + 1)
    Seq(gotoNode, exprNode)
  }

  private def astsForLabelStatement(label: IASTLabelStatement, order: Int): Seq[Ast] = {
    val cpgLabel    = newJumpTarget(label, order)
    val nestedStmts = nullSafeAst(label.getNestedStatement, order + 1)
    Ast(cpgLabel) +: nestedStmts
  }

  private def astForDoStatement(doStmt: IASTDoStatement): Ast = {
    val code = nodeSignature(doStmt)

    val doNode = newControlStructureNode(doStmt, ControlStructureTypes.DO, code)

    val conditionAst = nullSafeAst(doStmt.getCondition, 2)
    val stmtAsts     = nullSafeAst(doStmt.getBody, 1)

    Ast(doNode)
      .withChildren(stmtAsts)
      .withChild(conditionAst)
      .withConditionEdge(doNode, conditionAst.root)
  }

  private def astForSwitchStatement(switchStmt: IASTSwitchStatement): Ast = {
    val code = s"switch(${nullSafeCode(switchStmt.getControllerExpression)})"

    val switchNode = newControlStructureNode(switchStmt, ControlStructureTypes.SWITCH, code)

    val conditionAst = nullSafeAst(switchStmt.getControllerExpression, 1)
    val stmtAsts     = nullSafeAst(switchStmt.getBody, 2)

    Ast(switchNode)
      .withChild(conditionAst)
      .withChildren(stmtAsts)
      .withConditionEdge(switchNode, conditionAst.root)
  }

  private def astsForCaseStatement(caseStmt: IASTCaseStatement, order: Int): Seq[Ast] = {
    val labelNode = newJumpTarget(caseStmt, order)
    val stmt      = nullSafeAst(caseStmt.getExpression, order)
    Seq(Ast(labelNode), stmt)
  }

  private def astForDefaultStatement(caseStmt: IASTDefaultStatement, order: Int): Ast = {
    Ast(newJumpTarget(caseStmt, order))
  }

  private def astForTryStatement(tryStmt: ICPPASTTryBlockStatement): Ast = {
    val cpgTry = newControlStructureNode(tryStmt, ControlStructureTypes.TRY, "try")
    val body   = nullSafeAst(tryStmt.getTryBody, 1)
    // All catches must have order 2 for correct control flow generation.
    val catches = tryStmt.getCatchHandlers.flatMap { stmt =>
      astsForStatement(stmt.getCatchBody, 2)
    }.toIndexedSeq
    Ast(cpgTry).withChildren(body).withChildren(catches)
  }

  protected def astsForStatement(statement: IASTStatement, argIndex: Int): Seq[Ast] = {
    val r = statement match {
      case expr: IASTExpressionStatement          => Seq(astForExpression(expr.getExpression, argIndex))
      case block: IASTCompoundStatement           => Seq(astForBlockStatement(block, argIndex))
      case ifStmt: IASTIfStatement                => Seq(astForIf(ifStmt))
      case whileStmt: IASTWhileStatement          => Seq(astForWhile(whileStmt))
      case forStmt: IASTForStatement              => Seq(astForFor(forStmt))
      case forStmt: ICPPASTRangeBasedForStatement => Seq(astForRangedFor(forStmt))
      case doStmt: IASTDoStatement                => Seq(astForDoStatement(doStmt))
      case switchStmt: IASTSwitchStatement        => Seq(astForSwitchStatement(switchStmt))
      case ret: IASTReturnStatement               => Seq(astForReturnStatement(ret, argIndex))
      case br: IASTBreakStatement                 => Seq(astForBreakStatement(br))
      case cont: IASTContinueStatement            => Seq(astForContinueStatement(cont))
      case goto: IASTGotoStatement                => Seq(astForGotoStatement(goto))
      case goto: IGNUASTGotoStatement             => astsForGnuGotoStatement(goto, argIndex)
      case defStmt: IASTDefaultStatement          => Seq(astForDefaultStatement(defStmt, argIndex))
      case tryStmt: ICPPASTTryBlockStatement      => Seq(astForTryStatement(tryStmt))
      case caseStmt: IASTCaseStatement            => astsForCaseStatement(caseStmt, argIndex)
      case decl: IASTDeclarationStatement         => astsForDeclarationStatement(decl, argIndex)
      case label: IASTLabelStatement              => astsForLabelStatement(label, argIndex)
      case _: IASTNullStatement                   => Seq.empty
      case _                                      => Seq(astForNode(statement, argIndex))
    }
    r.map(x => asChildOfMacroCall(statement, x, argIndex))
  }

  private def astForFor(forStmt: IASTForStatement): Ast = {
    val codeInit = nullSafeCode(forStmt.getInitializerStatement)
    val codeCond = nullSafeCode(forStmt.getConditionExpression)
    val codeIter = nullSafeCode(forStmt.getIterationExpression)

    val code    = s"for ($codeInit$codeCond;$codeIter)"
    val forNode = newControlStructureNode(forStmt, ControlStructureTypes.FOR, code)

    val initAstBlock = NewBlock()
      .argumentIndex(1)
      .typeFullName(registerType(Defines.voidTypeName))
      .lineNumber(line(forStmt))
      .columnNumber(column(forStmt))
    scope.pushNewScope(initAstBlock)
    val initAst = Ast(initAstBlock).withChildren(nullSafeAst(forStmt.getInitializerStatement, 1))
    scope.popScope()

    val compareAst = nullSafeAst(forStmt.getConditionExpression, 2)
    val updateAst  = nullSafeAst(forStmt.getIterationExpression, 3)
    val stmtAst    = nullSafeAst(forStmt.getBody, 4)

    Ast(forNode)
      .withChild(initAst)
      .withChild(compareAst)
      .withChild(updateAst)
      .withChildren(stmtAst)
      .withConditionEdge(forNode, compareAst.root)
  }

  private def astForRangedFor(forStmt: ICPPASTRangeBasedForStatement): Ast = {
    val codeDecl = nullSafeCode(forStmt.getDeclaration)
    val codeInit = nullSafeCode(forStmt.getInitializerClause)

    val code    = s"for ($codeDecl:$codeInit)"
    val forNode = newControlStructureNode(forStmt, ControlStructureTypes.FOR, code)

    val initAst = astForNode(forStmt.getInitializerClause, 1)
    val declAst = astsForDeclaration(forStmt.getDeclaration, 2)
    val stmtAst = nullSafeAst(forStmt.getBody, 3)

    Ast(forNode)
      .withChild(initAst)
      .withChildren(declAst)
      .withChildren(stmtAst)
  }

  private def astForWhile(whileStmt: IASTWhileStatement): Ast = {
    val code = s"while (${nullSafeCode(whileStmt.getCondition)})"

    val whileNode = newControlStructureNode(whileStmt, ControlStructureTypes.WHILE, code)

    val conditionAst = nullSafeAst(whileStmt.getCondition, 1)
    val stmtAsts     = nullSafeAst(whileStmt.getBody, 2)

    Ast(whileNode)
      .withChild(conditionAst)
      .withChildren(stmtAsts)
      .withConditionEdge(whileNode, conditionAst.root)
  }

  private def astForIf(ifStmt: IASTIfStatement): Ast = {
    val (code, conditionAst) = ifStmt match {
      case s: CASTIfStatement =>
        val c = s"if (${nullSafeCode(s.getConditionExpression)})"
        val a = nullSafeAst(ifStmt.getConditionExpression, 1)
        (c, a)
      case s: CPPASTIfStatement if s.getConditionExpression != null =>
        val c = s"if (${nullSafeCode(s.getConditionExpression)})"
        val a = nullSafeAst(ifStmt.getConditionExpression, 1)
        (c, a)
      case s: CPPASTIfStatement if s.getConditionExpression == null =>
        val c = s"if (${nullSafeCode(s.getConditionDeclaration)})"
        val exprBlock = NewBlock()
          .argumentIndex(1)
          .typeFullName(registerType(Defines.voidTypeName))
          .lineNumber(line(s.getConditionDeclaration))
          .columnNumber(column(s.getConditionDeclaration))
        scope.pushNewScope(exprBlock)
        val a        = astsForDeclaration(s.getConditionDeclaration, 1)
        val blockAst = Ast(exprBlock).withChildren(a)
        scope.popScope()
        (c, blockAst)
    }

    val ifNode   = newControlStructureNode(ifStmt, ControlStructureTypes.IF, code)
    val stmtAsts = nullSafeAst(ifStmt.getThenClause, 2)

    val elseChild = if (ifStmt.getElseClause != null) {
      val elseNode = newControlStructureNode(ifStmt.getElseClause, ControlStructureTypes.ELSE, "else")
      val elseAsts = astsForStatement(ifStmt.getElseClause, 1)
      Ast(elseNode).withChildren(elseAsts)
    } else Ast()

    Ast(ifNode)
      .withChild(conditionAst)
      .withChildren(stmtAsts)
      .withChild(elseChild)
      .withConditionEdge(ifNode, conditionAst.root)
  }

}
