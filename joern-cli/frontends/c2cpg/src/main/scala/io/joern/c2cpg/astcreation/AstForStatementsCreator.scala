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

  protected def astForBlockStatement(blockStmt: IASTCompoundStatement, order: Int = -1): Ast = {
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

  private def astsForDeclarationStatement(decl: IASTDeclarationStatement): Seq[Ast] =
    decl.getDeclaration match {
      case simplDecl: IASTSimpleDeclaration
          if simplDecl.getDeclarators.headOption.exists(_.isInstanceOf[IASTFunctionDeclarator]) =>
        Seq(astForFunctionDeclarator(simplDecl.getDeclarators.head.asInstanceOf[IASTFunctionDeclarator]))
      case simplDecl: IASTSimpleDeclaration =>
        val locals =
          simplDecl.getDeclarators.zipWithIndex.toList.map { case (d, i) => astForDeclarator(simplDecl, d, i) }
        val calls =
          simplDecl.getDeclarators.filter(_.getInitializer != null).toList.map { d =>
            astForInitializer(d, d.getInitializer)
          }
        locals ++ calls
      case s: ICPPASTStaticAssertDeclaration         => Seq(astForStaticAssert(s))
      case usingDeclaration: ICPPASTUsingDeclaration => handleUsingDeclaration(usingDeclaration)
      case alias: ICPPASTAliasDeclaration            => Seq(astForAliasDeclaration(alias))
      case func: IASTFunctionDefinition              => Seq(astForFunctionDefinition(func))
      case alias: CPPASTNamespaceAlias               => Seq(astForNamespaceAlias(alias))
      case asm: IASTASMDeclaration                   => Seq(astForASMDeclaration(asm))
      case _: ICPPASTUsingDirective                  => Seq.empty
      case decl                                      => Seq(astForNode(decl))
    }

  private def astForReturnStatement(ret: IASTReturnStatement): Ast = {
    val cpgReturn = NewReturn()
      .code(nodeSignature(ret))
      .lineNumber(line(ret))
      .columnNumber(column(ret))
    val expr = nullSafeAst(ret.getReturnValue)
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

  private def astsForGnuGotoStatement(goto: IGNUASTGotoStatement): Seq[Ast] = {
    // This is for GNU GOTO labels as values.
    // See: https://gcc.gnu.org/onlinedocs/gcc/Labels-as-Values.html
    // For such GOTOs we cannot statically determine the target label. As a quick
    // hack we simply put edges to all labels found indicated by *. This might be an over-taint.
    val code     = s"goto *;"
    val gotoNode = Ast(newControlStructureNode(goto, ControlStructureTypes.GOTO, code))
    val exprNode = nullSafeAst(goto.getLabelNameExpression)
    Seq(gotoNode, exprNode)
  }

  private def astsForLabelStatement(label: IASTLabelStatement): Seq[Ast] = {
    val cpgLabel    = newJumpTarget(label)
    val nestedStmts = nullSafeAst(label.getNestedStatement)
    Ast(cpgLabel) +: nestedStmts
  }

  private def astForDoStatement(doStmt: IASTDoStatement): Ast = {
    val code = nodeSignature(doStmt)

    val doNode = newControlStructureNode(doStmt, ControlStructureTypes.DO, code)

    val conditionAst = nullSafeAst(doStmt.getCondition)
    val stmtAsts     = nullSafeAst(doStmt.getBody)

    Ast(doNode)
      .withChildren(stmtAsts)
      .withChild(conditionAst)
      .withConditionEdge(doNode, conditionAst.root)
  }

  private def astForSwitchStatement(switchStmt: IASTSwitchStatement): Ast = {
    val code = s"switch(${nullSafeCode(switchStmt.getControllerExpression)})"

    val switchNode = newControlStructureNode(switchStmt, ControlStructureTypes.SWITCH, code)

    val conditionAst = nullSafeAst(switchStmt.getControllerExpression)
    val stmtAsts     = nullSafeAst(switchStmt.getBody)

    Ast(switchNode)
      .withChild(conditionAst)
      .withChildren(stmtAsts)
      .withConditionEdge(switchNode, conditionAst.root)
  }

  private def astsForCaseStatement(caseStmt: IASTCaseStatement): Seq[Ast] = {
    val labelNode = newJumpTarget(caseStmt)
    val stmt      = nullSafeAst(caseStmt.getExpression)
    Seq(Ast(labelNode), stmt)
  }

  private def astForDefaultStatement(caseStmt: IASTDefaultStatement): Ast = {
    Ast(newJumpTarget(caseStmt))
  }

  private def astForTryStatement(tryStmt: ICPPASTTryBlockStatement): Ast = {
    val cpgTry = newControlStructureNode(tryStmt, ControlStructureTypes.TRY, "try")
    val body   = nullSafeAst(tryStmt.getTryBody)
    // All catches must have order 2 for correct control flow generation.
    // TODO fix this. Multiple siblings with the same order are invalid
    val catches = tryStmt.getCatchHandlers.flatMap { stmt =>
      astsForStatement(stmt.getCatchBody, 2)
    }.toIndexedSeq
    Ast(cpgTry).withChildren(body).withChildren(catches)
  }

  protected def astsForStatement(statement: IASTStatement, argIndex: Int = -1): Seq[Ast] = {
    val r = statement match {
      case expr: IASTExpressionStatement          => Seq(astForExpression(expr.getExpression))
      case block: IASTCompoundStatement           => Seq(astForBlockStatement(block, argIndex))
      case ifStmt: IASTIfStatement                => Seq(astForIf(ifStmt))
      case whileStmt: IASTWhileStatement          => Seq(astForWhile(whileStmt))
      case forStmt: IASTForStatement              => Seq(astForFor(forStmt))
      case forStmt: ICPPASTRangeBasedForStatement => Seq(astForRangedFor(forStmt))
      case doStmt: IASTDoStatement                => Seq(astForDoStatement(doStmt))
      case switchStmt: IASTSwitchStatement        => Seq(astForSwitchStatement(switchStmt))
      case ret: IASTReturnStatement               => Seq(astForReturnStatement(ret))
      case br: IASTBreakStatement                 => Seq(astForBreakStatement(br))
      case cont: IASTContinueStatement            => Seq(astForContinueStatement(cont))
      case goto: IASTGotoStatement                => Seq(astForGotoStatement(goto))
      case goto: IGNUASTGotoStatement             => astsForGnuGotoStatement(goto)
      case defStmt: IASTDefaultStatement          => Seq(astForDefaultStatement(defStmt))
      case tryStmt: ICPPASTTryBlockStatement      => Seq(astForTryStatement(tryStmt))
      case caseStmt: IASTCaseStatement            => astsForCaseStatement(caseStmt)
      case decl: IASTDeclarationStatement         => astsForDeclarationStatement(decl)
      case label: IASTLabelStatement              => astsForLabelStatement(label)
      case _: IASTNullStatement                   => Seq.empty
      case _                                      => Seq(astForNode(statement))
    }
    r.map(x => asChildOfMacroCall(statement, x))
  }

  private def astForFor(forStmt: IASTForStatement): Ast = {
    val codeInit = nullSafeCode(forStmt.getInitializerStatement)
    val codeCond = nullSafeCode(forStmt.getConditionExpression)
    val codeIter = nullSafeCode(forStmt.getIterationExpression)

    val code    = s"for ($codeInit$codeCond;$codeIter)"
    val forNode = newControlStructureNode(forStmt, ControlStructureTypes.FOR, code)

    val initAstBlock = NewBlock()
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

    val initAst = astForNode(forStmt.getInitializerClause)
    val declAst = astsForDeclaration(forStmt.getDeclaration)
    val stmtAst = nullSafeAst(forStmt.getBody)

    Ast(forNode)
      .withChild(initAst)
      .withChildren(declAst)
      .withChildren(stmtAst)
  }

  private def astForWhile(whileStmt: IASTWhileStatement): Ast = {
    val code = s"while (${nullSafeCode(whileStmt.getCondition)})"

    val whileNode = newControlStructureNode(whileStmt, ControlStructureTypes.WHILE, code)

    val conditionAst = nullSafeAst(whileStmt.getCondition)
    val stmtAsts     = nullSafeAst(whileStmt.getBody)

    Ast(whileNode)
      .withChild(conditionAst)
      .withChildren(stmtAsts)
      .withConditionEdge(whileNode, conditionAst.root)
  }

  private def astForIf(ifStmt: IASTIfStatement): Ast = {
    val (code, conditionAst) = ifStmt match {
      case s: CASTIfStatement =>
        val c = s"if (${nullSafeCode(s.getConditionExpression)})"
        val a = nullSafeAst(ifStmt.getConditionExpression)
        (c, a)
      case s: CPPASTIfStatement if s.getConditionExpression != null =>
        val c = s"if (${nullSafeCode(s.getConditionExpression)})"
        val a = nullSafeAst(ifStmt.getConditionExpression)
        (c, a)
      case s: CPPASTIfStatement if s.getConditionExpression == null =>
        val c = s"if (${nullSafeCode(s.getConditionDeclaration)})"
        val exprBlock = NewBlock()
          .typeFullName(registerType(Defines.voidTypeName))
          .lineNumber(line(s.getConditionDeclaration))
          .columnNumber(column(s.getConditionDeclaration))
        scope.pushNewScope(exprBlock)
        val a        = astsForDeclaration(s.getConditionDeclaration)
        val blockAst = Ast(exprBlock).withChildren(a)
        scope.popScope()
        (c, blockAst)
    }

    val ifNode = newControlStructureNode(ifStmt, ControlStructureTypes.IF, code)

    val thenAst = ifStmt.getThenClause match {
      case block: IASTCompoundStatement => astForBlockStatement(block)
      case other if other != null =>
        val thenBlock = NewBlock()
          .typeFullName(registerType(Defines.voidTypeName))
          .lineNumber(line(other))
          .columnNumber(column(other))
        scope.pushNewScope(thenBlock)
        val a        = astsForStatement(other)
        val blockAst = Ast(thenBlock).withChildren(a)
        scope.popScope()
        blockAst
      case _ => Ast()
    }

    val elseAst = ifStmt.getElseClause match {
      case block: IASTCompoundStatement =>
        val elseNode = newControlStructureNode(ifStmt.getElseClause, ControlStructureTypes.ELSE, "else")
        val elseAst  = astForBlockStatement(block)
        Ast(elseNode).withChild(elseAst)
      case other if other != null =>
        val elseNode = newControlStructureNode(ifStmt.getElseClause, ControlStructureTypes.ELSE, "else")
        val elseBlock = NewBlock()
          .typeFullName(registerType(Defines.voidTypeName))
          .lineNumber(line(other))
          .columnNumber(column(other))
        scope.pushNewScope(elseBlock)
        val a        = astsForStatement(other)
        val blockAst = Ast(elseBlock).withChildren(a)
        scope.popScope()
        Ast(elseNode).withChild(blockAst)
      case _ => Ast()
    }

    Ast(ifNode)
      .withChild(conditionAst)
      .withChild(thenAst)
      .withChild(elseAst)
      .withConditionEdge(ifNode, conditionAst.root)
  }

}
