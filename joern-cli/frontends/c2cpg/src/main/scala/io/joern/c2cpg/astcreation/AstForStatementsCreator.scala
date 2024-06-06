package io.joern.c2cpg.astcreation

import io.joern.c2cpg.parser.CdtParser
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.AstNodeNew
import io.shiftleft.codepropertygraph.generated.nodes.ExpressionNew
import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.core.dom.ast.cpp.*
import org.eclipse.cdt.core.dom.ast.gnu.IGNUASTGotoStatement
import org.eclipse.cdt.internal.core.dom.parser.c.CASTIfStatement
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTIfStatement
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTNamespaceAlias
import org.eclipse.cdt.internal.core.model.ASTStringUtil

import java.nio.file.Paths

trait AstForStatementsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  import io.joern.c2cpg.astcreation.AstCreatorHelper.OptionSafeAst

  protected def astForBlockStatement(blockStmt: IASTCompoundStatement, order: Int = -1): Ast = {
    val codeString = code(blockStmt)
    val blockCode  = if (codeString == "{}" || codeString.isEmpty) Defines.empty else codeString
    val node = blockNode(blockStmt, blockCode, registerType(Defines.voidTypeName))
      .order(order)
      .argumentIndex(order)
    scope.pushNewScope(node)
    var currOrder = 1
    val childAsts = blockStmt.getStatements.flatMap { stmt =>
      val r = astsForStatement(stmt, currOrder)
      currOrder = currOrder + r.length
      r
    }
    scope.popScope()
    blockAst(node, childAsts.toList)
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
      case declaration                               => Seq(astForNode(declaration))
    }

  private def astForReturnStatement(ret: IASTReturnStatement): Ast = {
    val cpgReturn = returnNode(ret, code(ret))
    val expr      = nullSafeAst(ret.getReturnValue)
    Ast(cpgReturn).withChild(expr).withArgEdge(cpgReturn, expr.root)
  }

  private def astForBreakStatement(br: IASTBreakStatement): Ast = {
    Ast(controlStructureNode(br, ControlStructureTypes.BREAK, code(br)))
  }

  private def astForContinueStatement(cont: IASTContinueStatement): Ast = {
    Ast(controlStructureNode(cont, ControlStructureTypes.CONTINUE, code(cont)))
  }

  private def astForGotoStatement(goto: IASTGotoStatement): Ast = {
    val code = s"goto ${ASTStringUtil.getSimpleName(goto.getName)};"
    Ast(controlStructureNode(goto, ControlStructureTypes.GOTO, code))
  }

  private def astsForGnuGotoStatement(goto: IGNUASTGotoStatement): Seq[Ast] = {
    // This is for GNU GOTO labels as values.
    // See: https://gcc.gnu.org/onlinedocs/gcc/Labels-as-Values.html
    // For such GOTOs we cannot statically determine the target label. As a quick
    // hack we simply put edges to all labels found indicated by *. This might be an over-taint.
    val code     = s"goto *;"
    val gotoNode = Ast(controlStructureNode(goto, ControlStructureTypes.GOTO, code))
    val exprNode = nullSafeAst(goto.getLabelNameExpression)
    Seq(gotoNode, exprNode)
  }

  private def astsForLabelStatement(label: IASTLabelStatement): Seq[Ast] = {
    val cpgLabel    = newJumpTargetNode(label)
    val nestedStmts = nullSafeAst(label.getNestedStatement)
    Ast(cpgLabel) +: nestedStmts
  }

  private def astForDoStatement(doStmt: IASTDoStatement): Ast = {
    val codeString   = code(doStmt)
    val doNode       = controlStructureNode(doStmt, ControlStructureTypes.DO, codeString)
    val conditionAst = astForConditionExpression(doStmt.getCondition)
    val bodyAst      = nullSafeAst(doStmt.getBody)
    controlStructureAst(doNode, Option(conditionAst), bodyAst, placeConditionLast = true)
  }

  private def astForSwitchStatement(switchStmt: IASTSwitchStatement): Ast = {
    val code         = s"switch(${nullSafeCode(switchStmt.getControllerExpression)})"
    val switchNode   = controlStructureNode(switchStmt, ControlStructureTypes.SWITCH, code)
    val conditionAst = astForConditionExpression(switchStmt.getControllerExpression)
    val stmtAsts     = nullSafeAst(switchStmt.getBody)
    controlStructureAst(switchNode, Option(conditionAst), stmtAsts)
  }

  private def astsForCaseStatement(caseStmt: IASTCaseStatement): Seq[Ast] = {
    val labelNode = newJumpTargetNode(caseStmt)
    val stmt      = astForConditionExpression(caseStmt.getExpression)
    Seq(Ast(labelNode), stmt)
  }

  private def astForDefaultStatement(caseStmt: IASTDefaultStatement): Ast = {
    Ast(newJumpTargetNode(caseStmt))
  }

  private def astForTryStatement(tryStmt: ICPPASTTryBlockStatement): Ast = {
    val tryNode = controlStructureNode(tryStmt, ControlStructureTypes.TRY, "try")
    val bodyAst = nullSafeAst(tryStmt.getTryBody) match {
      case Nil         => Ast()
      case elem :: Nil => elem
      case elements =>
        setArgumentIndices(elements)
        blockAst(blockNode(tryStmt.getTryBody)).withChildren(elements)
    }
    val catchAsts = tryStmt.getCatchHandlers.toSeq.map(astForCatchHandler)
    tryCatchAst(tryNode, bodyAst, catchAsts, None)
  }

  private def astForCatchHandler(catchHandler: ICPPASTCatchHandler): Ast = {
    val catchNode = controlStructureNode(catchHandler, ControlStructureTypes.CATCH, "catch")
    val declAst   = nullSafeAst(catchHandler.getDeclaration)
    val bodyAst   = nullSafeAst(catchHandler.getCatchBody)
    Ast(catchNode).withChildren(declAst).withChildren(bodyAst)
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
      case problem: IASTProblemStatement          => astsForProblemStatement(problem)
      case _: IASTNullStatement                   => Seq.empty
      case _                                      => Seq(astForNode(statement))
    }
    r.map(x => asChildOfMacroCall(statement, x))
  }

  private def astsForProblemStatement(statement: IASTProblemStatement): Seq[Ast] = {
    val lineNumber   = line(statement)
    val columnNumber = column(statement)
    // We only handle un-parsable macros here for now
    val isFromMacroExpansion = statement.getProblem.getNodeLocations.exists(_.isInstanceOf[IASTMacroExpansionLocation])
    val asts = if (isFromMacroExpansion) {
      new CdtParser(config).parse(statement.getRawSignature, Paths.get(statement.getContainingFilename)) match
        case Some(node) => node.getDeclarations.toIndexedSeq.flatMap(astsForDeclaration)
        case None       => Seq.empty
    } else {
      Seq.empty
    }
    // Restore the line/column numbers relative to the statements position
    asts.flatMap(_.nodes).foreach {
      case astNodeNew: AstNodeNew =>
        astNodeNew.lineNumber = (lineNumber ++ astNodeNew.lineNumber).reduceOption { case (a, b) => a + (b - 1) }
        astNodeNew.columnNumber = (columnNumber ++ astNodeNew.columnNumber).reduceOption(_ + _)
      case _ => // do nothing
    }
    asts
  }

  private def astForConditionExpression(expression: IASTExpression, explicitArgumentIndex: Option[Int] = None): Ast = {
    val ast = expression match {
      case exprList: IASTExpressionList =>
        val compareAstBlock = blockNode(expression, Defines.empty, registerType(Defines.voidTypeName))
        scope.pushNewScope(compareAstBlock)
        val compareBlockAstChildren = exprList.getExpressions.toList.map(nullSafeAst)
        setArgumentIndices(compareBlockAstChildren)
        val compareBlockAst = blockAst(compareAstBlock, compareBlockAstChildren)
        scope.popScope()
        compareBlockAst
      case other =>
        nullSafeAst(other)
    }
    explicitArgumentIndex.foreach { i =>
      ast.root.foreach { case expr: ExpressionNew => expr.argumentIndex = i }
    }
    ast
  }

  private def astForFor(forStmt: IASTForStatement): Ast = {
    val codeInit = nullSafeCode(forStmt.getInitializerStatement)
    val codeCond = nullSafeCode(forStmt.getConditionExpression)
    val codeIter = nullSafeCode(forStmt.getIterationExpression)

    val code    = s"for ($codeInit$codeCond;$codeIter)"
    val forNode = controlStructureNode(forStmt, ControlStructureTypes.FOR, code)

    val initAstBlock = blockNode(forStmt, Defines.empty, registerType(Defines.voidTypeName))
    scope.pushNewScope(initAstBlock)
    val initAst = blockAst(initAstBlock, nullSafeAst(forStmt.getInitializerStatement, 1).toList)
    scope.popScope()

    val compareAst = astForConditionExpression(forStmt.getConditionExpression, Option(2))
    val updateAst  = nullSafeAst(forStmt.getIterationExpression, 3)
    val bodyAsts   = nullSafeAst(forStmt.getBody, 4)
    forAst(forNode, Seq(), Seq(initAst), Seq(compareAst), Seq(updateAst), bodyAsts)
  }

  private def astForRangedFor(forStmt: ICPPASTRangeBasedForStatement): Ast = {
    val codeDecl = nullSafeCode(forStmt.getDeclaration)
    val codeInit = nullSafeCode(forStmt.getInitializerClause)

    val code    = s"for ($codeDecl:$codeInit)"
    val forNode = controlStructureNode(forStmt, ControlStructureTypes.FOR, code)

    val initAst = astForNode(forStmt.getInitializerClause)
    val declAst = astsForDeclaration(forStmt.getDeclaration)
    val stmtAst = nullSafeAst(forStmt.getBody)
    controlStructureAst(forNode, None, Seq(initAst) ++ declAst ++ stmtAst)
  }

  private def astForWhile(whileStmt: IASTWhileStatement): Ast = {
    val code       = s"while (${nullSafeCode(whileStmt.getCondition)})"
    val compareAst = astForConditionExpression(whileStmt.getCondition)
    val bodyAst    = nullSafeAst(whileStmt.getBody)
    whileAst(
      Option(compareAst),
      bodyAst,
      code = Option(code),
      lineNumber = line(whileStmt),
      columnNumber = column(whileStmt)
    )
  }

  private def astForIf(ifStmt: IASTIfStatement): Ast = {
    val (code, conditionAst) = ifStmt match {
      case s @ (_: CASTIfStatement | _: CPPASTIfStatement) if s.getConditionExpression != null =>
        val c          = s"if (${nullSafeCode(s.getConditionExpression)})"
        val compareAst = astForConditionExpression(s.getConditionExpression)
        (c, compareAst)
      case s: CPPASTIfStatement if s.getConditionExpression == null =>
        val c         = s"if (${nullSafeCode(s.getConditionDeclaration)})"
        val exprBlock = blockNode(s.getConditionDeclaration, Defines.empty, Defines.voidTypeName)
        scope.pushNewScope(exprBlock)
        val a = astsForDeclaration(s.getConditionDeclaration)
        setArgumentIndices(a)
        scope.popScope()
        (c, blockAst(exprBlock, a.toList))
    }

    val ifNode = controlStructureNode(ifStmt, ControlStructureTypes.IF, code)

    val thenAst = ifStmt.getThenClause match {
      case block: IASTCompoundStatement => astForBlockStatement(block)
      case other if other != null =>
        val thenBlock = blockNode(other, Defines.empty, Defines.voidTypeName)
        scope.pushNewScope(thenBlock)
        val a = astsForStatement(other)
        setArgumentIndices(a)
        scope.popScope()
        blockAst(thenBlock, a.toList)
      case _ => Ast()
    }

    val elseAst = ifStmt.getElseClause match {
      case block: IASTCompoundStatement =>
        val elseNode = controlStructureNode(ifStmt.getElseClause, ControlStructureTypes.ELSE, "else")
        val elseAst  = astForBlockStatement(block)
        Ast(elseNode).withChild(elseAst)
      case other if other != null =>
        val elseNode  = controlStructureNode(ifStmt.getElseClause, ControlStructureTypes.ELSE, "else")
        val elseBlock = blockNode(other, Defines.empty, Defines.voidTypeName)
        scope.pushNewScope(elseBlock)
        val a = astsForStatement(other)
        setArgumentIndices(a)
        scope.popScope()
        Ast(elseNode).withChild(blockAst(elseBlock, a.toList))
      case _ => Ast()
    }
    controlStructureAst(ifNode, Option(conditionAst), Seq(thenAst, elseAst))
  }
}
