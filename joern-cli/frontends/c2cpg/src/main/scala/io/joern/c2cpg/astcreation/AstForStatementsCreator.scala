package io.joern.c2cpg.astcreation

import io.joern.c2cpg.parser.CdtParser
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.codepropertygraph.generated.nodes.AstNodeNew
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.NewBlock
import io.shiftleft.codepropertygraph.generated.nodes.NewCall
import io.shiftleft.codepropertygraph.generated.nodes.NewLocal
import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.core.dom.ast.cpp.*
import org.eclipse.cdt.core.dom.ast.gnu.IGNUASTGotoStatement
import org.eclipse.cdt.internal.core.dom.parser.c.CASTIfStatement
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTIfStatement
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTNamespaceAlias
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTSimpleDeclaration
import org.eclipse.cdt.internal.core.model.ASTStringUtil

import java.nio.file.Paths
import scala.collection.mutable

trait AstForStatementsCreator { this: AstCreator =>

  protected def astForBlockStatement(blockStmt: IASTCompoundStatement, blockNode: NewBlock, order: Int = -1): Ast = {
    val codeString  = code(blockStmt)
    val blockLine   = line(blockStmt)
    val blockColumn = column(blockStmt)
    val node = blockNode
      .order(order)
      .argumentIndex(order)
      .code(codeString)
      .lineNumber(blockLine)
      .columnNumber(blockColumn)
      .typeFullName(registerType(Defines.Void))
    scope.pushNewBlockScope(node)
    var currOrder = 1
    val childAsts = blockStmt.getStatements.flatMap { stmt =>
      val r = astsForStatement(stmt, currOrder)
      currOrder = currOrder + r.length
      r
    }
    scope.popScope()
    blockAst(node, childAsts.toList)
  }

  protected def astsForStatement(statement: IASTStatement, argIndex: Int = -1): Seq[Ast] = {
    val r = statement match {
      case expr: IASTExpressionStatement          => Seq(astForExpression(expr.getExpression))
      case block: IASTCompoundStatement           => Seq(astForBlockStatement(block, blockNode(block), argIndex))
      case ifStmt: IASTIfStatement                => astForIf(ifStmt)
      case whileStmt: IASTWhileStatement          => Seq(astForWhile(whileStmt))
      case forStmt: IASTForStatement              => Seq(astForFor(forStmt))
      case forStmt: ICPPASTRangeBasedForStatement => Seq(astForRangedFor(forStmt))
      case doStmt: IASTDoStatement                => Seq(astForDoStatement(doStmt))
      case switchStmt: IASTSwitchStatement        => astForSwitchStatement(switchStmt)
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

  private def hasValidArrayModifier(arrayDecl: IASTArrayDeclarator): Boolean = {
    arrayDecl.getArrayModifiers.nonEmpty && arrayDecl.getArrayModifiers.forall(_.getConstantExpression != null)
  }

  private def astsForStructuredBindingDeclaration(
    struct: ICPPASTStructuredBindingDeclaration,
    init: Option[IASTInitializerClause] = None
  ): Seq[Ast] = {
    def leftAst(astName: IASTNode, localName: String, codeString: String, tpe: String): (NewCall, NewLocal, Ast) = {
      val op                 = Operators.assignment
      val assignmentCode     = s"$localName = $codeString"
      val assignmentCallNode = callNode(astName, assignmentCode, op, op, DispatchTypes.STATIC_DISPATCH, None, Some(tpe))
      val localNameNode      = localNode(astName, localName, localName, tpe)
      scope.addVariable(localName, localNameNode, tpe, C2CpgScope.ScopeType.BlockScope)
      val localId = identifierNode(astName, code(astName), code(astName), tpe)
      val leftAst = Ast(localId).withRefEdge(localId, localNameNode)
      (assignmentCallNode, localNameNode, leftAst)
    }

    val initializer  = init.getOrElse(struct.getInitializer)
    val tmpName      = uniqueName("", "", "tmp")._1
    val tpe          = registerType(typeFor(initializer))
    val localTmpNode = localNode(struct, tmpName, tmpName, tpe)
    scope.addVariable(tmpName, localTmpNode, tpe, C2CpgScope.ScopeType.BlockScope)

    val idNode             = identifierNode(struct, tmpName, tmpName, tpe)
    val rhsAst             = astForNode(initializer)
    val op                 = Operators.assignment
    val assignmentCode     = s"$tmpName = ${code(initializer)}"
    val assignmentCallNode = callNode(struct, assignmentCode, op, op, DispatchTypes.STATIC_DISPATCH, None, Some(tpe))
    val assignmentCallAst  = callAst(assignmentCallNode, List(Ast(idNode).withRefEdge(idNode, localTmpNode), rhsAst))

    val accessAsts = if typeFor(initializer).endsWith("]") then {
      struct.getNames.zipWithIndex.flatMap { case (astName, index) =>
        val localName                               = code(astName)
        val tpe                                     = registerType(typeFor(astName))
        val codeString                              = s"$tmpName[$index]"
        val (assignmentCallNode, localNode, lhsAst) = leftAst(astName, localName, codeString, tpe)
        val op                                      = Operators.indexAccess
        val arrayIndexCallNode = callNode(astName, codeString, op, op, DispatchTypes.STATIC_DISPATCH, None, Some(tpe))
        val idNode             = identifierNode(astName, tmpName, tmpName, tpe)
        val indexNode          = literalNode(astName, index.toString, registerType("int"))
        val arrayIndexCallAst  = callAst(arrayIndexCallNode, List(Ast(idNode), Ast(indexNode)))
        Seq(Ast(localNode), Ast(assignmentCallNode).withChildren(List(lhsAst, arrayIndexCallAst)))
      }
    } else {
      struct.getNames.flatMap { astName =>
        val localName                               = code(astName)
        val tpe                                     = registerType(typeFor(astName))
        val codeString                              = s"$tmpName.$localName"
        val (assignmentCallNode, localNode, lhsAst) = leftAst(astName, localName, codeString, tpe)
        val op                                      = Operators.memberAccess
        val memberAccessCallNode = callNode(astName, codeString, op, op, DispatchTypes.STATIC_DISPATCH, None, Some(tpe))
        val idNode               = identifierNode(astName, tmpName, tmpName, tpe)
        val fieldIdNode          = fieldIdentifierNode(astName, localName, localName)
        val memberAccessCallAst  = callAst(memberAccessCallNode, List(Ast(idNode), Ast(fieldIdNode)))
        Seq(Ast(localNode), Ast(assignmentCallNode).withChildren(List(lhsAst, memberAccessCallAst)))
      }
    }

    Seq(Ast(localTmpNode), assignmentCallAst) ++ accessAsts
  }

  private def isCoroutineCall(decl: IASTDeclaration): Boolean = {
    decl.getRawSignature.startsWith("co_yield ") || decl.getRawSignature.startsWith("co_await ")
  }

  /** CDT is unable to parse co_yield or co_await calls into actual AST elements. Hence, this hack to recover the
    * structure from CPPASTSimpleDeclaration.
    */
  private def astForCoroutineCall(decl: CPPASTSimpleDeclaration): Ast = {
    val op = decl.getRawSignature match {
      case s if s.startsWith("co_yield ") => "<operator>.yield"
      case _                              => "<operator>.await"
    }
    val node    = callNode(decl, code(decl), op, op, DispatchTypes.STATIC_DISPATCH)
    val argAsts = decl.getDeclarators.zipWithIndex.map { case (d, i) => astForDeclarator(decl, d, i) }
    callAst(node, argAsts.toSeq)
  }

  private def astsForIASTSimpleDeclaration(simpleDecl: IASTSimpleDeclaration): Seq[Ast] = {
    val declAsts = simpleDecl.getDeclarators.zipWithIndex.map {
      case (d: IASTFunctionDeclarator, _) => astForFunctionDeclarator(d)
      case (d, i)                         => astForDeclarator(simpleDecl, d, i)
    }
    val arrayModCallsAsts = simpleDecl.getDeclarators
      .collect { case d: IASTArrayDeclarator if hasValidArrayModifier(d) => d }
      .map { d =>
        val name          = Operators.alloc
        val tpe           = registerType(typeFor(d))
        val codeString    = code(d)
        val allocCallNode = callNode(d, codeString, name, name, DispatchTypes.STATIC_DISPATCH, None, Some(tpe))
        val allocCallAst  = callAst(allocCallNode, d.getArrayModifiers.toIndexedSeq.map(astForNode))
        val operatorName  = Operators.assignment
        val assignmentCallNode =
          callNode(d, codeString, operatorName, operatorName, DispatchTypes.STATIC_DISPATCH, None, Some(tpe))
        val left = astForNode(d.getName)
        callAst(assignmentCallNode, List(left, allocCallAst))
      }
    val initCallsAsts = simpleDecl.getDeclarators.filter(_.getInitializer != null).map { d =>
      astForInitializer(d, d.getInitializer)
    }
    val asts = Seq.from(declAsts ++ arrayModCallsAsts ++ initCallsAsts)
    setArgumentIndices(asts)
    asts
  }

  private def astsForDeclarationStatement(decl: IASTDeclarationStatement): Seq[Ast] =
    decl.getDeclaration match {
      case struct: ICPPASTStructuredBindingDeclaration                    => astsForStructuredBindingDeclaration(struct)
      case declStmt: CPPASTSimpleDeclaration if isCoroutineCall(declStmt) => Seq(astForCoroutineCall(declStmt))
      case simpleDecl: IASTSimpleDeclaration                              => astsForIASTSimpleDeclaration(simpleDecl)
      case s: ICPPASTStaticAssertDeclaration                              => Seq(astForStaticAssert(s))
      case usingDeclaration: ICPPASTUsingDeclaration                      => handleUsingDeclaration(usingDeclaration)
      case alias: ICPPASTAliasDeclaration                                 => Seq(astForAliasDeclaration(alias))
      case func: IASTFunctionDefinition                                   => Seq(astForFunctionDefinition(func))
      case alias: CPPASTNamespaceAlias                                    => Seq(astForNamespaceAlias(alias))
      case asm: IASTASMDeclaration                                        => Seq(astForASMDeclaration(asm))
      case _: ICPPASTUsingDirective                                       => Seq.empty
      case declaration                                                    => astsForDeclaration(declaration)
    }

  private def astForReturnStatement(ret: IASTReturnStatement): Ast = {
    val cpgReturn = returnNode(ret, code(ret))
    nullSafeAst(ret.getReturnValue) match {
      case retAst if retAst.root.isDefined => Ast(cpgReturn).withChild(retAst).withArgEdge(cpgReturn, retAst.root.get)
      case _                               => Ast(cpgReturn)
    }
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

  private def astForSwitchStatement(switchStmt: IASTSwitchStatement): Seq[Ast] = {
    val initAsts = switchStmt match {
      case s: ICPPASTSwitchStatement =>
        nullSafeAst(s.getInitializerStatement) ++ nullSafeAst(s.getControllerDeclaration)
      case _ =>
        Seq.empty
    }

    val codeString   = code(switchStmt)
    val switchNode   = controlStructureNode(switchStmt, ControlStructureTypes.SWITCH, codeString)
    val conditionAst = astForConditionExpression(switchStmt.getControllerExpression)
    val stmtAsts     = nullSafeAst(switchStmt.getBody)

    val finalAsts = initAsts :+ controlStructureAst(switchNode, Option(conditionAst), stmtAsts)
    setArgumentIndices(finalAsts)
    finalAsts
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
    val bodyAst = tryStmt.getTryBody match {
      case block: IASTCompoundStatement =>
        astForBlockStatement(block, blockNode(block))
      case other if other != null =>
        val bNode = blockNode(other)
        scope.pushNewBlockScope(bNode)
        val a = astsForStatement(other)
        setArgumentIndices(a)
        scope.popScope()
        blockAst(bNode, a.toList)
      case _ => Ast()
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

  private def astsForProblemStatement(statement: IASTProblemStatement): Seq[Ast] = {
    val lineNumber   = line(statement)
    val columnNumber = column(statement)
    // We only handle un-parsable macros here for now
    val isFromMacroExpansion = statement.getProblem.getNodeLocations.exists(_.isInstanceOf[IASTMacroExpansionLocation])
    val asts = if (isFromMacroExpansion) {
      new CdtParser(config, headerFileFinder, mutable.LinkedHashSet.empty)
        .parse(statement.getRawSignature, Paths.get(statement.getContainingFilename)) match
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

  private def astForFor(forStmt: IASTForStatement): Ast = {
    val codeInit = nullSafeCode(forStmt.getInitializerStatement)
    val codeCond = nullSafeCode(forStmt.getConditionExpression)
    val codeIter = nullSafeCode(forStmt.getIterationExpression)

    val code    = s"for ($codeInit$codeCond;$codeIter)"
    val forNode = controlStructureNode(forStmt, ControlStructureTypes.FOR, code)

    val (localAsts, initAsts) =
      nullSafeAst(forStmt.getInitializerStatement).partition(_.root.exists(_.isInstanceOf[NewLocal]))
    setArgumentIndices(initAsts)
    val compareAst = astForConditionExpression(forStmt.getConditionExpression)
    val updateAst  = nullSafeAst(forStmt.getIterationExpression)
    val bodyAsts   = nullSafeAst(forStmt.getBody)
    forAst(forNode, localAsts, initAsts, Seq(compareAst), Seq(updateAst), bodyAsts)
  }

  private def astForRangedFor(forStmt: ICPPASTRangeBasedForStatement): Ast = {
    val codeDecl = nullSafeCode(forStmt.getDeclaration)
    val codeInit = nullSafeCode(forStmt.getInitializerClause)
    val code     = s"for ($codeDecl:$codeInit)"
    val forNode  = controlStructureNode(forStmt, ControlStructureTypes.FOR, code)
    forStmt.getDeclaration match {
      case declaration: ICPPASTStructuredBindingDeclaration =>
        val (localAsts, initAsts) = astsForStructuredBindingDeclaration(declaration, Some(forStmt.getInitializerClause))
          .partition(_.root.exists(_.isInstanceOf[NewLocal]))
        setArgumentIndices(initAsts)
        val bodyAst = nullSafeAst(forStmt.getBody)
        forAst(forNode, localAsts, initAsts.filterNot(_.nodes.isEmpty), Seq.empty, Seq.empty, bodyAst)
      case _ =>
        val init     = astForNode(forStmt.getInitializerClause)
        val declAsts = astsForDeclaration(forStmt.getDeclaration)
        setArgumentIndices(init +: declAsts)
        val (localAsts, initAsts) = (init +: declAsts).partition(_.root.exists(_.isInstanceOf[NewLocal]))
        val bodyAst               = nullSafeAst(forStmt.getBody)
        forAst(forNode, localAsts, initAsts.filterNot(_.nodes.isEmpty), Seq.empty, Seq.empty, bodyAst)
    }
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

  private def astForIf(ifStmt: IASTIfStatement): Seq[Ast] = {
    val initAsts = ifStmt match {
      case s: ICPPASTIfStatement => nullSafeAst(s.getInitializerStatement)
      case _                     => Seq.empty
    }
    val conditionAst = ifStmt match {
      case s @ (_: CASTIfStatement | _: CPPASTIfStatement) if s.getConditionExpression != null =>
        astForConditionExpression(s.getConditionExpression)
      case s: CPPASTIfStatement if s.getConditionExpression == null =>
        val exprBlock = blockNode(s.getConditionDeclaration)
        scope.pushNewBlockScope(exprBlock)
        val a = astsForDeclaration(s.getConditionDeclaration)
        setArgumentIndices(a)
        scope.popScope()
        blockAst(exprBlock, a.toList)
    }

    val ifNode = controlStructureNode(ifStmt, ControlStructureTypes.IF, code(ifStmt))

    val thenAst = ifStmt.getThenClause match {
      case block: IASTCompoundStatement => astForBlockStatement(block, blockNode(block))
      case other if other != null =>
        val thenBlock = blockNode(other)
        scope.pushNewBlockScope(thenBlock)
        val a = astsForStatement(other)
        setArgumentIndices(a)
        scope.popScope()
        blockAst(thenBlock, a.toList)
      case _ => Ast()
    }

    val elseAst = ifStmt.getElseClause match {
      case block: IASTCompoundStatement =>
        val elseNode = controlStructureNode(ifStmt.getElseClause, ControlStructureTypes.ELSE, "else")
        val elseAst  = astForBlockStatement(block, blockNode(block))
        Ast(elseNode).withChild(elseAst)
      case other if other != null =>
        val elseNode  = controlStructureNode(ifStmt.getElseClause, ControlStructureTypes.ELSE, "else")
        val elseBlock = blockNode(other)
        scope.pushNewBlockScope(elseBlock)
        val a = astsForStatement(other)
        setArgumentIndices(a)
        scope.popScope()
        Ast(elseNode).withChild(blockAst(elseBlock, a.toList))
      case _ => Ast()
    }

    val finalAsts = initAsts :+ controlStructureAst(ifNode, Option(conditionAst), Seq(thenAst, elseAst))
    setArgumentIndices(finalAsts)
    finalAsts
  }
}
