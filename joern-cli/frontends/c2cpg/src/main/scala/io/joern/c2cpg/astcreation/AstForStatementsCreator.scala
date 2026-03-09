package io.joern.c2cpg.astcreation

import io.joern.c2cpg.parser.CdtParser
import io.joern.x2cpg.Ast
import io.joern.x2cpg.datastructures.VariableScopeManager
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.{
  AstNodeNew,
  NewBlock,
  NewCall,
  NewIdentifier,
  NewLiteral,
  NewLocal
}
import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.core.dom.ast.cpp.*
import org.eclipse.cdt.core.dom.ast.gnu.IGNUASTGotoStatement
import org.eclipse.cdt.internal.core.dom.parser.c.CASTIfStatement
import org.eclipse.cdt.internal.core.dom.parser.cpp.{CPPASTIfStatement, CPPASTNamespaceAlias, CPPASTSimpleDeclaration}
import org.eclipse.cdt.internal.core.model.ASTStringUtil

import java.nio.file.Paths

trait AstForStatementsCreator { this: AstCreator =>

  protected def astForBlockStatement(blockStmt: IASTCompoundStatement, blockNode: NewBlock): Ast = {
    val codeString  = code(blockStmt)
    val blockLine   = line(blockStmt)
    val blockColumn = column(blockStmt)
    val node = blockNode
      .code(codeString)
      .lineNumber(blockLine)
      .columnNumber(blockColumn)
      .typeFullName(registerType(Defines.Void))
    scope.pushNewBlockScope(node)
    val childAsts = blockStmt.getStatements.flatMap(astsForStatement).toList
    scope.popScope()
    blockAst(node, childAsts)
  }

  protected def astForUnsupportedCoroutineNode(node: IASTNode): Ast = {
    // CDT does not support co_await, co_yield, and co_return. That leads to completely wrong parse trees.
    Ast(unknownNode(node, code(node)))
  }

  protected def astsForStatement(statement: IASTStatement): Seq[Ast] = {
    if (isUnsupportedCoroutineKeyword(statement)) {
      return Seq(astForUnsupportedCoroutineNode(statement))
    }

    val r = statement match {
      case expr: IASTExpressionStatement          => Seq(astForExpression(expr.getExpression))
      case block: IASTCompoundStatement           => Seq(astForBlockStatement(block, blockNode(block)))
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

  protected def hasValidArrayModifier(arrayDecl: IASTArrayDeclarator): Boolean = {
    arrayDecl.getArrayModifiers.nonEmpty && arrayDecl.getArrayModifiers.forall(_.getConstantExpression != null)
  }

  private val UnsupportedCoroutineKeywords: List[String] = List("co_yield", "co_await", "co_return")

  protected def isUnsupportedCoroutineKeyword(node: IASTNode): Boolean = {
    val code = node.getRawSignature
    UnsupportedCoroutineKeywords.exists(k => code.startsWith(s"$k ") || code == k)
  }

  private def astsForStructuredBindingDeclaration(
    struct: ICPPASTStructuredBindingDeclaration,
    init: Option[IASTInitializerClause] = None
  ): Seq[Ast] = {
    def leftAst(astName: IASTNode, localName: String, codeString: String, tpe: String): (NewCall, NewLocal, Ast) = {
      val op             = Operators.assignment
      val assignmentCode = s"$localName = $codeString"
      val assignmentCallNode =
        callNode(astName, assignmentCode, op, op, DispatchTypes.STATIC_DISPATCH, None, Some(registerType(Defines.Void)))
      val localNameNode = localNode(astName, localName, localName, tpe)
      scope.addVariable(localName, localNameNode, tpe, VariableScopeManager.ScopeType.BlockScope)
      val localId = identifierNode(astName, code(astName), code(astName), tpe)
      val leftAst = Ast(localId).withRefEdge(localId, localNameNode)
      (assignmentCallNode, localNameNode, leftAst)
    }

    val initializer  = init.getOrElse(struct.getInitializer)
    val tmpName      = scopeLocalUniqueName("tmp")
    val tpe          = registerType(typeFor(initializer))
    val localTmpNode = localNode(struct, tmpName, tmpName, tpe)
    scope.addVariable(tmpName, localTmpNode, tpe, VariableScopeManager.ScopeType.BlockScope)

    val idNode = identifierNode(struct, tmpName, tmpName, tpe)
    val rhsAst = astForNode(initializer)
    val op     = Operators.assignment
    val assignmentCode =
      s"$tmpName = ${code(initializer).strip().stripPrefix("=").strip()}"
    val assignmentCallNode =
      callNode(struct, assignmentCode, op, op, DispatchTypes.STATIC_DISPATCH, None, Some(registerType(Defines.Void)))
    val assignmentCallAst = callAst(assignmentCallNode, List(Ast(idNode).withRefEdge(idNode, localTmpNode), rhsAst))

    val accessAsts = if (typeFor(initializer).endsWith("]")) {
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
        val op                                      = Operators.fieldAccess
        val memberAccessCallNode = callNode(astName, codeString, op, op, DispatchTypes.STATIC_DISPATCH, None, Some(tpe))
        val idNode               = identifierNode(astName, tmpName, tmpName, tpe)
        val fieldIdNode          = fieldIdentifierNode(astName, localName, localName)
        val memberAccessCallAst  = callAst(memberAccessCallNode, List(Ast(idNode), Ast(fieldIdNode)))
        Seq(Ast(localNode), Ast(assignmentCallNode).withChildren(List(lhsAst, memberAccessCallAst)))
      }
    }

    Seq(Ast(localTmpNode), assignmentCallAst) ++ accessAsts
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
        val idNode        = identifierNode(d, tpe, tpe, tpe)
        val allocCallNode = callNode(d, codeString, name, name, DispatchTypes.STATIC_DISPATCH, None, Some(tpe))
        val allocCallAst  = callAst(allocCallNode, Ast(idNode) +: d.getArrayModifiers.toIndexedSeq.map(astForNode))
        val operatorName  = Operators.assignment
        val assignmentCallNode =
          callNode(
            d,
            codeString,
            operatorName,
            operatorName,
            DispatchTypes.STATIC_DISPATCH,
            None,
            Some(registerType(Defines.Void))
          )
        val left = astForNode(d.getName)
        callAst(assignmentCallNode, List(left, allocCallAst))
      }
    val initCallsAsts = simpleDecl.getDeclarators.map {
      case d: ICPPASTDeclarator if d.getInitializer == null && isCPPClassLike(simpleDecl) => astForConstructorCall(d)
      case d if d.getInitializer != null => astForInitializer(d, d.getInitializer)
      case _                             => Ast()
    }
    Seq.from(declAsts ++ arrayModCallsAsts ++ initCallsAsts)
  }

  private def astsForDeclarationStatement(decl: IASTDeclarationStatement): Seq[Ast] =
    decl.getDeclaration match {
      case struct: ICPPASTStructuredBindingDeclaration => astsForStructuredBindingDeclaration(struct)
      case declStmt: CPPASTSimpleDeclaration if isUnsupportedCoroutineKeyword(declStmt) =>
        Seq(astForUnsupportedCoroutineNode(declStmt))
      case simpleDecl: IASTSimpleDeclaration                     => astsForIASTSimpleDeclaration(simpleDecl)
      case s: ICPPASTStaticAssertDeclaration                     => Seq(astForStaticAssert(s))
      case alias: ICPPASTAliasDeclaration                        => Seq(astForAliasDeclaration(alias))
      case func: IASTFunctionDefinition                          => Seq(astForFunctionDefinition(func))
      case alias: CPPASTNamespaceAlias                           => Seq(astForNamespaceAlias(alias))
      case asm: IASTASMDeclaration                               => Seq(astForASMDeclaration(asm))
      case _: ICPPASTUsingDeclaration | _: ICPPASTUsingDirective => Seq.empty // handled by CDT itself
      case declaration                                           => astsForDeclaration(declaration)
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
    val codeString  = code(label)
    val name        = shortName(label)
    val labelNode   = jumpTargetNode(label, name, codeString)
    val nestedStmts = nullSafeAst(label.getNestedStatement)
    Ast(labelNode) +: nestedStmts
  }

  private def astForDoStatement(doStmt: IASTDoStatement): Ast = {
    val codeString = code(doStmt)
    val doNode     = controlStructureNode(doStmt, ControlStructureTypes.DO, codeString)
    scope.pushNewBlockScope(doNode)
    val conditionAst = wrapInNullComparison(doStmt.getCondition, astForConditionExpression(doStmt.getCondition))
    val bodyAst      = nullSafeAst(doStmt.getBody)
    scope.popScope()
    controlStructureAst(doNode, Option(conditionAst), bodyAst, placeConditionLast = true)
  }

  private def astForSwitchStatement(switchStmt: IASTSwitchStatement): Seq[Ast] = {
    val codeString = code(switchStmt)
    val switchNode = controlStructureNode(switchStmt, ControlStructureTypes.SWITCH, codeString)

    val initAsts = switchStmt match {
      case s: ICPPASTSwitchStatement =>
        nullSafeAst(s.getInitializerStatement) ++ nullSafeAst(s.getControllerDeclaration)
      case _ =>
        Seq.empty
    }
    val conditionAst = astForConditionExpression(switchStmt.getControllerExpression)
    val stmtAsts     = nullSafeAst(switchStmt.getBody)
    initAsts :+ controlStructureAst(switchNode, Option(conditionAst), stmtAsts)
  }

  private def astsForCaseStatement(caseStmt: IASTCaseStatement): Seq[Ast] = {
    val jumpTargetNode_ = jumpTargetNode(caseStmt, "case", code(caseStmt))
    val stmt            = astForConditionExpression(caseStmt.getExpression)
    Seq(Ast(jumpTargetNode_), stmt)
  }

  private def astForDefaultStatement(caseStmt: IASTDefaultStatement): Ast = {
    val jumpTargetNode_ = jumpTargetNode(caseStmt, "default", code(caseStmt))
    Ast(jumpTargetNode_)
  }

  private def astForTryStatement(tryStmt: ICPPASTTryBlockStatement): Ast = {
    val tryNode = controlStructureNode(tryStmt, ControlStructureTypes.TRY, "try")
    val bodyAst = tryStmt.getTryBody match {
      case block: IASTCompoundStatement =>
        astForBlockStatement(block, blockNode(block))
      case other if other != null =>
        val bNode = blockNode(other)
        scope.pushNewBlockScope(bNode)
        val statementAsts = astsForStatement(other)
        scope.popScope()
        blockAst(bNode, statementAsts.toList)
      case _ => Ast()
    }
    val catchAsts = tryStmt.getCatchHandlers.toSeq.map(astForCatchHandler)
    tryCatchAst(tryNode, bodyAst, catchAsts, None)
  }

  private def astForCatchHandler(catchHandler: ICPPASTCatchHandler): Ast = {
    val catchNode = controlStructureNode(catchHandler, ControlStructureTypes.CATCH, "catch")
    scope.pushNewBlockScope(catchNode)
    val declAst = nullSafeAst(catchHandler.getDeclaration)
    val bodyAst = nullSafeAst(catchHandler.getCatchBody)
    scope.popScope()
    Ast(catchNode).withChildren(declAst).withChildren(bodyAst)
  }

  private def astsForProblemStatement(statement: IASTProblemStatement): Seq[Ast] = {
    val lineNumber   = line(statement)
    val columnNumber = column(statement)
    // We only handle un-parsable macros here for now
    val isFromMacroExpansion = statement.getProblem.getNodeLocations.exists(_.isInstanceOf[IASTMacroExpansionLocation])
    val code                 = statement.getRawSignature
    val file                 = Paths.get(statement.getContainingFilename)
    val asts = if (isFromMacroExpansion) {
      new CdtParser(config, headerFileFinder, None, global).parse(code, file) match {
        case Some(translationUnit: IASTTranslationUnit) =>
          translationUnit.getDeclarations.toIndexedSeq.flatMap(astsForDeclaration)
        case None =>
          Seq.empty
      }
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

    scope.pushNewBlockScope(forNode)
    val (localAsts, initAsts) =
      nullSafeAst(forStmt.getInitializerStatement).partition(_.root.exists(_.isInstanceOf[NewLocal]))
    setArgumentIndices(initAsts)
    val compareAst =
      wrapInNullComparison(forStmt.getConditionExpression, astForConditionExpression(forStmt.getConditionExpression))
    val updateAst = nullSafeAst(forStmt.getIterationExpression)
    val bodyAsts  = nullSafeAst(forStmt.getBody)
    scope.popScope()
    forAst(forNode, localAsts, initAsts, Seq(compareAst), Seq(updateAst), bodyAsts)
  }

  private def astForRangedFor(forStmt: ICPPASTRangeBasedForStatement): Ast = {
    forStmt.getDeclaration match {
      case declaration: ICPPASTStructuredBindingDeclaration => astForForEachStatementWithBinding(forStmt, declaration)
      case _                                                => astForForEachStatementWithIdentifier(forStmt)
    }
  }

  private def astForForEachStatementWithBinding(
    forStmt: ICPPASTRangeBasedForStatement,
    declaration: ICPPASTStructuredBindingDeclaration
  ): Ast = {
    val codeDecl = nullSafeCode(forStmt.getDeclaration)
    val codeInit = nullSafeCode(forStmt.getInitializerClause)
    val code     = s"for ($codeDecl:$codeInit)"
    val forNode  = controlStructureNode(forStmt, ControlStructureTypes.FOR, code)
    scope.pushNewBlockScope(forNode)
    val (localAsts, initAsts) = astsForStructuredBindingDeclaration(declaration, Some(forStmt.getInitializerClause))
      .partition(_.root.exists(_.isInstanceOf[NewLocal]))
    setArgumentIndices(initAsts)
    val bodyAst = nullSafeAst(forStmt.getBody)
    scope.popScope()
    forAst(forNode, localAsts, initAsts.filterNot(_.nodes.isEmpty), Seq.empty, Seq.empty, bodyAst)
  }

  /** De-sugaring from:
    *
    * for (type i : arr) { body }
    *
    * to:
    *
    * { var _iterator = arr.iterator(); var i; while (_iterator.hasNext()) { i = _iterator.next(); body } }
    */
  private def astForForEachStatementWithIdentifier(forStmt: ICPPASTRangeBasedForStatement): Ast = {
    // surrounding block:
    val blockNode = this.blockNode(forStmt)
    scope.pushNewBlockScope(blockNode)

    val collection     = forStmt.getInitializerClause
    val collectionName = code(collection)
    val idType         = registerType(typeFor(forStmt.getDeclaration))

    // iterator assignment:
    val iteratorName      = scopeLocalUniqueName("iterator")
    val iteratorLocalNode = localNode(forStmt, iteratorName, iteratorName, registerType(Defines.Iterator)).order(0)
    val iteratorNode      = identifierNode(forStmt, iteratorName, iteratorName, Defines.Iterator)
    diffGraph.addEdge(blockNode, iteratorLocalNode, EdgeTypes.AST)
    scope.addVariableReference(iteratorName, iteratorNode, Defines.Iterator, EvaluationStrategies.BY_REFERENCE)

    val iteratorCall =
      callNode(
        forStmt,
        s"$collectionName.iterator()",
        "iterator",
        s"${Defines.UnresolvedNamespace}.iterator:${Defines.Iterator}()",
        DispatchTypes.DYNAMIC_DISPATCH,
        Some(s"${Defines.Iterator}()"),
        Some(registerType(Defines.Iterator))
      )

    val iteratorCallBase = astForNode(collection)
    val iteratorCallAst  = createCallAst(iteratorCall, base = Some(iteratorCallBase), receiver = Some(iteratorCallBase))

    val iteratorAssignmentNode =
      callNode(
        forStmt,
        s"$iteratorName = ${iteratorCall.code}",
        Operators.assignment,
        Operators.assignment,
        DispatchTypes.STATIC_DISPATCH,
        None,
        Some(registerType(Defines.Void))
      )

    val iteratorAssignmentArgs = List(Ast(iteratorNode), iteratorCallAst)
    val iteratorAssignmentAst  = callAst(iteratorAssignmentNode, iteratorAssignmentArgs)

    // loop variable:
    val loopVariableName = forStmt.getDeclaration match {
      case decl: IASTSimpleDeclaration =>
        decl.getDeclarators.headOption.map(shortName).getOrElse(code(forStmt.getDeclaration))
      case _ =>
        code(forStmt.getDeclaration)
    }

    val loopVariableLocalNode = localNode(forStmt, loopVariableName, loopVariableName, idType).order(0)
    scope.addVariable(loopVariableName, loopVariableLocalNode, idType, VariableScopeManager.ScopeType.BlockScope)

    // while loop:
    val codeDecl      = code(forStmt.getDeclaration)
    val codeInit      = code(forStmt.getInitializerClause)
    val whileLoopCode = s"for ($codeDecl:$codeInit)"
    val whileLoopNode = controlStructureNode(forStmt, ControlStructureTypes.WHILE, whileLoopCode)

    // while loop test:
    val testCallNode =
      callNode(
        forStmt,
        s"$iteratorName.hasNext()",
        "hasNext",
        s"${Defines.Iterator}.hasNext:bool()",
        DispatchTypes.DYNAMIC_DISPATCH,
        Some("bool()"),
        Some(registerType("bool"))
      )

    val hasNextReceiverNode = identifierNode(forStmt, iteratorName, iteratorName, Defines.Iterator)
    scope.addVariableReference(iteratorName, hasNextReceiverNode, Defines.Iterator, EvaluationStrategies.BY_REFERENCE)

    val testAst =
      createCallAst(testCallNode, base = Some(Ast(hasNextReceiverNode)), receiver = Some(Ast(hasNextReceiverNode)))

    val whileLoopAst = Ast(whileLoopNode).withChild(testAst).withConditionEdge(whileLoopNode, testCallNode)

    // while loop variable assignment:
    val whileLoopVariableNode = identifierNode(forStmt, loopVariableName, loopVariableName, idType)
    scope.addVariableReference(loopVariableName, whileLoopVariableNode, idType, EvaluationStrategies.BY_REFERENCE)

    val nextCallNode =
      callNode(
        forStmt,
        s"$iteratorName.next()",
        "next",
        s"${Defines.Iterator}.next:${Defines.Any}()",
        DispatchTypes.DYNAMIC_DISPATCH,
        Some(s"${Defines.Any}()"),
        Some(registerType(Defines.Any))
      )

    val nextReceiverNode = identifierNode(forStmt, iteratorName, iteratorName, Defines.Iterator)
    scope.addVariableReference(iteratorName, nextReceiverNode, Defines.Iterator, EvaluationStrategies.BY_REFERENCE)

    val nextCallAst =
      createCallAst(nextCallNode, base = Some(Ast(nextReceiverNode)), receiver = Some(Ast(nextReceiverNode)))

    val loopVariableAssignmentNode = callNode(
      forStmt,
      s"$loopVariableName = ${nextCallNode.code}",
      Operators.assignment,
      Operators.assignment,
      DispatchTypes.STATIC_DISPATCH,
      None,
      Some(registerType(Defines.Void))
    )

    val loopVariableAssignmentArgs = List(Ast(whileLoopVariableNode), nextCallAst)
    val loopVariableAssignmentAst  = callAst(loopVariableAssignmentNode, loopVariableAssignmentArgs)

    // while loop block:
    val whileLoopBlockNode = this.blockNode(forStmt)
    scope.pushNewBlockScope(whileLoopBlockNode)

    val bodyAst                = nullSafeAst(forStmt.getBody)
    val whileLoopBlockChildren = loopVariableAssignmentAst +: bodyAst
    val whileLoopBlockAst      = blockAst(whileLoopBlockNode, whileLoopBlockChildren.toList)

    // end while loop block:
    scope.popScope()

    // end surrounding block:
    scope.popScope()

    val blockChildren =
      List(iteratorAssignmentAst, Ast(loopVariableLocalNode), whileLoopAst.withChild(whileLoopBlockAst))
    blockAst(blockNode, blockChildren)
  }

  private def astForWhile(whileStmt: IASTWhileStatement): Ast = {
    val code       = s"while (${nullSafeCode(whileStmt.getCondition)})"
    val compareAst = wrapInNullComparison(whileStmt.getCondition, astForConditionExpression(whileStmt.getCondition))
    val bodyAst    = nullSafeAst(whileStmt.getBody)
    whileAst(
      Option(compareAst),
      bodyAst,
      code = Option(code),
      lineNumber = line(whileStmt),
      columnNumber = column(whileStmt)
    )
  }

  private def wrapInNullComparison(node: IASTNode, conditionAst: Ast): Ast = {
    def isWrapCandidate(ast: Ast): Boolean = {
      ast.root match {
        case Some(r: NewCall)    => false
        case Some(r: NewBlock)   => false
        case Some(r: NewLiteral) => false
        case _                   => true
      }
    }

    if (node == null || conditionAst.root.isEmpty) {
      return conditionAst
    }
    conditionAst match {
      case ast if !isWrapCandidate(ast) => ast
      case ast =>
        val nullNode = conditionAst.root match {
          case Some(id: NewIdentifier) if id.typeFullName.endsWith("*") => literalNode(node, "NULL", Defines.Any)
          case _                                                        => literalNode(node, "0", "int")
        }
        val notEqualsCallNode = callNode(
          node,
          s"${code(node)} != ${nullNode.code}",
          Operators.notEquals,
          Operators.notEquals,
          DispatchTypes.STATIC_DISPATCH,
          None,
          Some(registerType("int"))
        )
        createCallAst(notEqualsCallNode, Seq(ast, Ast(nullNode)))
    }
  }

  private def astForIf(ifStmt: IASTIfStatement): Seq[Ast] = {
    val ifNode = controlStructureNode(ifStmt, ControlStructureTypes.IF, code(ifStmt))

    val initAsts = ifStmt match {
      case s: ICPPASTIfStatement => nullSafeAst(s.getInitializerStatement)
      case _                     => Seq.empty
    }
    val (conditionAstRaw, node) = ifStmt match {
      case s @ (_: CASTIfStatement | _: CPPASTIfStatement) if s.getConditionExpression != null =>
        (astForConditionExpression(s.getConditionExpression), s.getConditionExpression)
      case s: CPPASTIfStatement if s.getConditionExpression == null =>
        val exprBlock = blockNode(s.getConditionDeclaration)
        scope.pushNewBlockScope(exprBlock)
        val declAsts = astsForDeclaration(s.getConditionDeclaration)
        scope.popScope()
        (blockAst(exprBlock, declAsts.toList), s.getConditionDeclaration)
    }

    val conditionAst = wrapInNullComparison(node, conditionAstRaw)

    val thenAst = ifStmt.getThenClause match {
      case block: IASTCompoundStatement => astForBlockStatement(block, blockNode(block))
      case other if other != null =>
        val thenBlock = blockNode(other)
        scope.pushNewBlockScope(thenBlock)
        val statementAsts = astsForStatement(other)
        scope.popScope()
        blockAst(thenBlock, statementAsts.toList)
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
        val statementAsts = astsForStatement(other)
        scope.popScope()
        Ast(elseNode).withChild(blockAst(elseBlock, statementAsts.toList))
      case _ => Ast()
    }
    initAsts :+ controlStructureAst(ifNode, Option(conditionAst), Seq(thenAst, elseAst))
  }
}
