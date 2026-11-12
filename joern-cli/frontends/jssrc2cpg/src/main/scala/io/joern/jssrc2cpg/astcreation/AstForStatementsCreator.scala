package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.parser.BabelNodeSyntax.*
import io.joern.x2cpg.Ast
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.frontendspecific.jssrc2cpg.Defines
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies

trait AstForStatementsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  /** Sort all block statements with the following result:
    *   - all function declarations go first
    *   - all type aliases that are not plain type references go last
    *   - all remaining type aliases go before that
    *   - all remaining statements go second
    *
    * We do this to get TypeDecls created at the right spot so we can make use of them for the type aliases.
    */
  private def sortBlockStatements(blockStatements: List[BabelNode]): List[BabelNode] =
    blockStatements.sortBy {
      case _: ImportDeclaration                                           => 0
      case _: FunctionDeclaration                                         => 1
      case alias: (DeclareTypeAlias | TypeAlias | TSTypeAliasDeclaration) =>
        if (isPlainTypeAlias(alias)) 4 else 3
      case _ => 2
    }

  protected def createBlockStatementAsts(statements: Seq[BabelNode]): List[Ast] =
    sortBlockStatements(statements.toList).map(astForNodeWithFunctionReferenceAndCall)

  protected def astForWithStatement(withStatement: WithStatement): Ast = {
    val blockNode_ = blockNode(withStatement, withStatement.code, Defines.Any)
    scope.pushNewBlockScope(blockNode_)
    localAstParentStack.push(blockNode_)
    val objectAst = astForNodeWithFunctionReferenceAndCall(withStatement.`object`)
    val bodyAsts  = withStatement.body match {
      case block: BlockStatement => createBlockStatementAsts(block.body)
      case other                 => List(astForNodeWithFunctionReferenceAndCall(other))
    }
    val blockStatementAsts = objectAst +: bodyAsts
    localAstParentStack.pop()
    scope.popScope()
    blockAst(blockNode_, blockStatementAsts)
  }

  protected def astForBlockStatement(block: BlockStatement | TSModuleBlock): Ast = {
    val blockNode_ = blockNode(block, block.code, Defines.Any)
    scope.pushNewBlockScope(blockNode_)
    localAstParentStack.push(blockNode_)
    val statements = block match {
      case b: BlockStatement => b.body
      case m: TSModuleBlock  => m.body
    }
    val blockStatementAsts = createBlockStatementAsts(statements)
    localAstParentStack.pop()
    scope.popScope()
    blockAst(blockNode_, blockStatementAsts)
  }

  protected def astForReturnStatement(ret: YieldExpression | ReturnStatement): Ast = {
    val retCode  = ret.code.stripSuffix(";")
    val retNode  = returnNode(ret, retCode)
    val argument = ret match {
      case r: ReturnStatement => r.argument
      case y: YieldExpression => y.argument
    }
    argument
      .map { argument =>
        val argAst = astForNodeWithFunctionReference(argument)
        returnAst(retNode, List(argAst))
      }
      .getOrElse(Ast(retNode))
  }

  private def astForCatchClause(catchClause: CatchClause): Ast = {
    val blockNode_ = blockNode(catchClause, catchClause.code, Defines.Any)
    scope.pushNewBlockScope(blockNode_)
    localAstParentStack.push(blockNode_)
    val paramAst           = catchClause.param.map(astForNodeWithFunctionReference).getOrElse(Ast())
    val bodyAsts           = createBlockStatementAsts(catchClause.body.body)
    val blockStatementAsts = paramAst +: bodyAsts
    localAstParentStack.pop()
    scope.popScope()
    blockAst(blockNode_, blockStatementAsts)
  }

  protected def astForTryStatement(tryStmt: TryStatement): Ast = {
    val bodyAst  = astForNodeWithFunctionReference(tryStmt.block)
    val catchAst = tryStmt.handler.toList.map { handler =>
      val catchNode = controlStructureNode(handler, ControlStructureTypes.CATCH, handler.code)
      val catchAst  = astForCatchClause(handler)
      Ast(catchNode).withChild(catchAst)
    }
    val finalizerAst = tryStmt.finalizer.map { finalizer =>
      val finalNode = controlStructureNode(finalizer, ControlStructureTypes.FINALLY, finalizer.code)
      val finalAst  = astForNodeWithFunctionReference(finalizer)
      Ast(finalNode).withChild(finalAst)
    }
    tryCatchAst(tryStmt, bodyAst, catchAst, finalizerAst)
  }

  private def handleEmptyStatement(node: BabelNode): Ast = node match {
    case _: EmptyStatement => Ast(blockNode(node))
    case _                 => astForNodeWithFunctionReference(node)
  }

  def astForIfStatement(ifStmt: IfStatement): Ast = {
    val conditionAst = handleEmptyStatement(ifStmt.test)
    val thenAst      = handleEmptyStatement(ifStmt.consequent)
    val elseAst      = ifStmt.alternate.map(handleEmptyStatement)
    ifThenElseAst(ifStmt, Some(conditionAst), thenAst, elseAst)
  }

  protected def astForDoWhileStatement(doWhileStmt: DoWhileStatement): Ast = {
    val testAst = handleEmptyStatement(doWhileStmt.test)
    val bodyAst = handleEmptyStatement(doWhileStmt.body)
    doWhileAst(doWhileStmt, Some(testAst), Seq(bodyAst))
  }

  protected def astForWhileStatement(whileStmt: WhileStatement): Ast = {
    val testAst = handleEmptyStatement(whileStmt.test)
    val bodyAst = handleEmptyStatement(whileStmt.body)
    whileAst(whileStmt, Some(testAst), Seq(bodyAst))
  }

  protected def astForForStatement(forStmt: ForStatement): Ast = {
    val initAsts = forStmt.init.map(init => Seq(astForNodeWithFunctionReference(init))).getOrElse(Seq.empty)
    val testAsts = forStmt.test
      .map(test => Seq(astForNodeWithFunctionReference(test)))
      .getOrElse(Seq(Ast(literalNode(forStmt, "true", Option(Defines.Boolean)))))
    val updateAsts = forStmt.update.map(update => Seq(astForNodeWithFunctionReference(update))).getOrElse(Seq.empty)
    val bodyAst    = astForNodeWithFunctionReference(forStmt.body)

    forAst(forStmt, Seq.empty, initAsts, testAsts, updateAsts, Seq(bodyAst))
  }

  protected def astForLabeledStatement(labelStmt: LabeledStatement): Ast = {
    val labelName   = labelStmt.label.code
    val labeledNode = jumpTargetNode(labelStmt, labelName, s"$labelName:", Option(labelStmt.nodeType))

    val blockNode_ = blockNode(labelStmt, labelStmt.code, Defines.Any)
    scope.pushNewBlockScope(blockNode_)
    localAstParentStack.push(blockNode_)
    val bodyAst = astForNodeWithFunctionReference(labelStmt.body)
    scope.popScope()
    localAstParentStack.pop()

    val labelAsts = List(Ast(labeledNode), bodyAst)
    blockAst(blockNode_, labelAsts)
  }

  protected def astForBreakStatement(breakStmt: BreakStatement): Ast =
    breakAst(breakStmt, breakStmt.code, breakStmt.label.map(_.code))

  protected def astForContinueStatement(continueStmt: ContinueStatement): Ast =
    continueAst(continueStmt, continueStmt.code, continueStmt.label.map(_.code))

  protected def astForThrowStatement(throwStmt: ThrowStatement): Ast =
    throwAst(throwStmt, List(astForNodeWithFunctionReference(throwStmt.argument)))

  private def astsForSwitchCase(switchCase: SwitchCase): List[Ast] = {
    val labelAst       = Ast(jumpTargetNode(switchCase))
    val testAsts       = switchCase.test.map(astForNodeWithFunctionReference).toList
    val consequentAsts = astForNodes(switchCase.consequent)
    labelAst +: (testAsts ++ consequentAsts)
  }

  protected def astForSwitchStatement(switchStmt: SwitchStatement): Ast = {
    // The semantics of switch statement children is partially defined by their order value.
    // The blockAst must have order == 2. Only to avoid collision we set switchExpressionAst to 1
    // because the semantics of it is already indicated via the condition edge.
    val switchExpressionAst = astForNodeWithFunctionReference(switchStmt.discriminant)
    setOrderExplicitly(switchExpressionAst, 1)

    val blockNode_ = blockNode(switchStmt, switchStmt.code, Defines.Any).order(2)
    scope.pushNewBlockScope(blockNode_)
    localAstParentStack.push(blockNode_)
    val casesAsts = switchStmt.cases.flatMap(astsForSwitchCase)
    scope.popScope()
    localAstParentStack.pop()

    val switchBlockAst = blockAst(blockNode_, casesAsts.toList)
    switchAst(switchStmt, Some(switchExpressionAst), Seq(switchBlockAst))
  }

  /** De-sugaring from:
    *
    * for (var i in/of arr) { body }
    *
    * to:
    *
    * { var _iterator = <operator>.iterator(arr); var _result; var i; while (!(_result = _iterator.next()).done) { i =
    * _result.value; body } }
    */
  private def astForInOfStatementWithIdentifier(
    forInOfStmt: ForInStatement | ForOfStatement,
    idNode: Identifier
  ): Ast = {
    // surrounding block:
    val blockNode_ = blockNode(forInOfStmt, forInOfStmt.code, Defines.Any)
    scope.pushNewBlockScope(blockNode_)
    localAstParentStack.push(blockNode_)

    val collection     = rightOf(forInOfStmt)
    val collectionName = collection.code

    // _iterator assignment:
    val iteratorName      = generateUnusedVariableName(usedVariableNames, "_iterator")
    val iteratorLocalNode = localNode(forInOfStmt, iteratorName, iteratorName, Defines.Any).order(0)
    val iteratorNode      = identifierNode(forInOfStmt, iteratorName)
    diffGraph.addEdge(localAstParentStack.head, iteratorLocalNode, EdgeTypes.AST)
    scope.addVariableReference(iteratorName, iteratorNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)

    val iteratorCall =
      // TODO: add operator to schema
      callNode(
        forInOfStmt,
        s"<operator>.iterator($collectionName)",
        "<operator>.iterator",
        DispatchTypes.STATIC_DISPATCH
      )

    val objectKeysCallArgs = List(astForNodeWithFunctionReference(collection))
    val objectKeysCallAst  = callAst(iteratorCall, objectKeysCallArgs)

    val iteratorAssignmentNode =
      callNode(
        forInOfStmt,
        s"$iteratorName = <operator>.iterator($collectionName)",
        Operators.assignment,
        DispatchTypes.STATIC_DISPATCH
      )

    val iteratorAssignmentArgs = List(Ast(iteratorNode), objectKeysCallAst)
    val iteratorAssignmentAst  = callAst(iteratorAssignmentNode, iteratorAssignmentArgs)

    // _result:
    val resultName      = generateUnusedVariableName(usedVariableNames, "_result")
    val resultLocalNode = localNode(forInOfStmt, resultName, resultName, Defines.Any).order(0)
    val resultNode      = identifierNode(forInOfStmt, resultName)
    diffGraph.addEdge(localAstParentStack.head, resultLocalNode, EdgeTypes.AST)
    scope.addVariableReference(resultName, resultNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)

    // loop variable:
    val loopVariableName = idNode.code

    val loopVariableLocalNode = localNode(forInOfStmt, loopVariableName, loopVariableName, Defines.Any).order(0)
    val loopVariableNode      = identifierNode(forInOfStmt, loopVariableName)
    diffGraph.addEdge(localAstParentStack.head, loopVariableLocalNode, EdgeTypes.AST)
    scope.addVariableReference(loopVariableName, loopVariableNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)

    // while loop:
    // while loop test:
    val testCallNode =
      callNode(forInOfStmt, s"!($resultName = $iteratorName.next()).done", Operators.not, DispatchTypes.STATIC_DISPATCH)

    val doneBaseNode =
      callNode(
        forInOfStmt,
        s"($resultName = $iteratorName.next())",
        Operators.assignment,
        DispatchTypes.STATIC_DISPATCH
      )

    val lhsNode = identifierNode(forInOfStmt, resultName)

    val rhsNode = callNode(forInOfStmt, s"$iteratorName.next()", "next", DispatchTypes.DYNAMIC_DISPATCH)

    val nextBaseNode = identifierNode(forInOfStmt, iteratorName)

    val nextMemberNode = fieldIdentifierNode(forInOfStmt, "next", "next")

    val nextReceiverNode =
      createFieldAccessCallAst(nextBaseNode, nextMemberNode, forInOfStmt.lineNumber, forInOfStmt.columnNumber)

    val thisNextNode = identifierNode(forInOfStmt, iteratorName)

    val rhsArgs = List(Ast(thisNextNode))
    val rhsAst  = callAst(rhsNode, rhsArgs, receiver = Option(nextReceiverNode))

    val doneBaseArgs = List(Ast(lhsNode), rhsAst)
    val doneBaseAst  = callAst(doneBaseNode, doneBaseArgs)
    Ast.storeInDiffGraph(doneBaseAst, diffGraph)

    val doneMemberNode = fieldIdentifierNode(forInOfStmt, "done", "done")

    val testNode =
      createFieldAccessCallAst(doneBaseNode, doneMemberNode, forInOfStmt.lineNumber, forInOfStmt.columnNumber)

    val testCallArgs = List(testNode)
    val testCallAst  = callAst(testCallNode, testCallArgs)

    // while loop variable assignment:
    val whileLoopVariableNode = identifierNode(forInOfStmt, loopVariableName)

    val baseNode = identifierNode(forInOfStmt, resultName)

    val memberNode = fieldIdentifierNode(forInOfStmt, "value", "value")

    val accessAst = createFieldAccessCallAst(baseNode, memberNode, forInOfStmt.lineNumber, forInOfStmt.columnNumber)

    val loopVariableAssignmentNode = callNode(
      forInOfStmt,
      s"$loopVariableName = $resultName.value",
      Operators.assignment,
      DispatchTypes.STATIC_DISPATCH
    )

    val loopVariableAssignmentArgs = List(Ast(whileLoopVariableNode), accessAst)
    val loopVariableAssignmentAst  = callAst(loopVariableAssignmentNode, loopVariableAssignmentArgs)

    val whileLoopBlockNode = blockNode(forInOfStmt, forInOfStmt.code, Defines.Any)
    scope.pushNewBlockScope(whileLoopBlockNode)
    localAstParentStack.push(whileLoopBlockNode)

    // while loop block:
    val bodyAst = astForNodeWithFunctionReference(bodyOf(forInOfStmt))

    val whileLoopBlockChildren = List(loopVariableAssignmentAst, bodyAst)
    val whileLoopBlockAst      = blockAst(whileLoopBlockNode, whileLoopBlockChildren)

    scope.popScope()
    localAstParentStack.pop()

    // end surrounding block:
    scope.popScope()
    localAstParentStack.pop()

    val whileLoopAst  = whileAst(forInOfStmt, Some(testCallAst), List(whileLoopBlockAst))
    val blockChildren = List(iteratorAssignmentAst, Ast(resultNode), Ast(loopVariableNode), whileLoopAst)
    blockAst(blockNode_, blockChildren)
  }

  /** De-sugaring from:
    *
    * for (expr in/of arr) { body }
    *
    * to:
    *
    * { var _iterator = <operator>.iterator(arr); var _result; while (!(_result = _iterator.next()).done) { expr =
    * _result.value; body } }
    */
  private def astForInOfStatementWithExpression(
    forInOfStmt: ForInStatement | ForOfStatement,
    idNode: BabelNode
  ): Ast = {
    // surrounding block:
    val blockNode_ = blockNode(forInOfStmt, forInOfStmt.code, Defines.Any)
    scope.pushNewBlockScope(blockNode_)
    localAstParentStack.push(blockNode_)

    val collection     = rightOf(forInOfStmt)
    val collectionName = collection.code

    // _iterator assignment:
    val iteratorName      = generateUnusedVariableName(usedVariableNames, "_iterator")
    val iteratorLocalNode = localNode(forInOfStmt, iteratorName, iteratorName, Defines.Any).order(0)
    val iteratorNode      = identifierNode(forInOfStmt, iteratorName)
    diffGraph.addEdge(localAstParentStack.head, iteratorLocalNode, EdgeTypes.AST)
    scope.addVariableReference(iteratorName, iteratorNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)

    val iteratorCall =
      // TODO: add operator to schema
      callNode(
        forInOfStmt,
        s"<operator>.iterator($collectionName)",
        "<operator>.iterator",
        DispatchTypes.STATIC_DISPATCH
      )

    val objectKeysCallArgs = List(astForNodeWithFunctionReference(collection))
    val objectKeysCallAst  = callAst(iteratorCall, objectKeysCallArgs)

    val iteratorAssignmentNode =
      callNode(
        forInOfStmt,
        s"$iteratorName = <operator>.iterator($collectionName)",
        Operators.assignment,
        DispatchTypes.STATIC_DISPATCH
      )

    val iteratorAssignmentArgs = List(Ast(iteratorNode), objectKeysCallAst)
    val iteratorAssignmentAst  = callAst(iteratorAssignmentNode, iteratorAssignmentArgs)

    // _result:
    val resultName      = generateUnusedVariableName(usedVariableNames, "_result")
    val resultLocalNode = localNode(forInOfStmt, resultName, resultName, Defines.Any).order(0)
    val resultNode      = identifierNode(forInOfStmt, resultName)
    diffGraph.addEdge(localAstParentStack.head, resultLocalNode, EdgeTypes.AST)
    scope.addVariableReference(resultName, resultNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)

    // while loop:
    // while loop test:
    val testCallNode =
      callNode(forInOfStmt, s"!($resultName = $iteratorName.next()).done", Operators.not, DispatchTypes.STATIC_DISPATCH)

    val doneBaseNode =
      callNode(
        forInOfStmt,
        s"($resultName = $iteratorName.next())",
        Operators.assignment,
        DispatchTypes.STATIC_DISPATCH
      )

    val lhsNode = identifierNode(forInOfStmt, resultName)

    val rhsNode = callNode(forInOfStmt, s"$iteratorName.next()", "next", DispatchTypes.DYNAMIC_DISPATCH)

    val nextBaseNode = identifierNode(forInOfStmt, iteratorName)

    val nextMemberNode = fieldIdentifierNode(forInOfStmt, "next", "next")

    val nextReceiverNode =
      createFieldAccessCallAst(nextBaseNode, nextMemberNode, forInOfStmt.lineNumber, forInOfStmt.columnNumber)

    val thisNextNode = identifierNode(forInOfStmt, iteratorName)

    val rhsArgs = List(Ast(thisNextNode))
    val rhsAst  = callAst(rhsNode, rhsArgs, receiver = Option(nextReceiverNode))

    val doneBaseArgs = List(Ast(lhsNode), rhsAst)
    val doneBaseAst  = callAst(doneBaseNode, doneBaseArgs)
    Ast.storeInDiffGraph(doneBaseAst, diffGraph)

    val doneMemberNode = fieldIdentifierNode(forInOfStmt, "done", "done")

    val testNode =
      createFieldAccessCallAst(doneBaseNode, doneMemberNode, forInOfStmt.lineNumber, forInOfStmt.columnNumber)

    val testCallArgs = List(testNode)
    val testCallAst  = callAst(testCallNode, testCallArgs)

    // while loop variable assignment:
    val whileLoopVariableNode = astForNode(idNode)

    val baseNode = identifierNode(forInOfStmt, resultName)

    val memberNode = fieldIdentifierNode(forInOfStmt, "value", "value")

    val accessAst = createFieldAccessCallAst(baseNode, memberNode, forInOfStmt.lineNumber, forInOfStmt.columnNumber)

    val loopVariableAssignmentNode =
      callNode(forInOfStmt, s"${idNode.code} = $resultName.value", Operators.assignment, DispatchTypes.STATIC_DISPATCH)

    val loopVariableAssignmentArgs = List(whileLoopVariableNode, accessAst)
    val loopVariableAssignmentAst  = callAst(loopVariableAssignmentNode, loopVariableAssignmentArgs)

    val whileLoopBlockNode = blockNode(forInOfStmt, forInOfStmt.code, Defines.Any)
    scope.pushNewBlockScope(whileLoopBlockNode)
    localAstParentStack.push(whileLoopBlockNode)

    // while loop block:
    val bodyAst = astForNodeWithFunctionReference(bodyOf(forInOfStmt))

    val whileLoopBlockChildren = List(loopVariableAssignmentAst, bodyAst)
    val whileLoopBlockAst      = blockAst(whileLoopBlockNode, whileLoopBlockChildren)

    scope.popScope()
    localAstParentStack.pop()

    // end surrounding block:
    scope.popScope()
    localAstParentStack.pop()

    val whileLoopAst  = whileAst(forInOfStmt, Some(testCallAst), List(whileLoopBlockAst))
    val blockChildren = List(iteratorAssignmentAst, Ast(resultNode), whileLoopAst)
    blockAst(blockNode_, blockChildren)
  }

  /** De-sugaring from:
    *
    * for(var {a, b, c} of obj) { body }
    *
    * to:
    *
    * { var _iterator = <operator>.iterator(obj); var _result; var a; var b; var c; while (!(_result =
    * _iterator.next()).done) { a = _result.value.a; b = _result.value.b; c = _result.value.c; body } }
    */
  private def astForInOfStatementWithObject(
    forInOfStmt: ForInStatement | ForOfStatement,
    idNode: ObjectPattern
  ): Ast = {
    // surrounding block:
    val blockNode_ = blockNode(forInOfStmt, forInOfStmt.code, Defines.Any)
    scope.pushNewBlockScope(blockNode_)
    localAstParentStack.push(blockNode_)

    val collection     = rightOf(forInOfStmt)
    val collectionName = collection.code

    // _iterator assignment:
    val iteratorName      = generateUnusedVariableName(usedVariableNames, "_iterator")
    val iteratorLocalNode = localNode(forInOfStmt, iteratorName, iteratorName, Defines.Any).order(0)
    val iteratorNode      = identifierNode(forInOfStmt, iteratorName)
    diffGraph.addEdge(localAstParentStack.head, iteratorLocalNode, EdgeTypes.AST)
    scope.addVariableReference(iteratorName, iteratorNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)
    // TODO: add operator to schema
    val iteratorCall =
      callNode(
        forInOfStmt,
        s"<operator>.iterator($collectionName)",
        "<operator>.iterator",
        DispatchTypes.STATIC_DISPATCH
      )

    val objectKeysCallArgs = List(astForNodeWithFunctionReference(collection))
    val objectKeysCallAst  = callAst(iteratorCall, objectKeysCallArgs)

    val iteratorAssignmentNode =
      callNode(
        forInOfStmt,
        s"$iteratorName = <operator>.iterator($collectionName)",
        Operators.assignment,
        DispatchTypes.STATIC_DISPATCH
      )

    val iteratorAssignmentArgs = List(Ast(iteratorNode), objectKeysCallAst)
    val iteratorAssignmentAst  = callAst(iteratorAssignmentNode, iteratorAssignmentArgs)

    // _result:
    val resultName      = generateUnusedVariableName(usedVariableNames, "_result")
    val resultLocalNode = localNode(forInOfStmt, resultName, resultName, Defines.Any).order(0)
    val resultNode      = identifierNode(forInOfStmt, resultName)
    diffGraph.addEdge(localAstParentStack.head, resultLocalNode, EdgeTypes.AST)
    scope.addVariableReference(resultName, resultNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)

    // loop variable:
    val loopVariableNames = idNode.properties.map(property => stripQuotes(property.code)).toList

    val loopVariableLocalNodes = loopVariableNames.map(varName => localNode(forInOfStmt, varName, varName, Defines.Any))
    val loopVariableNodes      = loopVariableNames.map(identifierNode(forInOfStmt, _))
    loopVariableLocalNodes.foreach(diffGraph.addEdge(localAstParentStack.head, _, EdgeTypes.AST))
    loopVariableNames.zip(loopVariableNodes).foreach { case (loopVariableName, loopVariableNode) =>
      scope.addVariableReference(loopVariableName, loopVariableNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)
    }

    // while loop:
    // while loop test:
    val testCallNode =
      callNode(forInOfStmt, s"!($resultName = $iteratorName.next()).done", Operators.not, DispatchTypes.STATIC_DISPATCH)

    val doneBaseNode = callNode(
      forInOfStmt,
      s"($resultName = $iteratorName.next())",
      Operators.assignment,
      DispatchTypes.STATIC_DISPATCH
    )

    val lhsNode = identifierNode(forInOfStmt, resultName)

    val rhsNode = callNode(forInOfStmt, s"$iteratorName.next()", "next", DispatchTypes.DYNAMIC_DISPATCH)

    val nextBaseNode = identifierNode(forInOfStmt, iteratorName)

    val nextMemberNode = fieldIdentifierNode(forInOfStmt, "next", "next")

    val nextReceiverNode =
      createFieldAccessCallAst(nextBaseNode, nextMemberNode, forInOfStmt.lineNumber, forInOfStmt.columnNumber)

    val thisNextNode = identifierNode(forInOfStmt, iteratorName)

    val rhsArgs = List(Ast(thisNextNode))
    val rhsAst  = callAst(rhsNode, rhsArgs, receiver = Option(nextReceiverNode))

    val doneBaseArgs = List(Ast(lhsNode), rhsAst)
    val doneBaseAst  = callAst(doneBaseNode, doneBaseArgs)
    Ast.storeInDiffGraph(doneBaseAst, diffGraph)

    val doneMemberNode = fieldIdentifierNode(forInOfStmt, "done", "done")

    val testNode =
      createFieldAccessCallAst(doneBaseNode, doneMemberNode, forInOfStmt.lineNumber, forInOfStmt.columnNumber)

    val testCallArgs = List(testNode)
    val testCallAst  = callAst(testCallNode, testCallArgs)

    // while loop variable assignment:
    val loopVariableAssignmentAsts = loopVariableNames.map { loopVariableName =>
      val whileLoopVariableNode = identifierNode(forInOfStmt, loopVariableName)
      val baseNode              = identifierNode(forInOfStmt, resultName)
      val memberNode            = fieldIdentifierNode(forInOfStmt, "value", "value")
      val accessAst = createFieldAccessCallAst(baseNode, memberNode, forInOfStmt.lineNumber, forInOfStmt.columnNumber)
      val variableMemberNode = fieldIdentifierNode(forInOfStmt, loopVariableName, loopVariableName)
      val variableAccessAst  =
        createFieldAccessCallAst(accessAst, variableMemberNode, forInOfStmt.lineNumber, forInOfStmt.columnNumber)
      val loopVariableAssignmentNode = callNode(
        forInOfStmt,
        s"$loopVariableName = $resultName.value.$loopVariableName",
        Operators.assignment,
        DispatchTypes.STATIC_DISPATCH
      )
      val loopVariableAssignmentArgs = List(Ast(whileLoopVariableNode), variableAccessAst)
      callAst(loopVariableAssignmentNode, loopVariableAssignmentArgs)
    }

    val whileLoopBlockNode = blockNode(forInOfStmt, forInOfStmt.code, Defines.Any)
    scope.pushNewBlockScope(whileLoopBlockNode)
    localAstParentStack.push(whileLoopBlockNode)

    // while loop block:
    val bodyAst = astForNodeWithFunctionReference(bodyOf(forInOfStmt))

    val whileLoopBlockChildren = loopVariableAssignmentAsts :+ bodyAst
    val whileLoopBlockAst      = blockAst(whileLoopBlockNode, whileLoopBlockChildren)

    scope.popScope()
    localAstParentStack.pop()

    // end surrounding block:
    scope.popScope()
    localAstParentStack.pop()

    val whileLoopAst      = whileAst(forInOfStmt, Some(testCallAst), List(whileLoopBlockAst))
    val blockNodeChildren =
      List(iteratorAssignmentAst, Ast(resultNode)) ++ loopVariableNodes.map(Ast(_)) :+ whileLoopAst
    blockAst(blockNode_, blockNodeChildren)
  }

  /** De-sugaring from:
    *
    * for(var [a, b, c] of arr) { body }
    *
    * to:
    *
    * { var _iterator = <operator>.iterator(arr); var _result; var a; var b; var c; while (!(_result =
    * _iterator.next()).done) { a = _result.value[0]; b = _result.value[1]; c = _result.value[2]; body } }
    */
  private def astForInOfStatementWithArray(forInOfStmt: ForInStatement | ForOfStatement, idNode: ArrayPattern): Ast = {
    // surrounding block:
    val blockNode_ = blockNode(forInOfStmt, forInOfStmt.code, Defines.Any)
    scope.pushNewBlockScope(blockNode_)
    localAstParentStack.push(blockNode_)

    val collection     = rightOf(forInOfStmt)
    val collectionName = collection.code

    // _iterator assignment:
    val iteratorName      = generateUnusedVariableName(usedVariableNames, "_iterator")
    val iteratorLocalNode = localNode(forInOfStmt, iteratorName, iteratorName, Defines.Any).order(0)
    val iteratorNode      = identifierNode(forInOfStmt, iteratorName)
    diffGraph.addEdge(localAstParentStack.head, iteratorLocalNode, EdgeTypes.AST)
    scope.addVariableReference(iteratorName, iteratorNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)

    val iteratorCall = callNode(
      forInOfStmt,
      s"<operator>.iterator($collectionName)",
      "<operator>.iterator",
      DispatchTypes.STATIC_DISPATCH
    )

    val objectKeysCallArgs = List(astForNodeWithFunctionReference(collection))
    val objectKeysCallAst  = callAst(iteratorCall, objectKeysCallArgs)

    val iteratorAssignmentNode =
      callNode(
        forInOfStmt,
        s"$iteratorName = <operator>.iterator($collectionName)",
        Operators.assignment,
        DispatchTypes.STATIC_DISPATCH
      )

    val iteratorAssignmentArgs = List(Ast(iteratorNode), objectKeysCallAst)
    val iteratorAssignmentAst  = callAst(iteratorAssignmentNode, iteratorAssignmentArgs)

    // _result:
    val resultName      = generateUnusedVariableName(usedVariableNames, "_result")
    val resultLocalNode = localNode(forInOfStmt, resultName, resultName, Defines.Any).order(0)
    val resultNode      = identifierNode(forInOfStmt, resultName)
    diffGraph.addEdge(localAstParentStack.head, resultLocalNode, EdgeTypes.AST)
    scope.addVariableReference(resultName, resultNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)

    // loop variable:
    val loopVariableNames = idNode.elements.flatten.map(_.code).toList

    val loopVariableLocalNodes = loopVariableNames.map(varName => localNode(forInOfStmt, varName, varName, Defines.Any))
    val loopVariableNodes      = loopVariableNames.map(identifierNode(forInOfStmt, _))
    loopVariableLocalNodes.foreach(diffGraph.addEdge(localAstParentStack.head, _, EdgeTypes.AST))
    loopVariableNames.zip(loopVariableNodes).foreach { case (loopVariableName, loopVariableNode) =>
      scope.addVariableReference(loopVariableName, loopVariableNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)
    }

    // while loop:
    // while loop test:
    val testCallNode =
      callNode(forInOfStmt, s"!($resultName = $iteratorName.next()).done", Operators.not, DispatchTypes.STATIC_DISPATCH)

    val doneBaseNode = callNode(
      forInOfStmt,
      s"($resultName = $iteratorName.next())",
      Operators.assignment,
      DispatchTypes.STATIC_DISPATCH
    )

    val lhsNode = identifierNode(forInOfStmt, resultName)

    val rhsNode = callNode(forInOfStmt, s"$iteratorName.next()", "next", DispatchTypes.DYNAMIC_DISPATCH)

    val nextBaseNode = identifierNode(forInOfStmt, iteratorName)

    val nextMemberNode = fieldIdentifierNode(forInOfStmt, "next", "next")

    val nextReceiverNode =
      createFieldAccessCallAst(nextBaseNode, nextMemberNode, forInOfStmt.lineNumber, forInOfStmt.columnNumber)

    val thisNextNode = identifierNode(forInOfStmt, iteratorName)

    val rhsArgs = List(Ast(thisNextNode))
    val rhsAst  = callAst(rhsNode, rhsArgs, receiver = Option(nextReceiverNode))

    val doneBaseArgs = List(Ast(lhsNode), rhsAst)
    val doneBaseAst  = callAst(doneBaseNode, doneBaseArgs)
    Ast.storeInDiffGraph(doneBaseAst, diffGraph)

    val doneMemberNode = fieldIdentifierNode(forInOfStmt, "done", "done")

    val testNode =
      createFieldAccessCallAst(doneBaseNode, doneMemberNode, forInOfStmt.lineNumber, forInOfStmt.columnNumber)

    val testCallArgs = List(testNode)
    val testCallAst  = callAst(testCallNode, testCallArgs)

    // while loop variable assignment:
    val loopVariableAssignmentAsts = loopVariableNames.zipWithIndex.map { case (loopVariableName, index) =>
      val whileLoopVariableNode = identifierNode(forInOfStmt, loopVariableName)
      val baseNode              = identifierNode(forInOfStmt, resultName)
      val memberNode            = fieldIdentifierNode(forInOfStmt, "value", "value")
      val accessAst = createFieldAccessCallAst(baseNode, memberNode, forInOfStmt.lineNumber, forInOfStmt.columnNumber)
      val variableMemberNode = literalNode(forInOfStmt, index.toString, dynamicTypeOption = Some(Defines.Number))
      val variableAccessAst  =
        createIndexAccessCallAst(accessAst, Ast(variableMemberNode), forInOfStmt.lineNumber, forInOfStmt.columnNumber)
      val loopVariableAssignmentNode = callNode(
        forInOfStmt,
        s"$loopVariableName = $resultName.value[$index]",
        Operators.assignment,
        DispatchTypes.STATIC_DISPATCH
      )
      val loopVariableAssignmentArgs = List(Ast(whileLoopVariableNode), variableAccessAst)
      callAst(loopVariableAssignmentNode, loopVariableAssignmentArgs)
    }

    val whileLoopBlockNode = blockNode(forInOfStmt, forInOfStmt.code, Defines.Any)
    scope.pushNewBlockScope(whileLoopBlockNode)
    localAstParentStack.push(whileLoopBlockNode)

    // while loop block:
    val bodyAst = astForNodeWithFunctionReference(bodyOf(forInOfStmt))

    val whileLoopBlockChildren = loopVariableAssignmentAsts :+ bodyAst
    val whileLoopBlockAst      = blockAst(whileLoopBlockNode, whileLoopBlockChildren)

    scope.popScope()
    localAstParentStack.pop()

    // end surrounding block:
    scope.popScope()
    localAstParentStack.pop()

    val whileLoopAst      = whileAst(forInOfStmt, Some(testCallAst), List(whileLoopBlockAst))
    val blockNodeChildren =
      List(iteratorAssignmentAst, Ast(resultNode)) ++ loopVariableNodes.map(Ast(_)) :+ whileLoopAst
    blockAst(blockNode_, blockNodeChildren)
  }

  private def rightOf(stmt: ForInStatement | ForOfStatement): Expression = stmt match {
    case s: ForInStatement => s.right
    case s: ForOfStatement => s.right
  }

  private def bodyOf(stmt: ForInStatement | ForOfStatement): Statement = stmt match {
    case s: ForInStatement => s.body
    case s: ForOfStatement => s.body
  }

  private def extractLoopVariable(node: BabelNode): Option[BabelNode] = node match {
    case assignment: AssignmentPattern    => Option(assignment.left)
    case declaration: VariableDeclaration => declaration.declarations.headOption.map(_.id)
    case _                                => None
  }

  protected def astForInOfStatement(forInOfStmt: ForInStatement | ForOfStatement): Ast = {
    val loopVariable = forInOfStmt match {
      case s: ForInStatement => s.left
      case s: ForOfStatement => s.left
    }
    // check iteration loop variable type:
    loopVariable match {
      case _: (VariableDeclaration | AssignmentPattern) =>
        extractLoopVariable(loopVariable) match {
          case Some(pattern: ObjectPattern) => astForInOfStatementWithObject(forInOfStmt, pattern)
          case Some(pattern: ArrayPattern)  => astForInOfStatementWithArray(forInOfStmt, pattern)
          case Some(id: Identifier)         => astForInOfStatementWithIdentifier(forInOfStmt, id)
          case _                            => notHandledYet(forInOfStmt)
        }
      case pattern: ObjectPattern => astForInOfStatementWithObject(forInOfStmt, pattern)
      case pattern: ArrayPattern  => astForInOfStatementWithArray(forInOfStmt, pattern)
      case id: Identifier         => astForInOfStatementWithIdentifier(forInOfStmt, id)
      case _: Expression          => astForInOfStatementWithExpression(forInOfStmt, loopVariable)
    }
  }

}
