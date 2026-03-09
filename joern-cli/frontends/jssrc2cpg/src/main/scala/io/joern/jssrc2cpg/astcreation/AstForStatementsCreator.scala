package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.parser.BabelAst.*
import io.joern.jssrc2cpg.parser.BabelNodeInfo
import io.joern.x2cpg.Ast
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.frontendspecific.jssrc2cpg.Defines
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.NewJumpLabel
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import ujson.Obj
import ujson.Value

trait AstForStatementsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  /** Sort all block statements with the following result:
    *   - all function declarations go first
    *   - all type aliases that are not plain type references go last
    *   - all remaining type aliases go before that
    *   - all remaining statements go second
    *
    * We do this to get TypeDecls created at the right spot so we can make use of them for the type aliases.
    */
  private def sortBlockStatements(blockStatements: List[BabelNodeInfo]): List[BabelNodeInfo] =
    blockStatements.sortBy { nodeInfo =>
      nodeInfo.node match {
        case ImportDeclaration                                    => 0
        case FunctionDeclaration                                  => 1
        case DeclareTypeAlias if isPlainTypeAlias(nodeInfo)       => 4
        case TypeAlias if isPlainTypeAlias(nodeInfo)              => 4
        case TSTypeAliasDeclaration if isPlainTypeAlias(nodeInfo) => 4
        case DeclareTypeAlias                                     => 3
        case TypeAlias                                            => 3
        case TSTypeAliasDeclaration                               => 3
        case _                                                    => 2
      }
    }

  protected def createBlockStatementAsts(json: Value): List[Ast] = {
    val blockStmts = sortBlockStatements(json.arr.map(createBabelNodeInfo).toList)
    blockStmts.map(stmt => astForNodeWithFunctionReferenceAndCall(stmt.json))
  }

  protected def astForWithStatement(withStatement: BabelNodeInfo): Ast = {
    val blockNode_ = blockNode(withStatement, withStatement.code, Defines.Any)
    scope.pushNewBlockScope(blockNode_)
    localAstParentStack.push(blockNode_)
    val objectAst    = astForNodeWithFunctionReferenceAndCall(withStatement.json("object"))
    val bodyNodeInfo = createBabelNodeInfo(withStatement.json("body"))
    val bodyAsts = bodyNodeInfo.node match {
      case BlockStatement => createBlockStatementAsts(bodyNodeInfo.json("body"))
      case _              => List(astForNodeWithFunctionReferenceAndCall(bodyNodeInfo.json))
    }
    val blockStatementAsts = objectAst +: bodyAsts
    localAstParentStack.pop()
    scope.popScope()
    blockAst(blockNode_, blockStatementAsts)
  }

  protected def astForBlockStatement(block: BabelNodeInfo): Ast = {
    val blockNode_ = blockNode(block, block.code, Defines.Any)
    scope.pushNewBlockScope(blockNode_)
    localAstParentStack.push(blockNode_)
    val blockStatementAsts = createBlockStatementAsts(block.json("body"))
    localAstParentStack.pop()
    scope.popScope()
    blockAst(blockNode_, blockStatementAsts)
  }

  protected def astForReturnStatement(ret: BabelNodeInfo): Ast = {
    val retCode = ret.code.stripSuffix(";")
    val retNode = returnNode(ret, retCode)
    safeObj(ret.json, "argument")
      .map { argument =>
        val argAst = astForNodeWithFunctionReference(Obj(argument))
        returnAst(retNode, List(argAst))
      }
      .getOrElse(Ast(retNode))
  }

  private def astForCatchClause(catchClause: BabelNodeInfo): Ast = {
    val blockNode_ = blockNode(catchClause, catchClause.code, Defines.Any)
    scope.pushNewBlockScope(blockNode_)
    localAstParentStack.push(blockNode_)
    val paramAst = safeObj(catchClause.json, "param")
      .map { param => astForNodeWithFunctionReference(Obj(param)) }
      .getOrElse(Ast())
    val bodyAsts           = createBlockStatementAsts(catchClause.json("body")("body"))
    val blockStatementAsts = paramAst +: bodyAsts
    localAstParentStack.pop()
    scope.popScope()
    blockAst(blockNode_, blockStatementAsts)
  }

  protected def astForTryStatement(tryStmt: BabelNodeInfo): Ast = {
    val tryNode = controlStructureNode(tryStmt, ControlStructureTypes.TRY, code(tryStmt))
    val bodyAst = astForNodeWithFunctionReference(tryStmt.json("block"))
    val catchAst = safeObj(tryStmt.json, "handler").toList
      .map { handler =>
        val catchNodeInfo = createBabelNodeInfo(Obj(handler))
        val catchNode     = controlStructureNode(catchNodeInfo, ControlStructureTypes.CATCH, code(catchNodeInfo))
        val catchAst      = astForCatchClause(catchNodeInfo)
        Ast(catchNode).withChild(catchAst)
      }
    val finalizerAst = safeObj(tryStmt.json, "finalizer")
      .map { finalizer =>
        val finalNodeInfo = createBabelNodeInfo(Obj(finalizer))
        val finalNode     = controlStructureNode(finalNodeInfo, ControlStructureTypes.FINALLY, code(finalNodeInfo))
        val finalAst      = astForNodeWithFunctionReference(finalNodeInfo.json)
        Ast(finalNode).withChild(finalAst)
      }
    tryCatchAst(tryNode, bodyAst, catchAst, finalizerAst)
  }

  def astForIfStatement(ifStmt: BabelNodeInfo): Ast = {
    val ifNode        = controlStructureNode(ifStmt, ControlStructureTypes.IF, code(ifStmt))
    val testAst       = astForNodeWithFunctionReference(ifStmt.json("test"))
    val consequentAst = astForNodeWithFunctionReference(ifStmt.json("consequent"))
    val alternateAst = safeObj(ifStmt.json, "alternate")
      .map { alternate => astForNodeWithFunctionReference(Obj(alternate)) }
      .getOrElse(Ast())
    // The semantics of if statement children is partially defined by their order value.
    // The consequentAst must have order == 2 and alternateAst must have order == 3.
    // Only to avoid collision we set testAst to 1
    // because the semantics of it is already indicated via the condition edge.
    setOrderExplicitly(testAst, 1)
    setOrderExplicitly(consequentAst, 2)
    setOrderExplicitly(alternateAst, 3)
    Ast(ifNode)
      .withChild(testAst)
      .withConditionEdge(ifNode, testAst.nodes.head)
      .withChild(consequentAst)
      .withChild(alternateAst)
  }

  protected def astForDoWhileStatement(doWhileStmt: BabelNodeInfo): Ast = {
    val whileNode = controlStructureNode(doWhileStmt, ControlStructureTypes.DO, code(doWhileStmt))
    val testAst   = astForNodeWithFunctionReference(doWhileStmt.json("test"))
    val bodyAst   = astForNodeWithFunctionReference(doWhileStmt.json("body"))
    // The semantics of do-while statement children is partially defined by their order value.
    // The bodyAst must have order == 1. Only to avoid collision we set testAst to 2
    // because the semantics of it is already indicated via the condition edge.
    setOrderExplicitly(bodyAst, 1)
    setOrderExplicitly(testAst, 2)
    Ast(whileNode).withChild(bodyAst).withChild(testAst).withConditionEdge(whileNode, testAst.nodes.head)
  }

  protected def astForWhileStatement(whileStmt: BabelNodeInfo): Ast = {
    val whileNode = controlStructureNode(whileStmt, ControlStructureTypes.WHILE, code(whileStmt))
    val testAst   = astForNodeWithFunctionReference(whileStmt.json("test"))
    val bodyAst   = astForNodeWithFunctionReference(whileStmt.json("body"))
    // The semantics of while statement children is partially defined by their order value.
    // The bodyAst must have order == 2. Only to avoid collision we set testAst to 1
    // because the semantics of it is already indicated via the condition edge.
    setOrderExplicitly(testAst, 1)
    setOrderExplicitly(bodyAst, 2)
    Ast(whileNode).withChild(testAst).withConditionEdge(whileNode, testAst.nodes.head).withChild(bodyAst)
  }

  protected def astForForStatement(forStmt: BabelNodeInfo): Ast = {
    val forNode = controlStructureNode(forStmt, ControlStructureTypes.FOR, code(forStmt))
    val initAst = safeObj(forStmt.json, "init")
      .map { init =>
        astForNodeWithFunctionReference(Obj(init))
      }
      .getOrElse(Ast())
    val testAst = safeObj(forStmt.json, "test")
      .map { test =>
        astForNodeWithFunctionReference(Obj(test))
      }
      .getOrElse(Ast(literalNode(forStmt, "true", Option(Defines.Boolean))))
    val updateAst = safeObj(forStmt.json, "update")
      .map { update =>
        astForNodeWithFunctionReference(Obj(update))
      }
      .getOrElse(Ast())
    val bodyAst = astForNodeWithFunctionReference(forStmt.json("body"))

    // The semantics of for statement children is defined by their order value.
    // Thus we set the here explicitly and do not rely on the usual consecutive
    // ordering.
    setOrderExplicitly(initAst, 1)
    setOrderExplicitly(testAst, 2)
    setOrderExplicitly(updateAst, 3)
    setOrderExplicitly(bodyAst, 4)
    Ast(forNode).withChild(initAst).withChild(testAst).withChild(updateAst).withChild(bodyAst)
  }

  protected def astForLabeledStatement(labelStmt: BabelNodeInfo): Ast = {
    val labelName   = code(labelStmt.json("label"))
    val labeledNode = jumpTargetNode(labelStmt, labelName, s"$labelName:", Option(labelStmt.node.toString))

    val blockNode_ = blockNode(labelStmt, labelStmt.code, Defines.Any)
    scope.pushNewBlockScope(blockNode_)
    localAstParentStack.push(blockNode_)
    val bodyAst = astForNodeWithFunctionReference(labelStmt.json("body"))
    scope.popScope()
    localAstParentStack.pop()

    val labelAsts = List(Ast(labeledNode), bodyAst)
    blockAst(blockNode_, labelAsts)
  }

  protected def astForBreakStatement(breakStmt: BabelNodeInfo): Ast = {
    val labelAst = safeObj(breakStmt.json, "label").toList.map { label =>
      val labelNode = Obj(label)
      val labelCode = code(labelNode)
      Ast(
        NewJumpLabel()
          .parserTypeName(breakStmt.node.toString)
          .name(labelCode)
          .code(labelCode)
          .lineNumber(breakStmt.lineNumber)
          .columnNumber(breakStmt.columnNumber)
          .order(1)
      )
    }
    Ast(controlStructureNode(breakStmt, ControlStructureTypes.BREAK, code(breakStmt))).withChildren(labelAst)
  }

  protected def astForContinueStatement(continueStmt: BabelNodeInfo): Ast = {
    val labelAst = safeObj(continueStmt.json, "label").toList
      .map { label =>
        val labelNode = Obj(label)
        val labelCode = code(labelNode)
        Ast(
          NewJumpLabel()
            .parserTypeName(continueStmt.node.toString)
            .name(labelCode)
            .code(labelCode)
            .lineNumber(continueStmt.lineNumber)
            .columnNumber(continueStmt.columnNumber)
            .order(1)
        )
      }
    Ast(controlStructureNode(continueStmt, ControlStructureTypes.CONTINUE, code(continueStmt))).withChildren(labelAst)
  }

  protected def astForThrowStatement(throwStmt: BabelNodeInfo): Ast = {
    val argumentAst = astForNodeWithFunctionReference(throwStmt.json("argument"))
    val throwCallNode =
      callNode(throwStmt, throwStmt.code, "<operator>.throw", DispatchTypes.STATIC_DISPATCH)
    val argAsts = List(argumentAst)
    callAst(throwCallNode, argAsts)
  }

  private def astsForSwitchCase(switchCase: BabelNodeInfo): List[Ast] = {
    val labelAst       = Ast(jumpTargetNode(switchCase))
    val testAsts       = safeObj(switchCase.json, "test").map(t => astForNodeWithFunctionReference(Obj(t))).toList
    val consequentAsts = astForNodes(switchCase.json("consequent").arr.toList)
    labelAst +: (testAsts ++ consequentAsts)
  }

  protected def astForSwitchStatement(switchStmt: BabelNodeInfo): Ast = {
    val switchNode = controlStructureNode(switchStmt, ControlStructureTypes.SWITCH, code(switchStmt))

    // The semantics of switch statement children is partially defined by their order value.
    // The blockAst must have order == 2. Only to avoid collision we set switchExpressionAst to 1
    // because the semantics of it is already indicated via the condition edge.
    val switchExpressionAst = astForNodeWithFunctionReference(switchStmt.json("discriminant"))
    setOrderExplicitly(switchExpressionAst, 1)

    val blockNode_ = blockNode(switchStmt, switchStmt.code, Defines.Any).order(2)
    scope.pushNewBlockScope(blockNode_)
    localAstParentStack.push(blockNode_)
    val casesAsts = switchStmt.json("cases").arr.flatMap(c => astsForSwitchCase(createBabelNodeInfo(c)))
    scope.popScope()
    localAstParentStack.pop()

    Ast(switchNode)
      .withChild(switchExpressionAst)
      .withConditionEdge(switchNode, switchExpressionAst.nodes.head)
      .withChild(blockAst(blockNode_, casesAsts.toList))
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
  private def astForInOfStatementWithIdentifier(forInOfStmt: BabelNodeInfo, idNodeInfo: BabelNodeInfo): Ast = {
    // surrounding block:
    val blockNode_ = blockNode(forInOfStmt, forInOfStmt.code, Defines.Any)
    scope.pushNewBlockScope(blockNode_)
    localAstParentStack.push(blockNode_)

    val collection     = forInOfStmt.json("right")
    val collectionName = code(collection)

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
    val loopVariableName = idNodeInfo.code

    val loopVariableLocalNode = localNode(forInOfStmt, loopVariableName, loopVariableName, Defines.Any).order(0)
    val loopVariableNode      = identifierNode(forInOfStmt, loopVariableName)
    diffGraph.addEdge(localAstParentStack.head, loopVariableLocalNode, EdgeTypes.AST)
    scope.addVariableReference(loopVariableName, loopVariableNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)

    // while loop:
    val whileLoopNode = controlStructureNode(forInOfStmt, ControlStructureTypes.WHILE, code(forInOfStmt))

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

    val whileLoopAst = Ast(whileLoopNode).withChild(testCallAst).withConditionEdge(whileLoopNode, testCallNode)

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
    val bodyAst = astForNodeWithFunctionReference(forInOfStmt.json("body"))

    val whileLoopBlockChildren = List(loopVariableAssignmentAst, bodyAst)
    val whileLoopBlockAst      = blockAst(whileLoopBlockNode, whileLoopBlockChildren)

    scope.popScope()
    localAstParentStack.pop()

    // end surrounding block:
    scope.popScope()
    localAstParentStack.pop()

    val blockChildren =
      List(iteratorAssignmentAst, Ast(resultNode), Ast(loopVariableNode), whileLoopAst.withChild(whileLoopBlockAst))
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
  private def astForInOfStatementWithExpression(forInOfStmt: BabelNodeInfo, idNodeInfo: BabelNodeInfo): Ast = {
    // surrounding block:
    val blockNode_ = blockNode(forInOfStmt, forInOfStmt.code, Defines.Any)
    scope.pushNewBlockScope(blockNode_)
    localAstParentStack.push(blockNode_)

    val collection     = forInOfStmt.json("right")
    val collectionName = code(collection)

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
    val whileLoopNode = controlStructureNode(forInOfStmt, ControlStructureTypes.WHILE, code(forInOfStmt))

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

    val whileLoopAst = Ast(whileLoopNode).withChild(testCallAst).withConditionEdge(whileLoopNode, testCallNode)

    // while loop variable assignment:
    val whileLoopVariableNode = astForNode(idNodeInfo.json)

    val baseNode = identifierNode(forInOfStmt, resultName)

    val memberNode = fieldIdentifierNode(forInOfStmt, "value", "value")

    val accessAst = createFieldAccessCallAst(baseNode, memberNode, forInOfStmt.lineNumber, forInOfStmt.columnNumber)

    val loopVariableAssignmentNode = callNode(
      forInOfStmt,
      s"${idNodeInfo.code} = $resultName.value",
      Operators.assignment,
      DispatchTypes.STATIC_DISPATCH
    )

    val loopVariableAssignmentArgs = List(whileLoopVariableNode, accessAst)
    val loopVariableAssignmentAst  = callAst(loopVariableAssignmentNode, loopVariableAssignmentArgs)

    val whileLoopBlockNode = blockNode(forInOfStmt, forInOfStmt.code, Defines.Any)
    scope.pushNewBlockScope(whileLoopBlockNode)
    localAstParentStack.push(whileLoopBlockNode)

    // while loop block:
    val bodyAst = astForNodeWithFunctionReference(forInOfStmt.json("body"))

    val whileLoopBlockChildren = List(loopVariableAssignmentAst, bodyAst)
    val whileLoopBlockAst      = blockAst(whileLoopBlockNode, whileLoopBlockChildren)

    scope.popScope()
    localAstParentStack.pop()

    // end surrounding block:
    scope.popScope()
    localAstParentStack.pop()

    val blockChildren = List(iteratorAssignmentAst, Ast(resultNode), whileLoopAst.withChild(whileLoopBlockAst))
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
  private def astForInOfStatementWithObject(forInOfStmt: BabelNodeInfo, idNodeInfo: BabelNodeInfo): Ast = {
    // surrounding block:
    val blockNode_ = blockNode(forInOfStmt, forInOfStmt.code, Defines.Any)
    scope.pushNewBlockScope(blockNode_)
    localAstParentStack.push(blockNode_)

    val collection     = forInOfStmt.json("right")
    val collectionName = code(collection)

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
    val loopVariableNames = idNodeInfo.json("properties").arr.toList.map(p => stripQuotes(code(p)))

    val loopVariableLocalNodes = loopVariableNames.map(varName => localNode(forInOfStmt, varName, varName, Defines.Any))
    val loopVariableNodes      = loopVariableNames.map(identifierNode(forInOfStmt, _))
    loopVariableLocalNodes.foreach(diffGraph.addEdge(localAstParentStack.head, _, EdgeTypes.AST))
    loopVariableNames.zip(loopVariableNodes).foreach { case (loopVariableName, loopVariableNode) =>
      scope.addVariableReference(loopVariableName, loopVariableNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)
    }

    // while loop:
    val whileLoopNode = controlStructureNode(forInOfStmt, ControlStructureTypes.WHILE, code(forInOfStmt))

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

    val whileLoopAst = Ast(whileLoopNode).withChild(testCallAst).withConditionEdge(whileLoopNode, testCallNode)

    // while loop variable assignment:
    val loopVariableAssignmentAsts = loopVariableNames.map { loopVariableName =>
      val whileLoopVariableNode = identifierNode(forInOfStmt, loopVariableName)
      val baseNode              = identifierNode(forInOfStmt, resultName)
      val memberNode            = fieldIdentifierNode(forInOfStmt, "value", "value")
      val accessAst = createFieldAccessCallAst(baseNode, memberNode, forInOfStmt.lineNumber, forInOfStmt.columnNumber)
      val variableMemberNode = fieldIdentifierNode(forInOfStmt, loopVariableName, loopVariableName)
      val variableAccessAst =
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
    val bodyAst = astForNodeWithFunctionReference(forInOfStmt.json("body"))

    val whileLoopBlockChildren = loopVariableAssignmentAsts :+ bodyAst
    val whileLoopBlockAst      = blockAst(whileLoopBlockNode, whileLoopBlockChildren)

    scope.popScope()
    localAstParentStack.pop()

    // end surrounding block:
    scope.popScope()
    localAstParentStack.pop()

    val blockNodeChildren =
      List(iteratorAssignmentAst, Ast(resultNode)) ++ loopVariableNodes.map(Ast(_)) :+ whileLoopAst.withChild(
        whileLoopBlockAst
      )
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
  private def astForInOfStatementWithArray(forInOfStmt: BabelNodeInfo, idNodeInfo: BabelNodeInfo): Ast = {
    // surrounding block:
    val blockNode_ = blockNode(forInOfStmt, forInOfStmt.code, Defines.Any)
    scope.pushNewBlockScope(blockNode_)
    localAstParentStack.push(blockNode_)

    val collection     = forInOfStmt.json("right")
    val collectionName = code(collection)

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
    val loopVariableNames = idNodeInfo.json("elements").arr.toList.map(code)

    val loopVariableLocalNodes = loopVariableNames.map(varName => localNode(forInOfStmt, varName, varName, Defines.Any))
    val loopVariableNodes      = loopVariableNames.map(identifierNode(forInOfStmt, _))
    loopVariableLocalNodes.foreach(diffGraph.addEdge(localAstParentStack.head, _, EdgeTypes.AST))
    loopVariableNames.zip(loopVariableNodes).foreach { case (loopVariableName, loopVariableNode) =>
      scope.addVariableReference(loopVariableName, loopVariableNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)
    }

    // while loop:
    val whileLoopNode = controlStructureNode(forInOfStmt, ControlStructureTypes.WHILE, code(forInOfStmt))

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

    val whileLoopAst = Ast(whileLoopNode).withChild(testCallAst).withConditionEdge(whileLoopNode, testCallNode)

    // while loop variable assignment:
    val loopVariableAssignmentAsts = loopVariableNames.zipWithIndex.map { case (loopVariableName, index) =>
      val whileLoopVariableNode = identifierNode(forInOfStmt, loopVariableName)
      val baseNode              = identifierNode(forInOfStmt, resultName)
      val memberNode            = fieldIdentifierNode(forInOfStmt, "value", "value")
      val accessAst = createFieldAccessCallAst(baseNode, memberNode, forInOfStmt.lineNumber, forInOfStmt.columnNumber)
      val variableMemberNode = literalNode(forInOfStmt, index.toString, dynamicTypeOption = Some(Defines.Number))
      val variableAccessAst =
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
    val bodyAst = astForNodeWithFunctionReference(forInOfStmt.json("body"))

    val whileLoopBlockChildren = loopVariableAssignmentAsts :+ bodyAst
    val whileLoopBlockAst      = blockAst(whileLoopBlockNode, whileLoopBlockChildren)

    scope.popScope()
    localAstParentStack.pop()

    // end surrounding block:
    scope.popScope()
    localAstParentStack.pop()

    val blockNodeChildren =
      List(iteratorAssignmentAst, Ast(resultNode)) ++ loopVariableNodes.map(Ast(_)) :+ whileLoopAst.withChild(
        whileLoopBlockAst
      )
    blockAst(blockNode_, blockNodeChildren)
  }

  private def extractLoopVariableNodeInfo(nodeInfo: BabelNodeInfo): Option[BabelNodeInfo] =
    nodeInfo.node match {
      case AssignmentPattern =>
        Option(createBabelNodeInfo(nodeInfo.json("left")))
      case VariableDeclaration =>
        val varDeclNodeInfo = createBabelNodeInfo(nodeInfo.json("declarations").arr.head)
        if (varDeclNodeInfo.node == VariableDeclarator) Option(createBabelNodeInfo(varDeclNodeInfo.json("id")))
        else None
      case _ => None
    }

  protected def astForInOfStatement(forInOfStmt: BabelNodeInfo): Ast = {
    val loopVariableNodeInfo = createBabelNodeInfo(forInOfStmt.json("left"))
    // check iteration loop variable type:
    loopVariableNodeInfo.node match {
      case VariableDeclaration | AssignmentPattern =>
        val idNodeInfo = extractLoopVariableNodeInfo(loopVariableNodeInfo)
        idNodeInfo.map(_.node) match {
          case Some(ObjectPattern) => astForInOfStatementWithObject(forInOfStmt, idNodeInfo.get)
          case Some(ArrayPattern)  => astForInOfStatementWithArray(forInOfStmt, idNodeInfo.get)
          case Some(Identifier)    => astForInOfStatementWithIdentifier(forInOfStmt, idNodeInfo.get)
          case _                   => notHandledYet(forInOfStmt)
        }
      case ObjectPattern => astForInOfStatementWithObject(forInOfStmt, loopVariableNodeInfo)
      case ArrayPattern  => astForInOfStatementWithArray(forInOfStmt, loopVariableNodeInfo)
      case Identifier    => astForInOfStatementWithIdentifier(forInOfStmt, loopVariableNodeInfo)
      case _: Expression => astForInOfStatementWithExpression(forInOfStmt, loopVariableNodeInfo)
      case _             => notHandledYet(forInOfStmt)
    }
  }

}
