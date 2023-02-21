package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.parser.BabelAst._
import io.joern.jssrc2cpg.parser.BabelNodeInfo
import io.joern.x2cpg.datastructures.Stack._
import io.joern.jssrc2cpg.passes.Defines
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.NewJumpLabel
import io.shiftleft.codepropertygraph.generated.nodes.NewJumpTarget
import ujson.Obj
import ujson.Value

trait AstForStatementsCreator { this: AstCreator =>

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
    val blockAsts  = blockStmts.map(stmt => astForNodeWithFunctionReferenceAndCall(stmt.json))
    setArgumentIndices(blockAsts)
    blockAsts
  }

  protected def astForBlockStatement(block: BabelNodeInfo): Ast = {
    val blockNode = createBlockNode(block)
    scope.pushNewBlockScope(blockNode)
    localAstParentStack.push(blockNode)
    val blockStatementAsts = createBlockStatementAsts(block.json("body"))
    setArgumentIndices(blockStatementAsts)
    localAstParentStack.pop()
    scope.popScope()
    Ast(blockNode).withChildren(blockStatementAsts)
  }

  protected def astForReturnStatement(ret: BabelNodeInfo): Ast = {
    val retNode = createReturnNode(ret)
    safeObj(ret.json, "argument").map { argument =>
      val argAst = astForNodeWithFunctionReference(Obj(argument))
      createReturnAst(retNode, List(argAst))
    }.getOrElse(Ast(retNode))
  }

  private def astForCatchClause(catchClause: BabelNodeInfo): Ast =
    astForNodeWithFunctionReference(catchClause.json("body"))

  protected def astForTryStatement(tryStmt: BabelNodeInfo): Ast = {
    val tryNode = createControlStructureNode(tryStmt, ControlStructureTypes.TRY)
    val bodyAst = astForNodeWithFunctionReference(tryStmt.json("block"))
    val catchAst = safeObj(tryStmt.json, "handler").map { handler =>
      astForCatchClause(createBabelNodeInfo(Obj(handler)))
    }.getOrElse(Ast())
    val finalizerAst = safeObj(tryStmt.json, "finalizer").map { finalizer =>
      astForNodeWithFunctionReference(Obj(finalizer))
    }.getOrElse(Ast())
    // The semantics of try statement children is defined by there order value.
    // Thus we set the here explicitly and do not rely on the usual consecutive
    // ordering.
    setOrderExplicitly(bodyAst, 1)
    setOrderExplicitly(catchAst, 2)
    setOrderExplicitly(finalizerAst, 3)
    Ast(tryNode).withChildren(List(bodyAst, catchAst, finalizerAst))
  }

  def astForIfStatement(ifStmt: BabelNodeInfo): Ast = {
    val ifNode        = createControlStructureNode(ifStmt, ControlStructureTypes.IF)
    val testAst       = astForNodeWithFunctionReference(ifStmt.json("test"))
    val consequentAst = astForNodeWithFunctionReference(ifStmt.json("consequent"))
    val alternateAst = safeObj(ifStmt.json, "alternate").map { alternate =>
      astForNodeWithFunctionReference(Obj(alternate))
    }.getOrElse(Ast())
    // The semantics of if statement children is partially defined by there order value.
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
    val whileNode = createControlStructureNode(doWhileStmt, ControlStructureTypes.DO)
    val testAst   = astForNodeWithFunctionReference(doWhileStmt.json("test"))
    val bodyAst   = astForNodeWithFunctionReference(doWhileStmt.json("body"))
    // The semantics of do-while statement children is partially defined by there order value.
    // The bodyAst must have order == 1. Only to avoid collision we set testAst to 2
    // because the semantics of it is already indicated via the condition edge.
    setOrderExplicitly(bodyAst, 1)
    setOrderExplicitly(testAst, 2)
    Ast(whileNode).withChild(bodyAst).withChild(testAst).withConditionEdge(whileNode, testAst.nodes.head)
  }

  protected def astForWhileStatement(whileStmt: BabelNodeInfo): Ast = {
    val whileNode = createControlStructureNode(whileStmt, ControlStructureTypes.WHILE)
    val testAst   = astForNodeWithFunctionReference(whileStmt.json("test"))
    val bodyAst   = astForNodeWithFunctionReference(whileStmt.json("body"))
    // The semantics of while statement children is partially defined by there order value.
    // The bodyAst must have order == 2. Only to avoid collision we set testAst to 1
    // because the semantics of it is already indicated via the condition edge.
    setOrderExplicitly(testAst, 1)
    setOrderExplicitly(bodyAst, 2)
    Ast(whileNode).withChild(testAst).withConditionEdge(whileNode, testAst.nodes.head).withChild(bodyAst)
  }

  protected def astForForStatement(forStmt: BabelNodeInfo): Ast = {
    val forNode = createControlStructureNode(forStmt, ControlStructureTypes.FOR)
    val initAst = safeObj(forStmt.json, "init").map { init =>
      astForNodeWithFunctionReference(Obj(init))
    }.getOrElse(Ast())
    val testAst = safeObj(forStmt.json, "test").map { test =>
      astForNodeWithFunctionReference(Obj(test))
    }.getOrElse(Ast(createLiteralNode("true", Option(Defines.Boolean), forStmt.lineNumber, forStmt.columnNumber)))
    val updateAst = safeObj(forStmt.json, "update").map { update =>
      astForNodeWithFunctionReference(Obj(update))
    }.getOrElse(Ast())
    val bodyAst = astForNodeWithFunctionReference(forStmt.json("body"))

    // The semantics of for statement children is defined by there order value.
    // Thus we set the here explicitly and do not rely on the usual consecutive
    // ordering.
    setOrderExplicitly(initAst, 1)
    setOrderExplicitly(testAst, 2)
    setOrderExplicitly(updateAst, 3)
    setOrderExplicitly(bodyAst, 4)
    Ast(forNode).withChild(initAst).withChild(testAst).withChild(updateAst).withChild(bodyAst)
  }

  protected def astForLabeledStatement(labelStmt: BabelNodeInfo): Ast = {
    val labelName = code(labelStmt.json("label"))
    val labeledNode = NewJumpTarget()
      .parserTypeName(labelStmt.node.toString)
      .name(labelName)
      .code(s"$labelName:")
      .lineNumber(labelStmt.lineNumber)
      .columnNumber(labelStmt.columnNumber)

    val blockNode = createBlockNode(labelStmt)
    scope.pushNewBlockScope(blockNode)
    localAstParentStack.push(blockNode)
    val bodyAst = astForNodeWithFunctionReference(labelStmt.json("body"))
    scope.popScope()
    localAstParentStack.pop()

    val labelAsts = List(Ast(labeledNode), bodyAst)
    setArgumentIndices(labelAsts)
    Ast(blockNode).withChildren(labelAsts)
  }

  protected def astForBreakStatement(breakStmt: BabelNodeInfo): Ast = {
    val labelAst = safeObj(breakStmt.json, "label").map { label =>
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
    }.getOrElse(Ast())
    Ast(createControlStructureNode(breakStmt, ControlStructureTypes.BREAK)).withChild(labelAst)
  }

  protected def astForContinueStatement(continueStmt: BabelNodeInfo): Ast = {
    val labelAst = safeObj(continueStmt.json, "label").map { label =>
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
    }.getOrElse(Ast())
    Ast(createControlStructureNode(continueStmt, ControlStructureTypes.CONTINUE)).withChild(labelAst)
  }

  protected def astForThrowStatement(throwStmt: BabelNodeInfo): Ast = {
    val argumentAst = astForNodeWithFunctionReference(throwStmt.json("argument"))
    val throwCallNode =
      createCallNode(
        throwStmt.code,
        "<operator>.throw",
        DispatchTypes.STATIC_DISPATCH,
        throwStmt.lineNumber,
        throwStmt.columnNumber
      )
    val argAsts = List(argumentAst)
    callAst(throwCallNode, argAsts)
  }

  private def astsForSwitchCase(switchCase: BabelNodeInfo): List[Ast] = {
    val labelAst       = Ast(createJumpTarget(switchCase))
    val testAsts       = safeObj(switchCase.json, "test").map(t => astForNodeWithFunctionReference(Obj(t))).toList
    val consequentAsts = astForNodes(switchCase.json("consequent").arr.toList)
    labelAst +: (testAsts ++ consequentAsts)
  }

  protected def astForSwitchStatement(switchStmt: BabelNodeInfo): Ast = {
    val switchNode = createControlStructureNode(switchStmt, ControlStructureTypes.SWITCH)

    val switchExpressionAst = astForNodeWithFunctionReference(switchStmt.json("discriminant"))

    val blockNode = createBlockNode(switchStmt)
    val blockAst  = Ast(blockNode)
    scope.pushNewBlockScope(blockNode)
    localAstParentStack.push(blockNode)

    val casesAsts = switchStmt.json("cases").arr.flatMap(c => astsForSwitchCase(createBabelNodeInfo(c)))
    setArgumentIndices(casesAsts.toList)

    scope.popScope()
    localAstParentStack.pop()

    // The semantics of switch statement children is partially defined by there order value.
    // The blockAst must have order == 2. Only to avoid collision we set switchExpressionAst to 1
    // because the semantics of it is already indicated via the condition edge.
    setOrderExplicitly(switchExpressionAst, 1)
    setOrderExplicitly(blockAst, 2)
    Ast(switchNode)
      .withChild(switchExpressionAst)
      .withConditionEdge(switchNode, switchExpressionAst.nodes.head)
      .withChild(blockAst.withChildren(casesAsts))
  }

  /** De-sugaring from:
    *
    * for (var i in/of arr) { body }
    *
    * to:
    *
    * { var _iterator = Object.keys(arr)[Symbol.iterator](); var _result; var i; while (!(_result =
    * _iterator.next()).done) { i = _result.value; body } }
    */
  protected def astForInOfStatement(forInOfStmt: BabelNodeInfo): Ast = {
    // surrounding block:
    val blockNode = createBlockNode(forInOfStmt)
    scope.pushNewBlockScope(blockNode)
    localAstParentStack.push(blockNode)

    val collection     = forInOfStmt.json("right")
    val collectionName = code(collection)

    // _iterator assignment:
    val iteratorName      = generateUnusedVariableName(usedVariableNames, "_iterator")
    val iteratorLocalNode = createLocalNode(iteratorName, Defines.Any)
    val iteratorNode      = createIdentifierNode(iteratorName, forInOfStmt)
    diffGraph.addEdge(localAstParentStack.head, iteratorLocalNode, EdgeTypes.AST)
    scope.addVariableReference(iteratorName, iteratorNode)

    val callNode = createCallNode(
      s"Object.keys($collectionName)[Symbol.iterator]()",
      "",
      DispatchTypes.DYNAMIC_DISPATCH,
      forInOfStmt.lineNumber,
      forInOfStmt.columnNumber
    )

    val thisNode = createIdentifierNode("this", forInOfStmt)

    val indexCallNode = createCallNode(
      s"Object.keys($collectionName)[Symbol.iterator]",
      Operators.indexAccess,
      DispatchTypes.STATIC_DISPATCH,
      forInOfStmt.lineNumber,
      forInOfStmt.columnNumber
    )

    val objectKeysCallNode = createStaticCallNode(
      s"Object.keys($collectionName)",
      "keys",
      "Object.keys",
      forInOfStmt.lineNumber,
      forInOfStmt.columnNumber
    )

    val objectKeysCallArgs = List(astForNodeWithFunctionReference(collection))
    val objectKeysCallAst  = callAst(objectKeysCallNode, objectKeysCallArgs)

    val indexBaseNode = createIdentifierNode("Symbol", forInOfStmt)

    val indexMemberNode = createFieldIdentifierNode("iterator", forInOfStmt.lineNumber, forInOfStmt.columnNumber)

    val indexAccessNode =
      createFieldAccessCallAst(indexBaseNode, indexMemberNode, forInOfStmt.lineNumber, forInOfStmt.columnNumber)

    val indexCallArgs = List(objectKeysCallAst, indexAccessNode)
    val indexCallAst  = callAst(indexCallNode, indexCallArgs)

    val callNodeArgs = List(Ast(thisNode))
    val callNodeAst  = callAst(callNode, callNodeArgs, receiver = Option(indexCallAst))

    val iteratorAssignmentNode =
      createCallNode(
        s"$iteratorName = Object.keys($collectionName)[Symbol.iterator]()",
        Operators.assignment,
        DispatchTypes.STATIC_DISPATCH,
        forInOfStmt.lineNumber,
        forInOfStmt.columnNumber
      )

    val iteratorAssignmentArgs = List(Ast(iteratorNode), callNodeAst)
    val iteratorAssignmentAst  = callAst(iteratorAssignmentNode, iteratorAssignmentArgs)

    // _result:
    val resultName      = generateUnusedVariableName(usedVariableNames, "_result")
    val resultLocalNode = createLocalNode(resultName, Defines.Any)
    val resultNode      = createIdentifierNode(resultName, forInOfStmt)
    diffGraph.addEdge(localAstParentStack.head, resultLocalNode, EdgeTypes.AST)
    scope.addVariableReference(resultName, resultNode)

    // loop variable:
    val nodeInfo = createBabelNodeInfo(forInOfStmt.json("left"))
    val loopVariableName = nodeInfo.node match {
      case VariableDeclaration => code(nodeInfo.json("declarations").arr.head)
      case _                   => code(nodeInfo.json)
    }

    val loopVariableLocalNode = createLocalNode(loopVariableName, Defines.Any)
    val loopVariableNode      = createIdentifierNode(loopVariableName, forInOfStmt)
    diffGraph.addEdge(localAstParentStack.head, loopVariableLocalNode, EdgeTypes.AST)
    scope.addVariableReference(loopVariableName, loopVariableNode)

    // while loop:
    val whileLoopNode =
      createControlStructureNode(forInOfStmt, ControlStructureTypes.WHILE)

    // while loop test:
    val testCallNode = createCallNode(
      s"!($resultName = $iteratorName.next()).done",
      Operators.not,
      DispatchTypes.STATIC_DISPATCH,
      forInOfStmt.lineNumber,
      forInOfStmt.columnNumber
    )

    val doneBaseNode = createCallNode(
      s"($resultName = $iteratorName.next())",
      Operators.assignment,
      DispatchTypes.STATIC_DISPATCH,
      forInOfStmt.lineNumber,
      forInOfStmt.columnNumber
    )

    val lhsNode = createIdentifierNode(resultName, forInOfStmt)

    val rhsNode = createCallNode(
      s"$iteratorName.next()",
      "",
      DispatchTypes.DYNAMIC_DISPATCH,
      forInOfStmt.lineNumber,
      forInOfStmt.columnNumber
    )

    val nextBaseNode = createIdentifierNode(iteratorName, forInOfStmt)

    val nextMemberNode = createFieldIdentifierNode("next", forInOfStmt.lineNumber, forInOfStmt.columnNumber)

    val nextReceiverNode =
      createFieldAccessCallAst(nextBaseNode, nextMemberNode, forInOfStmt.lineNumber, forInOfStmt.columnNumber)

    val thisNextNode = createIdentifierNode(iteratorName, forInOfStmt)

    val rhsArgs = List(Ast(thisNextNode))
    val rhsAst  = callAst(rhsNode, rhsArgs, receiver = Option(nextReceiverNode))

    val doneBaseArgs = List(Ast(lhsNode), rhsAst)
    val doneBaseAst  = callAst(doneBaseNode, doneBaseArgs)
    Ast.storeInDiffGraph(doneBaseAst, diffGraph)

    val doneMemberNode = createFieldIdentifierNode("done", forInOfStmt.lineNumber, forInOfStmt.columnNumber)

    val testNode =
      createFieldAccessCallAst(doneBaseNode, doneMemberNode, forInOfStmt.lineNumber, forInOfStmt.columnNumber)

    val testCallArgs = List(testNode)
    val testCallAst  = callAst(testCallNode, testCallArgs)

    val whileLoopAst = Ast(whileLoopNode).withChild(testCallAst).withConditionEdge(whileLoopNode, testCallNode)

    // while loop variable assignment:
    val whileLoopVariableNode = createIdentifierNode(loopVariableName, forInOfStmt)

    val baseNode = createIdentifierNode(resultName, forInOfStmt)

    val memberNode = createFieldIdentifierNode("value", forInOfStmt.lineNumber, forInOfStmt.columnNumber)

    val accessAst = createFieldAccessCallAst(baseNode, memberNode, forInOfStmt.lineNumber, forInOfStmt.columnNumber)

    val loopVariableAssignmentNode = createCallNode(
      s"$loopVariableName = $resultName.value",
      Operators.assignment,
      DispatchTypes.STATIC_DISPATCH,
      forInOfStmt.lineNumber,
      forInOfStmt.columnNumber
    )

    val loopVariableAssignmentArgs = List(Ast(whileLoopVariableNode), accessAst)
    val loopVariableAssignmentAst  = callAst(loopVariableAssignmentNode, loopVariableAssignmentArgs)

    val whileLoopBlockNode = createBlockNode(forInOfStmt)
    scope.pushNewBlockScope(whileLoopBlockNode)
    localAstParentStack.push(whileLoopBlockNode)

    // while loop block:
    val bodyAst = astForNodeWithFunctionReference(forInOfStmt.json("body"))

    val whileLoopBlockAst = Ast(whileLoopBlockNode).withChild(loopVariableAssignmentAst).withChild(bodyAst)

    scope.popScope()
    localAstParentStack.pop()

    // end surrounding block:
    scope.popScope()
    localAstParentStack.pop()

    Ast(blockNode)
      .withChild(iteratorAssignmentAst)
      .withChild(Ast(resultNode))
      .withChild(Ast(loopVariableNode))
      .withChild(whileLoopAst.withChild(whileLoopBlockAst))
  }

}
