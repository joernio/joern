package io.joern.jssrc2cpg.astcreation

import io.joern.x2cpg.datastructures.Stack._
import io.joern.jssrc2cpg.parser.BabelAst
import io.joern.jssrc2cpg.parser.BabelNodeInfo
import io.joern.jssrc2cpg.passes.Defines
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.Operators
import ujson.Obj
import ujson.Value

trait AstForStatementsCreator {

  this: AstCreator =>

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
        case BabelAst.FunctionDeclaration                                  => 0
        case BabelAst.DeclareTypeAlias if isPlainTypeAlias(nodeInfo)       => 3
        case BabelAst.TypeAlias if isPlainTypeAlias(nodeInfo)              => 3
        case BabelAst.TSTypeAliasDeclaration if isPlainTypeAlias(nodeInfo) => 3
        case BabelAst.DeclareTypeAlias                                     => 2
        case BabelAst.TypeAlias                                            => 2
        case BabelAst.TSTypeAliasDeclaration                               => 2
        case _                                                             => 1
      }
    }

  protected def createBlockStatementAsts(json: Value): List[Ast] = {
    val blockStmts = sortBlockStatements(json.arr.map(createBabelNodeInfo).toList)
    val blockAsts = blockStmts.map { nodeInfo =>
      nodeInfo.node match {
        case BabelAst.FunctionDeclaration =>
          astForFunctionDeclaration(nodeInfo, shouldCreateAssignmentCall = true, shouldCreateFunctionReference = true)
        case _ => astForNode(nodeInfo.json)
      }
    }
    setIndices(blockAsts)
    blockAsts
  }

  protected def astForBlockStatement(block: BabelNodeInfo): Ast = {
    val blockNode = createBlockNode(block)
    scope.pushNewBlockScope(blockNode)
    localAstParentStack.push(blockNode)
    val blockStatementAsts = createBlockStatementAsts(block.json("body"))
    setIndices(blockStatementAsts)
    localAstParentStack.pop()
    scope.popScope()
    Ast(blockNode).withChildren(blockStatementAsts)
  }

  protected def astForReturnStatement(ret: BabelNodeInfo): Ast = {
    val retNode = createReturnNode(ret)
    safeObj(ret.json, "argument")
      .map { argument =>
        val argAst = astForNode(Obj(argument))
        createReturnAst(retNode, List(argAst))
      }
      .getOrElse(Ast(retNode))
  }

  private def astForCatchClause(catchClause: BabelNodeInfo): Ast =
    astForNode(catchClause.json("body"))

  protected def astForTryStatement(tryStmt: BabelNodeInfo): Ast = {
    val tryNode = createControlStructureNode(tryStmt, ControlStructureTypes.TRY)

    val bodyAst = astForNode(tryStmt.json("block"))

    val catchAst = safeObj(tryStmt.json, "handler")
      .map { handler =>
        astForCatchClause(createBabelNodeInfo(Obj(handler)))
      }
      .getOrElse(Ast())

    val finalizerAst = safeObj(tryStmt.json, "finalizer")
      .map { finalizer =>
        astForNode(Obj(finalizer))
      }
      .getOrElse(Ast())

    val tryChildren = List(bodyAst, catchAst, finalizerAst)
    setIndices(tryChildren)
    Ast(tryNode).withChildren(tryChildren)
  }

  def astForIfStatement(ifStmt: BabelNodeInfo): Ast = {
    val ifNode        = createControlStructureNode(ifStmt, ControlStructureTypes.IF)
    val testAst       = astForNode(ifStmt.json("test"))
    val consequentAst = astForNode(ifStmt.json("consequent"))
    val alternateAst = safeObj(ifStmt.json, "alternate")
      .map { alternate =>
        astForNode(Obj(alternate))
      }
      .getOrElse(Ast())
    val ifChildren = List(testAst, consequentAst, alternateAst)
    setIndices(ifChildren)
    Ast(ifNode)
      .withChild(testAst)
      .withConditionEdge(ifNode, testAst.nodes.head)
      .withChild(consequentAst)
      .withChild(alternateAst)
  }

  protected def astForDoWhileStatement(doWhileStmt: BabelNodeInfo): Ast = {
    val whileNode = createControlStructureNode(doWhileStmt, ControlStructureTypes.DO)
    val testAst   = astForNode(doWhileStmt.json("test"))
    val bodyAst   = astForNode(doWhileStmt.json("body"))
    setIndices(List(bodyAst, testAst))
    Ast(whileNode).withChild(bodyAst).withChild(testAst).withConditionEdge(whileNode, testAst.nodes.head)
  }

  protected def astForWhileStatement(whileStmt: BabelNodeInfo): Ast = {
    val whileNode = createControlStructureNode(whileStmt, ControlStructureTypes.WHILE)
    val testAst   = astForNode(whileStmt.json("test"))
    val bodyAst   = astForNode(whileStmt.json("body"))
    setIndices(List(testAst, bodyAst))
    Ast(whileNode).withChild(testAst).withConditionEdge(whileNode, testAst.nodes.head).withChild(bodyAst)
  }

  protected def astForForStatement(forStmt: BabelNodeInfo): Ast = {
    val forNode = createControlStructureNode(forStmt, ControlStructureTypes.FOR)
    val initAst = safeObj(forStmt.json, "init")
      .map { init =>
        astForNode(Obj(init))
      }
      .getOrElse(Ast())
    val testAst = safeObj(forStmt.json, "test")
      .map { test =>
        astForNode(Obj(test))
      }
      .getOrElse(Ast(createLiteralNode("true", Some(Defines.BOOLEAN.label), forStmt.lineNumber, forStmt.columnNumber)))
    val updateAst = safeObj(forStmt.json, "update")
      .map { update =>
        astForNode(Obj(update))
      }
      .getOrElse(Ast())
    val bodyAst = astForNode(forStmt.json("body"))
    setIndices(List(initAst, testAst, updateAst, bodyAst))
    Ast(forNode).withChild(initAst).withChild(testAst).withChild(updateAst).withChild(bodyAst)
  }

  protected def astForBreakStatement(breakStmt: BabelNodeInfo): Ast =
    Ast(createControlStructureNode(breakStmt, ControlStructureTypes.BREAK))

  protected def astForContinueStatement(continueStmt: BabelNodeInfo): Ast =
    Ast(createControlStructureNode(continueStmt, ControlStructureTypes.CONTINUE))

  protected def astForThrowStatement(throwStmt: BabelNodeInfo): Ast = {
    val argumentAst = astForNode(throwStmt.json("argument"))
    val throwCallNode =
      createCallNode(
        throwStmt.code,
        "<operator>.throw",
        DispatchTypes.STATIC_DISPATCH,
        throwStmt.lineNumber,
        throwStmt.columnNumber
      )
    val argAsts = List(argumentAst)
    createCallAst(throwCallNode, argAsts)
  }

  private def astsForSwitchCase(switchCase: BabelNodeInfo): List[Ast] = {
    val labelAst       = Ast(createJumpTarget(switchCase))
    val testAsts       = safeObj(switchCase.json, "test").map(t => astForNodeWithFunctionReference(Obj(t))).toList
    val consequentAsts = astForNodes(switchCase.json("consequent").arr.toList)
    labelAst +: (testAsts ++ consequentAsts)
  }

  protected def astForSwitchStatement(switchStmt: BabelNodeInfo): Ast = {
    val switchNode = createControlStructureNode(switchStmt, ControlStructureTypes.SWITCH)

    val switchExpressionAst = astForNode(switchStmt.json("discriminant"))

    val blockNode = createBlockNode(switchStmt)
    scope.pushNewBlockScope(blockNode)
    localAstParentStack.push(blockNode)

    val casesAsts = switchStmt.json("cases").arr.flatMap(c => astsForSwitchCase(createBabelNodeInfo(c)))
    setIndices(casesAsts.toList)

    scope.popScope()
    localAstParentStack.pop()

    setIndices(List(switchExpressionAst, Ast(blockNode)))
    Ast(switchNode)
      .withChild(switchExpressionAst)
      .withConditionEdge(switchNode, switchExpressionAst.nodes.head)
      .withChild(Ast(blockNode).withChildren(casesAsts))
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
    val iteratorLocalNode = createLocalNode(iteratorName, Defines.ANY.label)
    diffGraph.addEdge(localAstParentStack.head, iteratorLocalNode, EdgeTypes.AST)

    val iteratorNode = createIdentifierNode(iteratorName, forInOfStmt)

    val callNode = createCallNode(
      "Object.keys(" + collectionName + ")[Symbol.iterator]()",
      "",
      DispatchTypes.DYNAMIC_DISPATCH,
      forInOfStmt.lineNumber,
      forInOfStmt.columnNumber
    )

    val thisNode = createIdentifierNode("this", forInOfStmt)

    val indexCallNode = createCallNode(
      "Object.keys(" + collectionName + ")[Symbol.iterator]",
      Operators.indexAccess,
      DispatchTypes.STATIC_DISPATCH,
      forInOfStmt.lineNumber,
      forInOfStmt.columnNumber
    )

    val objectKeysCallNode = createStaticCallNode(
      "Object.keys(" + collectionName + ")",
      "keys",
      "Object.keys",
      forInOfStmt.lineNumber,
      forInOfStmt.columnNumber
    )

    val objectKeysCallArgs = List(astForNode(collection))
    val objectKeysCallAst  = createCallAst(objectKeysCallNode, objectKeysCallArgs)

    val indexBaseNode = createIdentifierNode("Symbol", forInOfStmt)

    val indexMemberNode = createFieldIdentifierNode("iterator", forInOfStmt.lineNumber, forInOfStmt.columnNumber)

    val indexAccessNode =
      createFieldAccessCallAst(indexBaseNode, indexMemberNode, forInOfStmt.lineNumber, forInOfStmt.columnNumber)

    val indexCallArgs = List(objectKeysCallAst, indexAccessNode)
    val indexCallAst  = createCallAst(indexCallNode, indexCallArgs)

    val callNodeArgs = List(Ast(thisNode))
    val callNodeAst  = createCallAst(callNode, callNodeArgs, receiver = Some(indexCallAst))

    val iteratorAssignmentNode =
      createCallNode(
        iteratorName + " = " + "Object.keys(" + collectionName + ")[Symbol.iterator]()",
        Operators.assignment,
        DispatchTypes.STATIC_DISPATCH,
        forInOfStmt.lineNumber,
        forInOfStmt.columnNumber
      )

    val iteratorAssignmentArgs = List(Ast(iteratorNode), callNodeAst)
    val iteratorAssignmentAst  = createCallAst(iteratorAssignmentNode, iteratorAssignmentArgs)

    // _result:
    val resultName      = generateUnusedVariableName(usedVariableNames, "_result")
    val resultLocalNode = createLocalNode(resultName, Defines.ANY.label)
    diffGraph.addEdge(localAstParentStack.head, resultLocalNode, EdgeTypes.AST)
    val resultNode = createIdentifierNode(resultName, forInOfStmt)

    // loop variable:
    val nodeInfo = createBabelNodeInfo(forInOfStmt.json("left"))
    val loopVariableName = nodeInfo.node match {
      case BabelAst.VariableDeclaration => code(nodeInfo.json("declarations").arr.head)
      case _                            => code(nodeInfo.json)
    }

    val loopVariableLocalNode = createLocalNode(loopVariableName, Defines.ANY.label)
    diffGraph.addEdge(localAstParentStack.head, loopVariableLocalNode, EdgeTypes.AST)
    val loopVariableNode = createIdentifierNode(loopVariableName, forInOfStmt)

    // while loop:
    val whileLoopNode =
      createControlStructureNode(forInOfStmt, ControlStructureTypes.WHILE)

    // while loop test:
    val testCallNode = createCallNode(
      "!(" + resultName + " = " + iteratorName + ".next()).done",
      Operators.not,
      DispatchTypes.STATIC_DISPATCH,
      forInOfStmt.lineNumber,
      forInOfStmt.columnNumber
    )

    val doneBaseNode = createCallNode(
      "(" + resultName + " = " + iteratorName + ".next())",
      Operators.assignment,
      DispatchTypes.STATIC_DISPATCH,
      forInOfStmt.lineNumber,
      forInOfStmt.columnNumber
    )

    val lhsNode = createIdentifierNode(resultName, forInOfStmt)

    val rhsNode = createCallNode(
      iteratorName + ".next()",
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
    val rhsAst  = createCallAst(rhsNode, rhsArgs, receiver = Some(nextReceiverNode))

    val doneBaseArgs = List(Ast(lhsNode), rhsAst)
    val doneBaseAst  = createCallAst(doneBaseNode, doneBaseArgs)
    Ast.storeInDiffGraph(doneBaseAst, diffGraph)

    val doneMemberNode = createFieldIdentifierNode("done", forInOfStmt.lineNumber, forInOfStmt.columnNumber)

    val testNode =
      createFieldAccessCallAst(doneBaseNode, doneMemberNode, forInOfStmt.lineNumber, forInOfStmt.columnNumber)

    val testCallArgs = List(testNode)
    val testCallAst  = createCallAst(testCallNode, testCallArgs)

    val whileLoopAst = Ast(whileLoopNode).withChild(testCallAst).withConditionEdge(whileLoopNode, testCallNode)

    // while loop variable assignment:
    val whileLoopVariableNode = createIdentifierNode(loopVariableName, forInOfStmt)

    val baseNode = createIdentifierNode(resultName, forInOfStmt)

    val memberNode = createFieldIdentifierNode("value", forInOfStmt.lineNumber, forInOfStmt.columnNumber)

    val accessAst = createFieldAccessCallAst(baseNode, memberNode, forInOfStmt.lineNumber, forInOfStmt.columnNumber)

    val loopVariableAssignmentNode = createCallNode(
      loopVariableName + " = " + resultName + ".value",
      Operators.assignment,
      DispatchTypes.STATIC_DISPATCH,
      forInOfStmt.lineNumber,
      forInOfStmt.columnNumber
    )

    val loopVariableAssignmentArgs = List(Ast(whileLoopVariableNode), accessAst)
    val loopVariableAssignmentAst  = createCallAst(loopVariableAssignmentNode, loopVariableAssignmentArgs)

    val whileLoopBlockNode = createBlockNode(forInOfStmt)
    scope.pushNewBlockScope(whileLoopBlockNode)
    localAstParentStack.push(whileLoopBlockNode)

    // while loop block:
    val bodyAst = astForNode(forInOfStmt.json("body"))

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
