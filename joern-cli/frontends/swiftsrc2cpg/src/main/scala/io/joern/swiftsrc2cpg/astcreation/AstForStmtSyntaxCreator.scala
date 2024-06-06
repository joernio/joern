package io.joern.swiftsrc2cpg.astcreation

import io.joern.swiftsrc2cpg.astcreation.AstCreatorHelper.OptionSafeAst
import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.*
import io.joern.x2cpg.Ast
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.codepropertygraph.generated.nodes.NewJumpLabel
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.Operators

trait AstForStmtSyntaxCreator(implicit withSchemaValidation: ValidationMode) {
  this: AstCreator =>

  private def astForBreakStmtSyntax(node: BreakStmtSyntax): Ast = {
    val labelAst = node.label.fold(Ast())(l =>
      val labelCode = code(l)
      Ast(
        NewJumpLabel()
          .parserTypeName(node.toString)
          .name(labelCode)
          .code(labelCode)
          .lineNumber(line(node))
          .columnNumber(column(node))
          .order(1)
      )
    )
    Ast(controlStructureNode(node, ControlStructureTypes.BREAK, code(node))).withChild(labelAst)
  }

  private def astForContinueStmtSyntax(node: ContinueStmtSyntax): Ast = {
    val labelAst = node.label.fold(Ast())(l =>
      val labelCode = code(l)
      Ast(
        NewJumpLabel()
          .parserTypeName(node.toString)
          .name(labelCode)
          .code(labelCode)
          .lineNumber(line(node))
          .columnNumber(column(node))
          .order(1)
      )
    )
    Ast(controlStructureNode(node, ControlStructureTypes.CONTINUE, code(node))).withChild(labelAst)
  }

  private def astForDeferStmtSyntax(node: DeferStmtSyntax): Ast = {
    astForNode(node.body)
  }

  private def astForDiscardStmtSyntax(node: DiscardStmtSyntax): Ast = notHandledYet(node)

  private def astForDoStmtSyntax(node: DoStmtSyntax): Ast = {
    val tryNode   = controlStructureNode(node, ControlStructureTypes.TRY, code(node))
    val bodyAst   = astForNode(node.body)
    val catchAsts = node.catchClauses.children.map(astForCatchHandler)
    tryCatchAst(tryNode, bodyAst, catchAsts, None)
  }

  private def astForCatchHandler(catchClause: CatchClauseSyntax): Ast = {
    val catchNode = controlStructureNode(catchClause, ControlStructureTypes.CATCH, code(catchClause))
    val declAst   = astForNode(catchClause.catchItems)
    val bodyAst   = astForNode(catchClause.body)
    setArgumentIndices(List(declAst, bodyAst))
    Ast(catchNode).withChild(declAst).withChild(bodyAst)
  }

  private def astForExpressionStmtSyntax(node: ExpressionStmtSyntax): Ast = {
    astForNodeWithFunctionReference(node.expression)
  }

  private def astForFallThroughStmtSyntax(node: FallThroughStmtSyntax): Ast = {
    Ast(controlStructureNode(node, ControlStructureTypes.CONTINUE, code(node)))
  }

  private def extractLoopVariableNodeInfo(binding: ValueBindingPatternSyntax): Option[PatternSyntax] = {
    binding.pattern match {
      case id: IdentifierPatternSyntax                        => Option(id)
      case expr: ExpressionPatternSyntax                      => Option(expr)
      case tuple: TuplePatternSyntax                          => Option(tuple)
      case _: WildcardPatternSyntax | _: MissingPatternSyntax => Option(binding.pattern)
      case _                                                  => None
    }
  }

  private def astForForStmtBody(node: ForStmtSyntax): Ast = {
    node.whereClause match {
      case Some(whereClause: WhereClauseSyntax) =>
        val ifNode  = controlStructureNode(whereClause.condition, ControlStructureTypes.IF, code(whereClause.condition))
        val testAst = astForNode(whereClause)
        val consequentAst = astForNode(node.body)
        setOrderExplicitly(testAst, 1)
        setOrderExplicitly(consequentAst, 2)
        Ast(ifNode)
          .withChild(testAst)
          .withConditionEdge(ifNode, testAst.nodes.head)
          .withChild(consequentAst)
      case None => astForNode(node.body)
    }
  }

  private def astForForStmtSyntaxWithWildcard(node: ForStmtSyntax): Ast = {
    val initAsts = Seq(astForNodeWithFunctionReference(node.sequence))
    val bodyAst  = astForForStmtBody(node)
    val forNode  = controlStructureNode(node, ControlStructureTypes.FOR, code(node))
    forAst(forNode, Nil, initAsts, Nil, Nil, bodyAst)
  }

  /** De-sugaring from:
    *
    * for i in arr { body }
    *
    * to:
    *
    * { var _iterator = <operator>.iterator(arr); var _result; var i; while (!(_result = _iterator.next()).done) { i =
    * _result.value; body } }
    */
  private def astForForStmtSyntaxWithIdentifier(node: ForStmtSyntax, id: IdentifierPatternSyntax): Ast = {
    // surrounding block:
    val blockNode_ = blockNode(node)
    scope.pushNewBlockScope(blockNode_)
    localAstParentStack.push(blockNode_)

    val collection     = node.sequence
    val collectionName = code(collection)

    // _iterator assignment:
    val iteratorName      = generateUnusedVariableName(usedVariableNames, "_iterator")
    val iteratorLocalNode = localNode(node, iteratorName, iteratorName, Defines.Any).order(0)
    val iteratorNode      = identifierNode(node, iteratorName)
    diffGraph.addEdge(localAstParentStack.head, iteratorLocalNode, EdgeTypes.AST)
    scope.addVariableReference(iteratorName, iteratorNode)

    val iteratorCall =
      // TODO: add operator to schema
      callNode(node, s"<operator>.iterator($collectionName)", "<operator>.iterator", DispatchTypes.STATIC_DISPATCH)

    val objectKeysCallArgs = List(astForNodeWithFunctionReference(collection))
    val objectKeysCallAst  = callAst(iteratorCall, objectKeysCallArgs)

    val iteratorAssignmentNode =
      callNode(
        node,
        s"$iteratorName = <operator>.iterator($collectionName)",
        Operators.assignment,
        DispatchTypes.STATIC_DISPATCH
      )

    val iteratorAssignmentArgs = List(Ast(iteratorNode), objectKeysCallAst)
    val iteratorAssignmentAst  = callAst(iteratorAssignmentNode, iteratorAssignmentArgs)

    // _result:
    val resultName      = generateUnusedVariableName(usedVariableNames, "_result")
    val resultLocalNode = localNode(node, resultName, resultName, Defines.Any).order(0)
    val resultNode      = identifierNode(node, resultName)
    diffGraph.addEdge(localAstParentStack.head, resultLocalNode, EdgeTypes.AST)
    scope.addVariableReference(resultName, resultNode)

    // loop variable:
    val loopVariableName = code(id.identifier)

    val loopVariableLocalNode = localNode(node, loopVariableName, loopVariableName, Defines.Any).order(0)
    val loopVariableNode      = identifierNode(node, loopVariableName)
    diffGraph.addEdge(localAstParentStack.head, loopVariableLocalNode, EdgeTypes.AST)
    scope.addVariableReference(loopVariableName, loopVariableNode)

    // while loop:
    val whileLoopNode = controlStructureNode(node, ControlStructureTypes.WHILE, code(node))

    // while loop test:
    val testCallNode =
      callNode(node, s"!($resultName = $iteratorName.next()).done", Operators.not, DispatchTypes.STATIC_DISPATCH)

    val doneBaseNode =
      callNode(node, s"($resultName = $iteratorName.next())", Operators.assignment, DispatchTypes.STATIC_DISPATCH)

    val lhsNode = identifierNode(node, resultName)

    val rhsNode = callNode(node, s"$iteratorName.next()", "next", DispatchTypes.DYNAMIC_DISPATCH)

    val nextBaseNode = identifierNode(node, iteratorName)

    val nextMemberNode = createFieldIdentifierNode("next", line(node), column(node))

    val nextReceiverNode =
      createFieldAccessCallAst(nextBaseNode, nextMemberNode, line(node), column(node))

    val thisNextNode = identifierNode(node, iteratorName)

    val rhsArgs = List(Ast(thisNextNode))
    val rhsAst  = callAst(rhsNode, rhsArgs, receiver = Option(nextReceiverNode))

    val doneBaseArgs = List(Ast(lhsNode), rhsAst)
    val doneBaseAst  = callAst(doneBaseNode, doneBaseArgs)
    Ast.storeInDiffGraph(doneBaseAst, diffGraph)

    val doneMemberNode = createFieldIdentifierNode("done", line(node), column(node))

    val testNode = createFieldAccessCallAst(doneBaseNode, doneMemberNode, line(node), column(node))

    val testCallArgs = List(testNode)
    val testCallAst  = callAst(testCallNode, testCallArgs)

    val whileLoopAst = Ast(whileLoopNode).withChild(testCallAst).withConditionEdge(whileLoopNode, testCallNode)

    // while loop variable assignment:
    val whileLoopVariableNode = identifierNode(node, loopVariableName)

    val baseNode = identifierNode(node, resultName)

    val memberNode = createFieldIdentifierNode("value", line(node), column(node))

    val accessAst = createFieldAccessCallAst(baseNode, memberNode, line(node), column(node))

    val loopVariableAssignmentNode =
      callNode(node, s"$loopVariableName = $resultName.value", Operators.assignment, DispatchTypes.STATIC_DISPATCH)

    val loopVariableAssignmentArgs = List(Ast(whileLoopVariableNode), accessAst)
    val loopVariableAssignmentAst  = callAst(loopVariableAssignmentNode, loopVariableAssignmentArgs)

    val whileLoopBlockNode = blockNode(node)
    scope.pushNewBlockScope(whileLoopBlockNode)
    localAstParentStack.push(whileLoopBlockNode)

    // while loop block:
    val bodyAst = astForForStmtBody(node)

    val whileLoopBlockChildren = List(loopVariableAssignmentAst, bodyAst)
    setArgumentIndices(whileLoopBlockChildren)
    val whileLoopBlockAst = blockAst(whileLoopBlockNode, whileLoopBlockChildren)

    scope.popScope()
    localAstParentStack.pop()

    // end surrounding block:
    scope.popScope()
    localAstParentStack.pop()

    val blockChildren =
      List(iteratorAssignmentAst, Ast(resultNode), Ast(loopVariableNode), whileLoopAst.withChild(whileLoopBlockAst))
    setArgumentIndices(blockChildren)
    blockAst(blockNode_, blockChildren)
  }

  /** De-sugaring from:
    *
    * for expr in arr { body }
    *
    * to:
    *
    * { var _iterator = <operator>.iterator(arr); var _result; while (!(_result = _iterator.next()).done) { expr =
    * _result.value; body } }
    */
  private def astForForStmtSyntaxWithExpression(node: ForStmtSyntax, id: ExpressionPatternSyntax): Ast = {
    // surrounding block:
    val blockNode_ = blockNode(node)
    scope.pushNewBlockScope(blockNode_)
    localAstParentStack.push(blockNode_)

    val collection     = node.sequence
    val collectionName = code(collection)

    // _iterator assignment:
    val iteratorName      = generateUnusedVariableName(usedVariableNames, "_iterator")
    val iteratorLocalNode = localNode(node, iteratorName, iteratorName, Defines.Any).order(0)
    val iteratorNode      = identifierNode(node, iteratorName)
    diffGraph.addEdge(localAstParentStack.head, iteratorLocalNode, EdgeTypes.AST)
    scope.addVariableReference(iteratorName, iteratorNode)

    val iteratorCall =
      // TODO: add operator to schema
      callNode(node, s"<operator>.iterator($collectionName)", "<operator>.iterator", DispatchTypes.STATIC_DISPATCH)

    val objectKeysCallArgs = List(astForNodeWithFunctionReference(collection))
    val objectKeysCallAst  = callAst(iteratorCall, objectKeysCallArgs)

    val iteratorAssignmentNode =
      callNode(
        node,
        s"$iteratorName = <operator>.iterator($collectionName)",
        Operators.assignment,
        DispatchTypes.STATIC_DISPATCH
      )

    val iteratorAssignmentArgs = List(Ast(iteratorNode), objectKeysCallAst)
    val iteratorAssignmentAst  = callAst(iteratorAssignmentNode, iteratorAssignmentArgs)

    // _result:
    val resultName      = generateUnusedVariableName(usedVariableNames, "_result")
    val resultLocalNode = localNode(node, resultName, resultName, Defines.Any).order(0)
    val resultNode      = identifierNode(node, resultName)
    diffGraph.addEdge(localAstParentStack.head, resultLocalNode, EdgeTypes.AST)
    scope.addVariableReference(resultName, resultNode)

    // while loop:
    val whileLoopNode = controlStructureNode(node, ControlStructureTypes.WHILE, code(node))

    // while loop test:
    val testCallNode =
      callNode(node, s"!($resultName = $iteratorName.next()).done", Operators.not, DispatchTypes.STATIC_DISPATCH)

    val doneBaseNode =
      callNode(node, s"($resultName = $iteratorName.next())", Operators.assignment, DispatchTypes.STATIC_DISPATCH)

    val lhsNode = identifierNode(node, resultName)

    val rhsNode = callNode(node, s"$iteratorName.next()", "next", DispatchTypes.DYNAMIC_DISPATCH)

    val nextBaseNode = identifierNode(node, iteratorName)

    val nextMemberNode = createFieldIdentifierNode("next", line(node), column(node))

    val nextReceiverNode =
      createFieldAccessCallAst(nextBaseNode, nextMemberNode, line(node), column(node))

    val thisNextNode = identifierNode(node, iteratorName)

    val rhsArgs = List(Ast(thisNextNode))
    val rhsAst  = callAst(rhsNode, rhsArgs, receiver = Option(nextReceiverNode))

    val doneBaseArgs = List(Ast(lhsNode), rhsAst)
    val doneBaseAst  = callAst(doneBaseNode, doneBaseArgs)
    Ast.storeInDiffGraph(doneBaseAst, diffGraph)

    val doneMemberNode = createFieldIdentifierNode("done", line(node), column(node))

    val testNode =
      createFieldAccessCallAst(doneBaseNode, doneMemberNode, line(node), column(node))

    val testCallArgs = List(testNode)
    val testCallAst  = callAst(testCallNode, testCallArgs)

    val whileLoopAst = Ast(whileLoopNode).withChild(testCallAst).withConditionEdge(whileLoopNode, testCallNode)

    // while loop variable assignment:
    val whileLoopVariableNode = astForNode(id)

    val baseNode = identifierNode(node, resultName)

    val memberNode = createFieldIdentifierNode("value", line(node), column(node))

    val accessAst = createFieldAccessCallAst(baseNode, memberNode, line(node), column(node))

    val loopVariableAssignmentNode =
      callNode(node, s"${code(id.expression)} = $resultName.value", Operators.assignment, DispatchTypes.STATIC_DISPATCH)

    val loopVariableAssignmentArgs = List(whileLoopVariableNode, accessAst)
    val loopVariableAssignmentAst  = callAst(loopVariableAssignmentNode, loopVariableAssignmentArgs)

    val whileLoopBlockNode = blockNode(node)
    scope.pushNewBlockScope(whileLoopBlockNode)
    localAstParentStack.push(whileLoopBlockNode)

    // while loop block:
    val bodyAst = astForForStmtBody(node)

    val whileLoopBlockChildren = List(loopVariableAssignmentAst, bodyAst)
    setArgumentIndices(whileLoopBlockChildren)
    val whileLoopBlockAst = blockAst(whileLoopBlockNode, whileLoopBlockChildren)

    scope.popScope()
    localAstParentStack.pop()

    // end surrounding block:
    scope.popScope()
    localAstParentStack.pop()

    val blockChildren = List(iteratorAssignmentAst, Ast(resultNode), whileLoopAst.withChild(whileLoopBlockAst))
    setArgumentIndices(blockChildren)
    blockAst(blockNode_, blockChildren)
  }

  /** De-sugaring from:
    *
    * for (a, b, c) of arr) { body }
    *
    * to:
    *
    * { var _iterator = <operator>.iterator(arr); var _result; var a; var b; var c; while (!(_result =
    * _iterator.next()).done) { a = _result.value._1; b = _result.value._2; c = _result.value._3; body } }
    */
  private def astForForStmtSyntaxWithTuple(node: ForStmtSyntax, id: TuplePatternSyntax): Ast = {
    // surrounding block:
    val blockNode_ = blockNode(node)
    scope.pushNewBlockScope(blockNode_)
    localAstParentStack.push(blockNode_)

    val collection     = node.sequence
    val collectionName = code(collection)

    // _iterator assignment:
    val iteratorName      = generateUnusedVariableName(usedVariableNames, "_iterator")
    val iteratorLocalNode = localNode(node, iteratorName, iteratorName, Defines.Any).order(0)
    val iteratorNode      = identifierNode(node, iteratorName)
    diffGraph.addEdge(localAstParentStack.head, iteratorLocalNode, EdgeTypes.AST)
    scope.addVariableReference(iteratorName, iteratorNode)
    // TODO: add operator to schema
    val iteratorCall =
      callNode(node, s"<operator>.iterator($collectionName)", "<operator>.iterator", DispatchTypes.STATIC_DISPATCH)

    val objectKeysCallArgs = List(astForNodeWithFunctionReference(collection))
    val objectKeysCallAst  = callAst(iteratorCall, objectKeysCallArgs)

    val iteratorAssignmentNode =
      callNode(
        node,
        s"$iteratorName = <operator>.iterator($collectionName)",
        Operators.assignment,
        DispatchTypes.STATIC_DISPATCH
      )

    val iteratorAssignmentArgs = List(Ast(iteratorNode), objectKeysCallAst)
    val iteratorAssignmentAst  = callAst(iteratorAssignmentNode, iteratorAssignmentArgs)

    // _result:
    val resultName      = generateUnusedVariableName(usedVariableNames, "_result")
    val resultLocalNode = localNode(node, resultName, resultName, Defines.Any).order(0)
    val resultNode      = identifierNode(node, resultName)
    diffGraph.addEdge(localAstParentStack.head, resultLocalNode, EdgeTypes.AST)
    scope.addVariableReference(resultName, resultNode)

    // loop variable:
    val loopVariableNames = id.elements.children.map(c => code(c.pattern))

    val loopVariableLocalNodes = loopVariableNames.map(varName => localNode(node, varName, varName, Defines.Any))
    val loopVariableNodes      = loopVariableNames.map(identifierNode(node, _))
    loopVariableLocalNodes.foreach(diffGraph.addEdge(localAstParentStack.head, _, EdgeTypes.AST))
    loopVariableNames.zip(loopVariableNodes).foreach { case (loopVariableName, loopVariableNode) =>
      scope.addVariableReference(loopVariableName, loopVariableNode)
    }

    // while loop:
    val whileLoopNode = controlStructureNode(node, ControlStructureTypes.WHILE, code(node))

    // while loop test:
    val testCallNode =
      callNode(node, s"!($resultName = $iteratorName.next()).done", Operators.not, DispatchTypes.STATIC_DISPATCH)

    val doneBaseNode =
      callNode(node, s"($resultName = $iteratorName.next())", Operators.assignment, DispatchTypes.STATIC_DISPATCH)

    val lhsNode = identifierNode(node, resultName)

    val rhsNode = callNode(node, s"$iteratorName.next()", "next", DispatchTypes.DYNAMIC_DISPATCH)

    val nextBaseNode = identifierNode(node, iteratorName)

    val nextMemberNode = createFieldIdentifierNode("next", line(node), column(node))

    val nextReceiverNode =
      createFieldAccessCallAst(nextBaseNode, nextMemberNode, line(node), column(node))

    val thisNextNode = identifierNode(node, iteratorName)

    val rhsArgs = List(Ast(thisNextNode))
    val rhsAst  = callAst(rhsNode, rhsArgs, receiver = Option(nextReceiverNode))

    val doneBaseArgs = List(Ast(lhsNode), rhsAst)
    val doneBaseAst  = callAst(doneBaseNode, doneBaseArgs)
    Ast.storeInDiffGraph(doneBaseAst, diffGraph)

    val doneMemberNode = createFieldIdentifierNode("done", line(node), column(node))

    val testNode =
      createFieldAccessCallAst(doneBaseNode, doneMemberNode, line(node), column(node))

    val testCallArgs = List(testNode)
    val testCallAst  = callAst(testCallNode, testCallArgs)

    val whileLoopAst = Ast(whileLoopNode).withChild(testCallAst).withConditionEdge(whileLoopNode, testCallNode)

    // while loop variable assignment:
    val loopVariableAssignmentAsts = loopVariableNames.zipWithIndex.map { case (loopVariableName, idx) =>
      val whileLoopVariableNode = identifierNode(node, loopVariableName)
      val baseNode              = identifierNode(node, resultName)
      val memberNode            = createFieldIdentifierNode("value", line(node), column(node))
      val accessAst             = createFieldAccessCallAst(baseNode, memberNode, line(node), column(node))
      val index                 = idx + 1
      val variableMemberNode =
        createFieldIdentifierNode(s"_$index", line(node), column(node))
      val variableAccessAst =
        createFieldAccessCallAst(accessAst, variableMemberNode, line(node), column(node))
      val loopVariableAssignmentNode = callNode(
        node,
        s"$loopVariableName = $resultName.value._$index",
        Operators.assignment,
        DispatchTypes.STATIC_DISPATCH
      )
      val loopVariableAssignmentArgs = List(Ast(whileLoopVariableNode), variableAccessAst)
      callAst(loopVariableAssignmentNode, loopVariableAssignmentArgs)
    }

    val whileLoopBlockNode = blockNode(node)
    scope.pushNewBlockScope(whileLoopBlockNode)
    localAstParentStack.push(whileLoopBlockNode)

    // while loop block:
    val bodyAst = astForForStmtBody(node)

    val whileLoopBlockChildren = loopVariableAssignmentAsts :+ bodyAst
    setArgumentIndices(whileLoopBlockChildren)
    val whileLoopBlockAst = blockAst(whileLoopBlockNode, whileLoopBlockChildren.toList)

    scope.popScope()
    localAstParentStack.pop()

    // end surrounding block:
    scope.popScope()
    localAstParentStack.pop()

    val blockNodeChildren =
      List(iteratorAssignmentAst, Ast(resultNode)) ++ loopVariableNodes.map(Ast(_)) :+ whileLoopAst.withChild(
        whileLoopBlockAst
      )
    setArgumentIndices(blockNodeChildren)
    blockAst(blockNode_, blockNodeChildren)
  }

  private def astForForStmtSyntax(node: ForStmtSyntax): Ast = {
    node.pattern match
      case binding: ValueBindingPatternSyntax =>
        extractLoopVariableNodeInfo(binding) match {
          case Some(id: IdentifierPatternSyntax)                        => astForForStmtSyntaxWithIdentifier(node, id)
          case Some(expr: ExpressionPatternSyntax)                      => astForForStmtSyntaxWithExpression(node, expr)
          case Some(tuple: TuplePatternSyntax)                          => astForForStmtSyntaxWithTuple(node, tuple)
          case Some(_: WildcardPatternSyntax | _: MissingPatternSyntax) => astForForStmtSyntaxWithWildcard(node)
          case _                                                        => notHandledYet(node)
        }
      case id: IdentifierPatternSyntax                        => astForForStmtSyntaxWithIdentifier(node, id)
      case tuple: TuplePatternSyntax                          => astForForStmtSyntaxWithTuple(node, tuple)
      case expr: ExpressionPatternSyntax                      => astForForStmtSyntaxWithExpression(node, expr)
      case _: WildcardPatternSyntax | _: MissingPatternSyntax => astForForStmtSyntaxWithWildcard(node)
      case _                                                  => notHandledYet(node)
  }

  private def astForGuardStmtSyntax(node: GuardStmtSyntax): Ast = {
    val code         = this.code(node)
    val ifNode       = controlStructureNode(node, ControlStructureTypes.IF, code)
    val conditionAst = astForNode(node.conditions)
    val thenAst      = Ast()
    val elseAst      = astForNode(node.body)
    setOrderExplicitly(elseAst, 3)
    controlStructureAst(ifNode, Option(conditionAst), Seq(thenAst, elseAst))
  }

  private def astForLabeledStmtSyntax(node: LabeledStmtSyntax): Ast = {
    val labeledNode = jumpTargetNode(node, code(node.label), code(node))

    val blockNode_ = blockNode(node)
    scope.pushNewBlockScope(blockNode_)
    localAstParentStack.push(blockNode_)
    val bodyAst = astForNodeWithFunctionReference(node.statement)
    scope.popScope()
    localAstParentStack.pop()

    val labelAsts = List(Ast(labeledNode), bodyAst)
    setArgumentIndices(labelAsts)
    blockAst(blockNode_, labelAsts)
  }

  private def astForMissingStmtSyntax(node: MissingStmtSyntax): Ast = notHandledYet(node)

  private def astForRepeatStmtSyntax(node: RepeatStmtSyntax): Ast = {
    val code = this.code(node)
    // In Swift, a repeat-while loop is semantically the same as a C do-while loop
    val doNode       = controlStructureNode(node, ControlStructureTypes.DO, code)
    val conditionAst = astForNodeWithFunctionReference(node.condition)
    val bodyAst      = astForNode(node.body)
    setOrderExplicitly(conditionAst, 1)
    setOrderExplicitly(bodyAst, 2)
    controlStructureAst(doNode, Option(conditionAst), Seq(bodyAst), placeConditionLast = true)
  }

  private def astForReturnStmtSyntax(node: ReturnStmtSyntax): Ast = {
    val cpgReturn = returnNode(node, code(node))
    node.expression match {
      case Some(value) =>
        val expr = astForNodeWithFunctionReference(value)
        Ast(cpgReturn).withChild(expr).withArgEdge(cpgReturn, expr.root)
      case None =>
        Ast(cpgReturn)
    }
  }

  private def astForThenStmtSyntax(node: ThenStmtSyntax): Ast = notHandledYet(node)

  private def astForThrowStmtSyntax(node: ThrowStmtSyntax): Ast = {
    val op        = "<operator>.throw"
    val callNode_ = callNode(node, code(node), op, op, DispatchTypes.STATIC_DISPATCH)
    val exprAst   = astForNodeWithFunctionReference(node.expression)
    callAst(callNode_, List(exprAst))
  }

  private def astForWhileStmtSyntax(node: WhileStmtSyntax): Ast = {
    val code         = this.code(node)
    val conditionAst = astForNodeWithFunctionReference(node.conditions)
    val bodyAst      = astForNode(node.body)
    setOrderExplicitly(conditionAst, 1)
    setOrderExplicitly(bodyAst, 2)
    whileAst(
      Option(conditionAst),
      Seq(bodyAst),
      code = Option(code),
      lineNumber = line(node),
      columnNumber = column(node)
    )
  }

  private def astForYieldStmtSyntax(node: YieldStmtSyntax): Ast = {
    val cpgReturn = returnNode(node, code(node))
    val expr      = astForNodeWithFunctionReference(node.yieldedExpressions)
    Ast(cpgReturn).withChild(expr).withArgEdge(cpgReturn, expr.root)
  }

  protected def astForStmtSyntax(stmtSyntax: StmtSyntax): Ast = stmtSyntax match {
    case node: BreakStmtSyntax       => astForBreakStmtSyntax(node)
    case node: ContinueStmtSyntax    => astForContinueStmtSyntax(node)
    case node: DeferStmtSyntax       => astForDeferStmtSyntax(node)
    case node: DiscardStmtSyntax     => astForDiscardStmtSyntax(node)
    case node: DoStmtSyntax          => astForDoStmtSyntax(node)
    case node: ExpressionStmtSyntax  => astForExpressionStmtSyntax(node)
    case node: FallThroughStmtSyntax => astForFallThroughStmtSyntax(node)
    case node: ForStmtSyntax         => astForForStmtSyntax(node)
    case node: GuardStmtSyntax       => astForGuardStmtSyntax(node)
    case node: LabeledStmtSyntax     => astForLabeledStmtSyntax(node)
    case node: MissingStmtSyntax     => astForMissingStmtSyntax(node)
    case node: RepeatStmtSyntax      => astForRepeatStmtSyntax(node)
    case node: ReturnStmtSyntax      => astForReturnStmtSyntax(node)
    case node: ThenStmtSyntax        => astForThenStmtSyntax(node)
    case node: ThrowStmtSyntax       => astForThrowStmtSyntax(node)
    case node: WhileStmtSyntax       => astForWhileStmtSyntax(node)
    case node: YieldStmtSyntax       => astForYieldStmtSyntax(node)
  }

}
