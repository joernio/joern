package io.joern.swiftsrc2cpg.astcreation

import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.*
import io.joern.x2cpg
import io.joern.x2cpg.{Ast, ValidationMode}
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.frontendspecific.swiftsrc2cpg.Defines
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.{NewControlStructure, NewJumpLabel}

import scala.annotation.unused

trait AstForStmtSyntaxCreator(implicit withSchemaValidation: ValidationMode) {
  this: AstCreator =>

  private def astForBreakStmtSyntax(node: BreakStmtSyntax): Ast = {
    val labelAst = node.label.fold(Ast())(l => {
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
    })
    Ast(controlStructureNode(node, ControlStructureTypes.BREAK, code(node))).withChild(labelAst)
  }

  private def astForContinueStmtSyntax(node: ContinueStmtSyntax): Ast = {
    val labelAst = node.label.fold(Ast())(l => {
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
    })
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
    Ast(catchNode).withChild(declAst).withChild(bodyAst)
  }

  private def astForExpressionStmtSyntax(node: ExpressionStmtSyntax): Ast = {
    astForNode(node.expression)
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
        val ifNode = controlStructureNode(whereClause.condition, ControlStructureTypes.IF, code(whereClause.condition))
        val testAstRaw = astForNode(whereClause)
        val testAst = testAstRaw.root match {
          case Some(_) => testAstRaw
          case None    => blockAst(blockNode(whereClause), List.empty)
        }
        val thenAst = astForNode(node.body)
        ifThenElseAst(ifNode, Option(testAst), thenAst, None)
      case None => astForNode(node.body)
    }
  }

  private def astForForStmtSyntaxWithWildcard(node: ForStmtSyntax): Ast = {
    val initAsts = Seq(astForNode(node.sequence))
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
    val iteratorName      = scopeLocalUniqueName("iterator")
    val iteratorLocalNode = localNode(node, iteratorName, iteratorName, Defines.Any).order(0)
    val iteratorNode      = identifierNode(node, iteratorName)
    diffGraph.addEdge(localAstParentStack.head, iteratorLocalNode, EdgeTypes.AST)
    scope.addVariableReference(iteratorName, iteratorNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)

    // TODO: add operator to schema
    var op = "<operator>.iterator"
    val iteratorCall =
      createStaticCallNode(node, s"<operator>.iterator($collectionName)", op, op, Defines.Iterator)

    val objectKeysCallArgs = List(astForNode(collection))
    val objectKeysCallAst  = callAst(iteratorCall, objectKeysCallArgs)

    op = Operators.assignment
    val iteratorAssignmentNode =
      createStaticCallNode(node, s"$iteratorName = <operator>.iterator($collectionName)", op, op, Defines.Void)

    val iteratorAssignmentArgs = List(Ast(iteratorNode), objectKeysCallAst)
    val iteratorAssignmentAst  = callAst(iteratorAssignmentNode, iteratorAssignmentArgs)

    // _result:
    val resultName      = scopeLocalUniqueName("result")
    val resultLocalNode = localNode(node, resultName, resultName, Defines.Any).order(0)
    val resultNode      = identifierNode(node, resultName)
    diffGraph.addEdge(localAstParentStack.head, resultLocalNode, EdgeTypes.AST)
    scope.addVariableReference(resultName, resultNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)

    // loop variable:
    val loopVariableName = code(id.identifier)

    val loopVariableLocalNode = localNode(node, loopVariableName, loopVariableName, Defines.Any).order(0)
    val loopVariableNode      = identifierNode(node, loopVariableName)
    diffGraph.addEdge(localAstParentStack.head, loopVariableLocalNode, EdgeTypes.AST)
    scope.addVariableReference(loopVariableName, loopVariableNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)

    // while loop:
    val whileLoopNode = controlStructureNode(node, ControlStructureTypes.WHILE, code(node))

    // while loop test:
    op = Operators.logicalNot
    val testCallNode = createStaticCallNode(node, s"!($resultName = $iteratorName.next()).done", op, op, Defines.Bool)

    op = Operators.assignment
    val doneBaseNode =
      createStaticCallNode(node, s"($resultName = $iteratorName.next())", op, op, Defines.Void)

    val lhsNode = identifierNode(node, resultName)

    val rhsNode = callNode(
      node,
      s"$iteratorName.next()",
      "next",
      x2cpg.Defines.DynamicCallUnknownFullName,
      DispatchTypes.DYNAMIC_DISPATCH,
      None,
      Option(Defines.Any)
    )

    val nextBaseNode = identifierNode(node, iteratorName)

    val nextMemberNode = fieldIdentifierNode(node, "next", "next")

    val nextReceiverNode = createFieldAccessCallAst(node, Ast(nextBaseNode), nextMemberNode)

    val thisNextNode = identifierNode(node, iteratorName)

    val rhsArgs = List(Ast(thisNextNode))
    val rhsAst  = callAst(rhsNode, rhsArgs, receiver = Option(nextReceiverNode))

    val doneBaseArgs = List(Ast(lhsNode), rhsAst)
    val doneBaseAst  = callAst(doneBaseNode, doneBaseArgs)

    val doneMemberNode = fieldIdentifierNode(node, "done", "done")

    val testNode = createFieldAccessCallAst(node, doneBaseAst, doneMemberNode)

    val testCallArgs = List(testNode)
    val testCallAst  = callAst(testCallNode, testCallArgs)

    // while loop variable assignment:
    val whileLoopVariableNode = identifierNode(node, loopVariableName)

    val baseNode = identifierNode(node, resultName)

    val memberNode = fieldIdentifierNode(node, "value", "value")

    val accessAst = createFieldAccessCallAst(node, Ast(baseNode), memberNode)

    op = Operators.assignment
    val loopVariableAssignmentNode =
      createStaticCallNode(node, s"$loopVariableName = $resultName.value", op, op, Defines.Void)

    val loopVariableAssignmentArgs = List(Ast(whileLoopVariableNode), accessAst)
    val loopVariableAssignmentAst  = callAst(loopVariableAssignmentNode, loopVariableAssignmentArgs)

    val whileLoopBlockNode = blockNode(node)
    scope.pushNewBlockScope(whileLoopBlockNode)
    localAstParentStack.push(whileLoopBlockNode)

    // while loop block:
    val bodyAst = astForForStmtBody(node)

    val whileLoopBlockChildren = List(loopVariableAssignmentAst, bodyAst)
    val whileLoopBlockAst      = blockAst(whileLoopBlockNode, whileLoopBlockChildren)

    scope.popScope()
    localAstParentStack.pop()

    // end surrounding block:
    scope.popScope()
    localAstParentStack.pop()

    val whileLoopAst = whileAst(
      Option(testCallAst),
      List(whileLoopBlockAst),
      code = Option(code(node)),
      lineNumber = line(node),
      columnNumber = column(node)
    )

    val blockChildren =
      List(iteratorAssignmentAst, Ast(resultNode), Ast(loopVariableNode), whileLoopAst)
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
    val iteratorName      = scopeLocalUniqueName("iterator")
    val iteratorLocalNode = localNode(node, iteratorName, iteratorName, Defines.Any).order(0)
    val iteratorNode      = identifierNode(node, iteratorName)
    diffGraph.addEdge(localAstParentStack.head, iteratorLocalNode, EdgeTypes.AST)
    scope.addVariableReference(iteratorName, iteratorNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)

    // TODO: add operator to schema
    var op = "<operator>.iterator"
    val iteratorCall =
      createStaticCallNode(node, s"<operator>.iterator($collectionName)", op, op, Defines.Iterator)

    val objectKeysCallArgs = List(astForNode(collection))
    val objectKeysCallAst  = callAst(iteratorCall, objectKeysCallArgs)

    op = Operators.assignment
    val iteratorAssignmentNode =
      createStaticCallNode(node, s"$iteratorName = <operator>.iterator($collectionName)", op, op, Defines.Void)

    val iteratorAssignmentArgs = List(Ast(iteratorNode), objectKeysCallAst)
    val iteratorAssignmentAst  = callAst(iteratorAssignmentNode, iteratorAssignmentArgs)

    // _result:
    val resultName      = scopeLocalUniqueName("result")
    val resultLocalNode = localNode(node, resultName, resultName, Defines.Any).order(0)
    val resultNode      = identifierNode(node, resultName)
    diffGraph.addEdge(localAstParentStack.head, resultLocalNode, EdgeTypes.AST)
    scope.addVariableReference(resultName, resultNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)

    // while loop:
    val whileLoopNode = controlStructureNode(node, ControlStructureTypes.WHILE, code(node))

    // while loop test:
    op = Operators.logicalNot
    val testCallNode = createStaticCallNode(node, s"!($resultName = $iteratorName.next()).done", op, op, Defines.Bool)

    op = Operators.assignment
    val doneBaseNode =
      createStaticCallNode(node, s"($resultName = $iteratorName.next())", op, op, Defines.Void)

    val lhsNode = identifierNode(node, resultName)

    val rhsNode = callNode(
      node,
      s"$iteratorName.next()",
      "next",
      x2cpg.Defines.DynamicCallUnknownFullName,
      DispatchTypes.DYNAMIC_DISPATCH,
      None,
      Option(Defines.Any)
    )

    val nextBaseNode = identifierNode(node, iteratorName)

    val nextMemberNode = fieldIdentifierNode(node, "next", "next")

    val nextReceiverNode = createFieldAccessCallAst(node, Ast(nextBaseNode), nextMemberNode)

    val thisNextNode = identifierNode(node, iteratorName)

    val rhsArgs = List(Ast(thisNextNode))
    val rhsAst  = callAst(rhsNode, rhsArgs, receiver = Option(nextReceiverNode))

    val doneBaseArgs = List(Ast(lhsNode), rhsAst)
    val doneBaseAst  = callAst(doneBaseNode, doneBaseArgs)
    Ast.storeInDiffGraph(doneBaseAst, diffGraph)

    val doneMemberNode = fieldIdentifierNode(node, "done", "done")

    val testNode = createFieldAccessCallAst(node, Ast(doneBaseNode), doneMemberNode)

    val testCallArgs = List(testNode)
    val testCallAst  = callAst(testCallNode, testCallArgs)

    // while loop variable assignment:
    val whileLoopVariableNode = astForNode(id)

    val baseNode = identifierNode(node, resultName)

    val memberNode = fieldIdentifierNode(node, "value", "value")

    val accessAst = createFieldAccessCallAst(node, Ast(baseNode), memberNode)

    op = Operators.assignment
    val loopVariableAssignmentNode =
      createStaticCallNode(node, s"${code(id.expression)} = $resultName.value", op, op, Defines.Void)

    val loopVariableAssignmentArgs = List(whileLoopVariableNode, accessAst)
    val loopVariableAssignmentAst  = callAst(loopVariableAssignmentNode, loopVariableAssignmentArgs)

    val whileLoopBlockNode = blockNode(node)
    scope.pushNewBlockScope(whileLoopBlockNode)
    localAstParentStack.push(whileLoopBlockNode)

    // while loop block:
    val bodyAst = astForForStmtBody(node)

    val whileLoopBlockChildren = List(loopVariableAssignmentAst, bodyAst)
    val whileLoopBlockAst      = blockAst(whileLoopBlockNode, whileLoopBlockChildren)

    scope.popScope()
    localAstParentStack.pop()

    // end surrounding block:
    scope.popScope()
    localAstParentStack.pop()

    val whileLoopAst = whileAst(
      Option(testCallAst),
      List(whileLoopBlockAst),
      code = Option(code(node)),
      lineNumber = line(node),
      columnNumber = column(node)
    )

    val blockChildren = List(iteratorAssignmentAst, Ast(resultNode), whileLoopAst)
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
    val iteratorName      = scopeLocalUniqueName("iterator")
    val iteratorLocalNode = localNode(node, iteratorName, iteratorName, Defines.Any).order(0)
    val iteratorNode      = identifierNode(node, iteratorName)
    diffGraph.addEdge(localAstParentStack.head, iteratorLocalNode, EdgeTypes.AST)
    scope.addVariableReference(iteratorName, iteratorNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)

    // TODO: add operator to schema
    var op = "<operator>.iterator"
    val iteratorCall =
      createStaticCallNode(node, s"<operator>.iterator($collectionName)", op, op, Defines.Iterator)

    val objectKeysCallArgs = List(astForNode(collection))
    val objectKeysCallAst  = callAst(iteratorCall, objectKeysCallArgs)

    op = Operators.assignment
    val iteratorAssignmentNode =
      createStaticCallNode(node, s"$iteratorName = <operator>.iterator($collectionName)", op, op, Defines.Void)

    val iteratorAssignmentArgs = List(Ast(iteratorNode), objectKeysCallAst)
    val iteratorAssignmentAst  = callAst(iteratorAssignmentNode, iteratorAssignmentArgs)

    // _result:
    val resultName      = scopeLocalUniqueName("result")
    val resultLocalNode = localNode(node, resultName, resultName, Defines.Any).order(0)
    val resultNode      = identifierNode(node, resultName)
    diffGraph.addEdge(localAstParentStack.head, resultLocalNode, EdgeTypes.AST)
    scope.addVariableReference(resultName, resultNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)

    // loop variable:
    val loopVariableNames = id.elements.children.map(c => code(c.pattern))

    val loopVariableLocalNodes = loopVariableNames.map(varName => localNode(node, varName, varName, Defines.Any))
    val loopVariableNodes      = loopVariableNames.map(identifierNode(node, _))
    loopVariableLocalNodes.foreach(diffGraph.addEdge(localAstParentStack.head, _, EdgeTypes.AST))
    loopVariableNames.zip(loopVariableNodes).foreach { case (loopVariableName, loopVariableNode) =>
      scope.addVariableReference(loopVariableName, loopVariableNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)
    }

    // while loop:
    val whileLoopNode = controlStructureNode(node, ControlStructureTypes.WHILE, code(node))

    // while loop test:
    op = Operators.logicalNot
    val testCallNode =
      createStaticCallNode(node, s"!($resultName = $iteratorName.next()).done", op, op, Defines.Bool)

    op = Operators.assignment
    val doneBaseNode =
      createStaticCallNode(node, s"($resultName = $iteratorName.next())", op, op, Defines.Void)

    val lhsNode = identifierNode(node, resultName)

    val rhsNode = callNode(
      node,
      s"$iteratorName.next()",
      "next",
      x2cpg.Defines.DynamicCallUnknownFullName,
      DispatchTypes.DYNAMIC_DISPATCH,
      None,
      Option(Defines.Any)
    )

    val nextBaseNode = identifierNode(node, iteratorName)

    val nextMemberNode = fieldIdentifierNode(node, "next", "next")

    val nextReceiverNode = createFieldAccessCallAst(node, Ast(nextBaseNode), nextMemberNode)

    val thisNextNode = identifierNode(node, iteratorName)

    val rhsArgs = List(Ast(thisNextNode))
    val rhsAst  = callAst(rhsNode, rhsArgs, receiver = Option(nextReceiverNode))

    val doneBaseArgs = List(Ast(lhsNode), rhsAst)
    val doneBaseAst  = callAst(doneBaseNode, doneBaseArgs)

    val doneMemberNode = fieldIdentifierNode(node, "done", "done")

    val testNode = createFieldAccessCallAst(node, doneBaseAst, doneMemberNode)

    val testCallArgs = List(testNode)
    val testCallAst  = callAst(testCallNode, testCallArgs)

    // while loop variable assignment:
    val loopVariableAssignmentAsts = loopVariableNames.zipWithIndex.map { case (loopVariableName, idx) =>
      val whileLoopVariableNode = identifierNode(node, loopVariableName)
      val baseNode              = identifierNode(node, resultName)
      val memberNode            = fieldIdentifierNode(node, "value", "value")
      val accessAst             = createFieldAccessCallAst(node, Ast(baseNode), memberNode)
      val index                 = idx + 1
      val variableMemberNode    = fieldIdentifierNode(node, s"_$index", s"_$index")
      val variableAccessAst     = createFieldAccessCallAst(node, accessAst, variableMemberNode)

      val op = Operators.assignment
      val loopVariableAssignmentNode =
        createStaticCallNode(node, s"$loopVariableName = $resultName.value._$index", op, op, Defines.Void)

      val loopVariableAssignmentArgs = List(Ast(whileLoopVariableNode), variableAccessAst)
      callAst(loopVariableAssignmentNode, loopVariableAssignmentArgs)
    }

    val whileLoopBlockNode = blockNode(node)
    scope.pushNewBlockScope(whileLoopBlockNode)
    localAstParentStack.push(whileLoopBlockNode)

    // while loop block:
    val bodyAst = astForForStmtBody(node)

    val whileLoopBlockChildren = loopVariableAssignmentAsts :+ bodyAst
    val whileLoopBlockAst      = blockAst(whileLoopBlockNode, whileLoopBlockChildren.toList)

    scope.popScope()
    localAstParentStack.pop()

    // end surrounding block:
    scope.popScope()
    localAstParentStack.pop()

    val whileLoopAst = whileAst(
      Option(testCallAst),
      List(whileLoopBlockAst),
      code = Option(code(node)),
      lineNumber = line(node),
      columnNumber = column(node)
    )

    val blockNodeChildren =
      List(iteratorAssignmentAst, Ast(resultNode)) ++ loopVariableNodes.map(Ast(_)) :+ whileLoopAst
    blockAst(blockNode_, blockNodeChildren)
  }

  private def astForForStmtSyntax(node: ForStmtSyntax): Ast = {
    node.pattern match {
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
  }

  private def astForGuardStmtSyntax(node: GuardStmtSyntax): Ast = {
    // This is already handled in AstCreatorHelper.astsForBlockElements
    Ast()
  }

  /** Handles Swift optional binding (guard-let) constructs.
    *
    * De-sugars `guard let x = foo() else { exit }` into:
    *
    * Condition: { <tmp>0 = foo(); <tmp>0 != nil }
    *
    * Then block: { let x = <tmp>0 }
    *
    * Else block: { exit }
    *
    * For multiple bindings `guard let a = foo(), let b = bar() else { exit }`:
    *
    * Condition: { <tmp>0 = foo(); <tmp>1 = bar(); <tmp>0 != nil && <tmp>1 != nil }
    *
    * Then block: { a = <tmp>0; b = <tmp>1 }
    *
    * For mixed cases with/without initializers `guard let a = foo(), let b else { exit }`:
    *
    * Condition: { <tmp>0 = foo(); <tmp>0 != nil && b != nil }
    *
    * Then block: { a = <tmp>0 }
    */
  private def astForGuardLetStmtSyntax(
    node: GuardStmtSyntax,
    ifNode: NewControlStructure,
    optionalBindings: Seq[OptionalBindingConditionSyntax]
  ): Ast = {
    val bindingInfos = collectBindingInfos(optionalBindings)
    val conditionAst = buildOptionalBindingCondition(node, bindingInfos)

    // For guard, the then block contains the unwrapped bindings in the parent scope
    val thenBlockNode = blockNode(node)
    scope.pushNewBlockScope(thenBlockNode)
    localAstParentStack.push(thenBlockNode)
    val unwrapAsts = buildUnwrapAssignments(bindingInfos)
    scope.popScope()
    localAstParentStack.pop()
    val thenAst = blockAst(thenBlockNode, unwrapAsts)

    val elseAst = astForNode(node.body)

    ifThenElseAst(ifNode, Option(conditionAst), thenAst, Option(elseAst))
  }

  /** Handles mixed optional binding constructs with both simple and tuple patterns.
    *
    * De-sugars `guard let a = foo(), let (b, c) = bar() else { exit }` into:
    *
    * Condition: { <tmp>0 = foo(); <tmp>0 != nil }
    *
    * Then block: { let a = <tmp>0; let (b, c) = bar() }
    */
  private def astForGuardLetStmtSyntaxMixed(
    node: GuardStmtSyntax,
    ifNode: NewControlStructure,
    simpleBindings: Seq[OptionalBindingConditionSyntax],
    tupleBindings: Seq[OptionalBindingConditionSyntax]
  ): Ast = {
    val bindingInfos = collectBindingInfos(simpleBindings)
    val conditionAst = buildOptionalBindingCondition(node, bindingInfos)

    val thenBlockNode = blockNode(node)
    scope.pushNewBlockScope(thenBlockNode)
    localAstParentStack.push(thenBlockNode)
    val unwrapAsts = buildUnwrapAssignments(bindingInfos) ++ tupleBindings.map(astForNode)
    scope.popScope()
    localAstParentStack.pop()
    val thenAst = blockAst(thenBlockNode, unwrapAsts)

    val elseAst = astForNode(node.body)

    ifThenElseAst(ifNode, Option(conditionAst), thenAst, Option(elseAst))
  }

  /** Handles partial optional binding desugaring with other conditions.
    *
    * De-sugars `guard let a = foo(), someCondition else { exit }` into:
    *
    * Condition: { <tmp>0 = foo(); <tmp>0 != nil && someCondition }
    *
    * Then block: { let a = <tmp>0 }
    */
  private def astForGuardLetStmtSyntaxPartial(
    node: GuardStmtSyntax,
    ifNode: NewControlStructure,
    simpleBindings: Seq[OptionalBindingConditionSyntax],
    tupleBindings: Seq[OptionalBindingConditionSyntax],
    otherConditions: Seq[ConditionElementSyntax]
  ): Ast = {
    val bindingInfos = collectBindingInfos(simpleBindings)
    val conditionAst = buildOptionalBindingCondition(node, bindingInfos, otherConditions)

    val thenBlockNode = blockNode(node)
    scope.pushNewBlockScope(thenBlockNode)
    localAstParentStack.push(thenBlockNode)
    val unwrapAsts = buildUnwrapAssignments(bindingInfos) ++ tupleBindings.map(astForNode)
    scope.popScope()
    localAstParentStack.pop()
    val thenAst = blockAst(thenBlockNode, unwrapAsts)

    val elseAst = astForNode(node.body)

    ifThenElseAst(ifNode, Option(conditionAst), thenAst, Option(elseAst))
  }

  private def astForLabeledStmtSyntax(node: LabeledStmtSyntax): Ast = {
    val labeledNode = jumpTargetNode(node, code(node.label), code(node))
    val blockNode_  = blockNode(node)
    scope.pushNewBlockScope(blockNode_)
    localAstParentStack.push(blockNode_)
    val bodyAst = astForNode(node.statement)
    scope.popScope()
    localAstParentStack.pop()
    blockAst(blockNode_, List(Ast(labeledNode), bodyAst))
  }

  private def astForMissingStmtSyntax(@unused node: MissingStmtSyntax): Ast = Ast()

  private def astForRepeatStmtSyntax(node: RepeatStmtSyntax): Ast = {
    val code = this.code(node)
    // In Swift, a repeat-while loop is semantically the same as a C do-while loop
    val doNode          = controlStructureNode(node, ControlStructureTypes.DO, code)
    val conditionAst    = astForNode(node.condition)
    val bodyAst         = astForNode(node.body)
    val astWithChildren = controlStructureAst(doNode, Option(conditionAst), Seq(bodyAst), placeConditionLast = true)
    bodyAst.root match {
      case Some(bodyRoot) => astWithChildren.withDoBodyEdge(doNode, bodyRoot)
      case None           => astWithChildren
    }
  }

  private def astForReturnStmtSyntax(node: ReturnStmtSyntax): Ast = {
    val cpgReturn = returnNode(node, code(node))
    node.expression match {
      case Some(value) =>
        val expr = astForNode(value)
        val ast  = Ast(cpgReturn).withChild(expr)
        expr.root match {
          case Some(value) => ast.withArgEdge(cpgReturn, value)
          case None        => ast
        }
      case None =>
        Ast(cpgReturn)
    }
  }

  private def astForThenStmtSyntax(node: ThenStmtSyntax): Ast = notHandledYet(node)

  private def astForThrowStmtSyntax(node: ThrowStmtSyntax): Ast = {
    val op  = "<operator>.throw"
    val tpe = fullnameProvider.typeFullname(node).getOrElse(Defines.Any)
    registerType(tpe)
    val callNode_ = createStaticCallNode(node, code(node), op, op, tpe)
    val exprAst   = astForNode(node.expression)
    callAst(callNode_, List(exprAst))
  }

  private def astForWhileStmtSyntax(node: WhileStmtSyntax): Ast = {
    val code = this.code(node)

    handleOptionalBindingConditions(
      node.conditions.children,
      onAllSimple = simpleBindings => astForWhileLetStmtSyntax(node, simpleBindings),
      onMixed = (simpleBindings, tupleBindings) => astForWhileLetStmtSyntaxMixed(node, simpleBindings, tupleBindings),
      onPartial = (simpleBindings, tupleBindings, otherConditions) =>
        astForWhileLetStmtSyntaxPartial(node, simpleBindings, tupleBindings, otherConditions),
      onStandard = () => {
        val conditionAst = astForNode(node.conditions)
        val bodyAst      = astForNode(node.body)
        whileAst(
          Option(conditionAst),
          Seq(bodyAst),
          code = Option(code),
          lineNumber = line(node),
          columnNumber = column(node)
        )
      }
    )
  }

  /** Handles Swift optional binding (while-let) constructs.
    *
    * De-sugars `while let item = iterator.next() { body }` into:
    *
    * Condition: { <tmp>0 = iterator.next(); <tmp>0 != nil }
    *
    * Loop body: { let item = <tmp>0; body }
    *
    * For multiple bindings `while let a = foo(), let b = bar() { body }`:
    *
    * Condition: { <tmp>0 = foo(); <tmp>1 = bar(); <tmp>0 != nil && <tmp>1 != nil }
    *
    * Loop body: { a = <tmp>0; b = <tmp>1; body }
    *
    * For mixed cases with/without initializers `while let a = foo(), let b { body }`:
    *
    * Condition: { <tmp>0 = foo(); <tmp>0 != nil && b != nil }
    *
    * Loop body: { a = <tmp>0; body }
    */
  private def astForWhileLetStmtSyntax(
    node: WhileStmtSyntax,
    optionalBindings: Seq[OptionalBindingConditionSyntax]
  ): Ast = {
    val bindingInfos = collectBindingInfos(optionalBindings)
    val conditionAst = buildOptionalBindingCondition(node, bindingInfos)
    val bodyAst      = buildBodyWithUnwrapping(node.body, node.body.statements.children, bindingInfos)

    whileAst(
      Option(conditionAst),
      Seq(bodyAst),
      code = Option(code(node)),
      lineNumber = line(node),
      columnNumber = column(node)
    )
  }

  /** Handles mixed optional binding constructs with both simple and tuple patterns.
    *
    * De-sugars `while let a = foo(), let (b, c) = bar() { body }` into:
    *
    * Condition: { <tmp>0 = foo(); <tmp>0 != nil }
    *
    * Loop body: { let a = <tmp>0; let (b, c) = bar(); body }
    */
  private def astForWhileLetStmtSyntaxMixed(
    node: WhileStmtSyntax,
    simpleBindings: Seq[OptionalBindingConditionSyntax],
    tupleBindings: Seq[OptionalBindingConditionSyntax]
  ): Ast = {
    val bindingInfos = collectBindingInfos(simpleBindings)
    val conditionAst = buildOptionalBindingCondition(node, bindingInfos)
    val bodyAst      = buildBodyWithUnwrapping(node.body, tupleBindings ++ node.body.statements.children, bindingInfos)

    whileAst(
      Option(conditionAst),
      Seq(bodyAst),
      code = Option(code(node)),
      lineNumber = line(node),
      columnNumber = column(node)
    )
  }

  /** Handles partial optional binding desugaring with other conditions.
    *
    * De-sugars `while let a = foo(), someCondition { body }` into:
    *
    * Condition: { <tmp>0 = foo(); <tmp>0 != nil && someCondition }
    *
    * Loop body: { let a = <tmp>0; body }
    */
  private def astForWhileLetStmtSyntaxPartial(
    node: WhileStmtSyntax,
    simpleBindings: Seq[OptionalBindingConditionSyntax],
    tupleBindings: Seq[OptionalBindingConditionSyntax],
    otherConditions: Seq[ConditionElementSyntax]
  ): Ast = {
    val bindingInfos = collectBindingInfos(simpleBindings)
    val conditionAst = buildOptionalBindingCondition(node, bindingInfos, otherConditions)
    val bodyAst      = buildBodyWithUnwrapping(node.body, tupleBindings ++ node.body.statements.children, bindingInfos)

    whileAst(
      Option(conditionAst),
      Seq(bodyAst),
      code = Option(code(node)),
      lineNumber = line(node),
      columnNumber = column(node)
    )
  }

  private def astForYieldStmtSyntax(node: YieldStmtSyntax): Ast = {
    val cpgReturn = returnNode(node, code(node))
    val expr      = astForNode(node.yieldedExpressions)
    val ast       = Ast(cpgReturn).withChild(expr)
    expr.root match {
      case Some(value) => ast.withArgEdge(cpgReturn, value)
      case None        => ast
    }
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
