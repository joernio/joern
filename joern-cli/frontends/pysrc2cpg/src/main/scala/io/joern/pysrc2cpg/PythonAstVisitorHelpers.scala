package io.joern.pysrc2cpg

import io.joern.pysrc2cpg.memop.MemoryOperation
import io.joern.pysrc2cpg.memop.MemoryOperation.{Del, Load, Store}
import io.joern.pythonparser.{AstPrinter, ast}
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.frontendspecific.pysrc2cpg.PythonOperators
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.*

import scala.collection.immutable.::
import scala.collection.immutable.Nil
import scala.collection.mutable

trait PythonAstVisitorHelpers(implicit withSchemaValidation: ValidationMode) { this: PythonAstVisitor =>

  protected def codeOf(node: NewNode): String = {
    node.asInstanceOf[AstNodeNew].code
  }

  protected def lineAndColOf(node: ast.iattributes): LineAndColumn = {
    // node.end_col_offset - 1 because the end column offset of the parser points
    // behind the last symbol.
    LineAndColumn(
      node.lineno,
      node.col_offset,
      node.end_lineno,
      node.end_col_offset - 1,
      node.input_offset,
      node.end_input_offset
    )
  }

  private var tmpCounter = 0

  protected def getUnusedName(prefix: String = null): String = {
    // TODO check that result name does not collide with existing variables.
    val result = if (prefix != null) {
      s"${prefix}_tmp$tmpCounter"
    } else {
      s"tmp$tmpCounter"
    }
    tmpCounter += 1
    result
  }

  protected def createTry(
    body: Iterable[NewNode],
    handlers: Iterable[NewNode],
    finalBlock: Iterable[NewNode],
    orElseBlock: Iterable[NewNode],
    lineAndColumn: LineAndColumn
  ): NewNode = {
    val controlStructureNode = nodeBuilder.controlStructureNode("try: ...", ControlStructureTypes.TRY, lineAndColumn)

    val bodyBlockNode      = createBlock(body, lineAndColumn).asInstanceOf[NewBlock]
    val handlersBlockNodes = handlers.map(x => createBlock(Iterable(x), lineAndColumn).asInstanceOf[NewBlock]).toSeq

    val orElseBlockSeq = orElseBlock.toSeq
    val finalBlockSeq  = finalBlock.toSeq

    val elseBlockNodes = if (orElseBlockSeq.nonEmpty) {
      val orElseBlockNode = createBlock(orElseBlockSeq, lineAndColumn).asInstanceOf[NewBlock]
      Seq(orElseBlockNode)
    } else { Seq.empty }

    val finallyBlockNodes = if (finalBlockSeq.nonEmpty) {
      val finalBlockNode = createBlock(finalBlockSeq, lineAndColumn).asInstanceOf[NewBlock]
      Seq(finalBlockNode)
    } else { Seq.empty }

    val handlersAsts = handlersBlockNodes.map { handlerNode =>
      val controlStructureNode =
        nodeBuilder.controlStructureNode(codeOf(handlerNode), ControlStructureTypes.CATCH, lineAndColumn)
      addAstChildNodes(controlStructureNode, 1, Seq(handlerNode))
      controlStructureNode
    }

    val finallyAsts = finallyBlockNodes.map { finallyNode =>
      val controlStructureNode =
        nodeBuilder.controlStructureNode(codeOf(finallyNode), ControlStructureTypes.FINALLY, lineAndColumn)
      addAstChildNodes(controlStructureNode, 1, Seq(finallyNode))
      controlStructureNode
    }

    val elseAsts = elseBlockNodes.map { elseNode =>
      val controlStructureNode =
        nodeBuilder.controlStructureNode(codeOf(elseNode), ControlStructureTypes.ELSE, lineAndColumn)
      addAstChildNodes(controlStructureNode, 1, Seq(elseNode))
      controlStructureNode
    }

    addAstChildNodes(controlStructureNode, 1, Seq(bodyBlockNode) ++ handlersAsts ++ elseAsts ++ finallyAsts)
    controlStructureNode
  }

  protected def createTransformedImport(
    from: String,
    names: Iterable[ast.Alias],
    lineAndCol: LineAndColumn
  ): NewNode = {
    val importAssignNodes =
      names.map { alias =>
        val importedAsIdentifierName = alias.asName.getOrElse(alias.name)

        val nameParts = importedAsIdentifierName.split('.')

        val importAssignLhsIdentifierNode =
          createIdentifierNode(nameParts(0), Store, lineAndCol)

        val arguments = Seq(
          nodeBuilder.stringLiteralNode(from, lineAndCol),
          nodeBuilder.stringLiteralNode(alias.name, lineAndCol)
        ) ++ (alias.asName match {
          case Some(aliasName) => Seq(nodeBuilder.stringLiteralNode(aliasName, lineAndCol))
          case None            => Seq()
        })

        val importCallNode =
          createCall(createIdentifierNode("import", Load, lineAndCol), "import", lineAndCol, arguments, Nil, None)

        val assignNode = createAssignment(importAssignLhsIdentifierNode, importCallNode, lineAndCol)
        assignNode
      }

    if (importAssignNodes.size > 1) {
      createBlock(importAssignNodes, lineAndCol)
    } else {
      // Empty importAssignNodes cannot happen.
      importAssignNodes.head
    }
  }

  // Used for assign statements, for loop target assignment and
  // for comprehension target assignment.
  protected def createValueToTargetsDecomposition(
    targets: Iterable[ast.iexpr],
    valueNode: NewNode,
    lineAndColumn: LineAndColumn
  ): Iterable[NewNode] = {
    if (
      targets.size == 1 &&
      !targets.head.isInstanceOf[ast.Tuple] &&
      !targets.head.isInstanceOf[ast.List]
    ) {
      // No lowering or wrapping in a block is required if we have a single target and
      // no decomposition.
      val targetNode = convert(targets.head)

      Iterable.single(createAssignment(targetNode, valueNode, lineAndColumn))
    } else {
      // Lowering of x, (y,z) = a = b = c:
      // Note: No surrounding block is created. This is the duty of the caller.
      //     tmp = c
      //     x = tmp[0]
      //     y = tmp[1][0]
      //     z = tmp[1][1]
      //     a = c
      //     b = c
      // Lowering of for x, (y, z) in c:
      //     tmp = c
      //     x = tmp[0]
      //     y = tmp[1][0]
      //     z = tmp[1][1]
      // Lowering of x, *y, z = someList:
      //     tmp = someList
      //     x = tmp[0]
      //     y = slice(tmp, 1, -1, 1)
      //     z = tmp[-1]
      val tmpVariableName = getUnusedName()

      val tmpVariableAssignNode =
        createAssignmentToIdentifier(tmpVariableName, valueNode, lineAndColumn)

      val loweredAssignNodes = mutable.ArrayBuffer.empty[NewNode]
      loweredAssignNodes.append(tmpVariableAssignNode)

      targets.foreach { target =>
        val targetWithAccessChains = getTargetsWithAccessChains(target)
        targetWithAccessChains.foreach { case (trgt, accessChain, starredInfoOpt) =>
          val targetNode        = convert(trgt)
          val tmpIdentifierNode = createIdentifierNode(tmpVariableName, Load, lineAndColumn)

          val sourceNode = starredInfoOpt match {
            case Some(starredInfo) =>
              val baseNode = if (accessChain.tail.nonEmpty) {
                createIndexAccessChain(tmpIdentifierNode, accessChain.tail, lineAndColumn)
              } else {
                tmpIdentifierNode
              }
              val upperIndex = if (starredInfo.countAfter == 0) None else Some(-starredInfo.countAfter)
              createSliceCall(baseNode, starredInfo.position, upperIndex, lineAndColumn)

            case None =>
              createIndexAccessChain(tmpIdentifierNode, accessChain, lineAndColumn)
          }

          val targetAssignNode = createAssignment(targetNode, sourceNode, lineAndColumn)
          loweredAssignNodes.append(targetAssignNode)
        }
      }
      loweredAssignNodes
    }
  }

  protected case class StarredInfo(position: Int, totalCount: Int, countAfter: Int)

  protected def getTargetsWithAccessChains(target: ast.iexpr): Iterable[(ast.iexpr, List[Int], Option[StarredInfo])] = {
    val result = mutable.ArrayBuffer.empty[(ast.iexpr, List[Int], Option[StarredInfo])]
    getTargetsInternal(target, Nil)

    def getTargetsInternal(target: ast.iexpr, indexChain: List[Int]): Unit = {
      target match {
        case tuple: ast.Tuple =>
          handleSequence(tuple.elts.toSeq, indexChain)
        case list: ast.List =>
          handleSequence(list.elts.toSeq, indexChain)
        case _ =>
          result.append((target, indexChain, None))
      }
    }

    def handleSequence(elements: Seq[ast.iexpr], indexChain: List[Int]): Unit = {
      val starredIndex = elements.indexWhere(_.isInstanceOf[ast.Starred])

      if (starredIndex >= 0) {
        elements.take(starredIndex).zipWithIndex.foreach { case (element, index) =>
          getTargetsInternal(element, index :: indexChain)
        }

        val starred     = elements(starredIndex).asInstanceOf[ast.Starred]
        val countAfter  = elements.size - starredIndex - 1
        val starredInfo = StarredInfo(starredIndex, elements.size, countAfter)
        result.append((starred.value, starredIndex :: indexChain, Some(starredInfo)))

        elements.drop(starredIndex + 1).zipWithIndex.foreach { case (element, idx) =>
          getTargetsInternal(element, -(countAfter - idx) :: indexChain)
        }
      } else {
        elements.zipWithIndex.foreach { case (element, index) =>
          getTargetsInternal(element, index :: indexChain)
        }
      }
    }

    result
  }

  protected def createComprehensionLowering(
    tmpVariableName: String,
    containerInitAssignNode: NewNode,
    innerMostLoopNode: NewNode,
    comprehensions: Iterable[ast.Comprehension],
    lineAndColumn: LineAndColumn
  ): NewNode = {
    val specialTargetLocals = mutable.ArrayBuffer.empty[NewLocal]

    // Innermost generator is transformed first and becomes the body of the
    // generator one layer up. The body of the innermost generator is the
    // list comprehensions element expression wrapped in an tmp.append() call.
    val nestedLoopBlockNode =
      comprehensions.foldRight(innerMostLoopNode) { case (comprehension, loopBodyNode) =>
        extractComprehensionSpecialVariableNames(comprehension.target).foreach { name =>
          // For the target names we need to create special scoped variables.
          val localNode = nodeBuilder.localNode(name.id, None)
          specialTargetLocals.append(localNode)
          contextStack.addSpecialVariable(localNode)
        }
        createForLowering(
          comprehension.target,
          comprehension.iter,
          comprehension.ifs,
          Iterable.single(loopBodyNode),
          Iterable.empty,
          comprehension.is_async,
          lineAndColumn
        )
      }

    val returnIdentifierNode = createIdentifierNode(tmpVariableName, Load, lineAndColumn)

    val blockNode =
      createBlock(containerInitAssignNode :: nestedLoopBlockNode :: returnIdentifierNode :: Nil, lineAndColumn)

    addAstChildNodes(blockNode, 1, specialTargetLocals)

    blockNode
  }

  // Extracts plain names, starred names and name or starred name elements from tuples and lists.
  private def extractComprehensionSpecialVariableNames(target: ast.iexpr): Iterable[ast.Name] = {
    target match {
      case name: ast.Name =>
        name :: Nil
      case starred: ast.Starred =>
        extractComprehensionSpecialVariableNames(starred.value)
      case tuple: ast.Tuple =>
        tuple.elts.flatMap(extractComprehensionSpecialVariableNames)
      case list: ast.List =>
        list.elts.flatMap(extractComprehensionSpecialVariableNames)
      case _ =>
        Nil
    }
  }

  protected def createBlock(blockElements: Iterable[NewNode], lineAndColumn: LineAndColumn): NewNode = {
    val blockCode = blockElements.map(codeOf).mkString("\n")
    val blockNode = nodeBuilder.blockNode(blockCode, lineAndColumn)

    val orderIndex = new AutoIncIndex(1)
    addAstChildNodes(blockNode, orderIndex, blockElements)

    blockNode
  }

  private def codeForCallNode(
    receiverNode: NewNode,
    argumentNodes: Iterable[NewNode],
    keywordArguments: Iterable[(String, NewNode)],
    callAstNode: Option[ast.iast]
  ): String = {
    callAstNode
      .map(new AstPrinter("").print)
      .getOrElse(
        codeOf(receiverNode) +
          "(" +
          argumentNodes.map(codeOf).mkString(", ") +
          (if (argumentNodes.nonEmpty && keywordArguments.nonEmpty) ", " else "") +
          keywordArguments
            .map { case (keyword: String, argNode) => keyword + " = " + codeOf(argNode) }
            .mkString(", ") +
          ")"
      )
  }

  protected def createCall(
    receiverNode: NewNode,
    name: String,
    lineAndColumn: LineAndColumn,
    argumentNodes: Iterable[NewNode],
    keywordArguments: Iterable[(String, NewNode)],
    callAstNode: Option[ast.iast]
  ): NewCall = {
    val code     = codeForCallNode(receiverNode, argumentNodes, keywordArguments, callAstNode)
    val callNode = nodeBuilder.callNode(code, name, DispatchTypes.DYNAMIC_DISPATCH, lineAndColumn)

    edgeBuilder.astEdge(receiverNode, callNode, 0)
    edgeBuilder.receiverEdge(receiverNode, callNode)

    var index = 1
    argumentNodes.foreach { argumentNode =>
      edgeBuilder.astEdge(argumentNode, callNode, order = index)
      edgeBuilder.argumentEdge(argumentNode, callNode, argIndex = index)
      index += 1
    }

    keywordArguments.foreach { case (keyword: String, argumentNode) =>
      edgeBuilder.astEdge(argumentNode, callNode, order = index)
      edgeBuilder.argumentEdge(argumentNode, callNode, argName = keyword)
      index += 1
    }

    callNode
  }

  protected def createInstanceCall(
    receiverNode: NewNode,
    instanceNode: NewNode,
    name: String,
    lineAndColumn: LineAndColumn,
    argumentNodes: Iterable[NewNode],
    keywordArguments: Iterable[(String, NewNode)],
    callAstNode: Option[ast.iast]
  ): NewCall = {
    val code     = codeForCallNode(receiverNode, argumentNodes, keywordArguments, callAstNode)
    val callNode = nodeBuilder.callNode(code, name, DispatchTypes.DYNAMIC_DISPATCH, lineAndColumn)

    edgeBuilder.astEdge(receiverNode, callNode, 0)
    edgeBuilder.receiverEdge(receiverNode, callNode)
    edgeBuilder.astEdge(instanceNode, callNode, 1)
    edgeBuilder.argumentEdge(instanceNode, callNode, 0)

    var argIndex = 1
    argumentNodes.foreach { argumentNode =>
      edgeBuilder.astEdge(argumentNode, callNode, argIndex + 1)
      edgeBuilder.argumentEdge(argumentNode, callNode, argIndex)
      argIndex += 1
    }

    keywordArguments.foreach { case (keyword: String, argumentNode) =>
      edgeBuilder.astEdge(argumentNode, callNode, order = argIndex + 1)
      edgeBuilder.argumentEdge(argumentNode, callNode, argName = keyword)
      argIndex += 1
    }

    callNode
  }

  // NOTE if xMayHaveSideEffects == false, function x must return a distinct
  // tree/node for each invocation!!!
  // Otherwise the same tree/node may get placed in different places of the AST
  // which is invalid and in this concrete case here triggered setting the
  // argumentIndex twice once for x being receiver and once for x being the
  // instance.
  // If x may have side effects we lower as follows: x.y(<args>) =>
  // {
  //   tmp = x
  //   CALL(recv = tmp.y, inst = tmp, args=<args>)
  // }
  protected def createXDotYCall(
    x: () => NewNode,
    y: String,
    xMayHaveSideEffects: Boolean,
    lineAndColumn: LineAndColumn,
    argumentNodes: Iterable[NewNode],
    keywordArguments: Iterable[(String, NewNode)],
    callAstNode: Option[ast.iast]
  ): NewNode = {
    if (xMayHaveSideEffects) {
      val tmpVarName    = getUnusedName()
      val tmpAssignCall = createAssignmentToIdentifier(tmpVarName, x(), lineAndColumn)
      val receiverNode =
        createFieldAccess(createIdentifierNode(tmpVarName, Load, lineAndColumn), y, lineAndColumn)
      val instanceNode = createIdentifierNode(tmpVarName, Load, lineAndColumn)
      val instanceCallNode =
        createInstanceCall(receiverNode, instanceNode, y, lineAndColumn, argumentNodes, keywordArguments, callAstNode)
      createBlock(tmpAssignCall :: instanceCallNode :: Nil, lineAndColumn)
    } else {
      val receiverNode = createFieldAccess(x(), y, lineAndColumn)
      createInstanceCall(receiverNode, x(), y, lineAndColumn, argumentNodes, keywordArguments, callAstNode)
    }
  }

  // NOTE: The argument indicies start from 0!
  protected def createStaticCall(
    name: String,
    methodFullName: String,
    lineAndColumn: LineAndColumn,
    argumentNodes: Iterable[NewNode],
    keywordArguments: Iterable[(String, NewNode)]
  ): NewNode = {
    val code = name +
      "(" +
      argumentNodes.map(codeOf).mkString(", ") +
      (if (argumentNodes.nonEmpty && keywordArguments.nonEmpty) ", " else "") +
      keywordArguments
        .map { case (keyword: String, argNode) => keyword + " = " + codeOf(argNode) }
        .mkString(", ") +
      ")"
    val callNode = nodeBuilder.callNode(code, methodFullName, DispatchTypes.STATIC_DISPATCH, lineAndColumn)

    var argIndex = 0
    argumentNodes.foreach { argumentNode =>
      edgeBuilder.astEdge(argumentNode, callNode, argIndex)
      edgeBuilder.argumentEdge(argumentNode, callNode, argIndex)
      argIndex += 1
    }

    keywordArguments.foreach { case (keyword: String, argumentNode) =>
      edgeBuilder.astEdge(argumentNode, callNode, order = argIndex)
      edgeBuilder.argumentEdge(argumentNode, callNode, argName = keyword)
      argIndex += 1
    }

    callNode
  }

  protected def createNAryOperatorCall(
    opCodeAndFullName: () => (String, String),
    operands: Iterable[NewNode],
    lineAndColumn: LineAndColumn
  ): NewNode = {

    val (operatorCode, methodFullName) = opCodeAndFullName()
    val code     = operands.map(operandNode => codeOf(operandNode)).mkString(" " + operatorCode + " ")
    val callNode = nodeBuilder.callNode(code, methodFullName, DispatchTypes.STATIC_DISPATCH, lineAndColumn)

    addAstChildrenAsArguments(callNode, 1, operands)

    callNode
  }

  protected def createBinaryOperatorCall(
    lhsNode: NewNode,
    opCodeAndFullName: () => (String, String),
    rhsNode: NewNode,
    lineAndColumn: LineAndColumn
  ): NewCall = {
    val (opCode, opFullName) = opCodeAndFullName()

    val code     = codeOf(lhsNode) + " " + opCode + " " + codeOf(rhsNode)
    val callNode = nodeBuilder.callNode(code, opFullName, DispatchTypes.STATIC_DISPATCH, lineAndColumn)

    addAstChildrenAsArguments(callNode, 1, lhsNode, rhsNode)
    callNode
  }

  protected def createLiteralOperatorCall(
    codeStart: String,
    codeEnd: String,
    opFullName: String,
    lineAndColumn: LineAndColumn,
    operands: NewNode*
  ): NewCall = {
    val code     = operands.map(codeOf).mkString(codeStart, ", ", codeEnd)
    val callNode = nodeBuilder.callNode(code, opFullName, DispatchTypes.STATIC_DISPATCH, lineAndColumn)

    addAstChildrenAsArguments(callNode, 1, operands)

    callNode
  }

  protected def createStarredUnpackOperatorCall(unpackOperand: NewNode, lineAndColumn: LineAndColumn): NewNode = {
    val code     = "*" + codeOf(unpackOperand)
    val callNode = nodeBuilder.callNode(code, "<operator>.starredUnpack", DispatchTypes.STATIC_DISPATCH, lineAndColumn)

    addAstChildrenAsArguments(callNode, 1, unpackOperand)
    callNode
  }

  protected def createAssignment(lhsNode: NewNode, rhsNode: NewNode, lineAndColumn: LineAndColumn): NewNode = {
    val code     = codeOf(lhsNode) + " = " + codeOf(rhsNode)
    val callNode = nodeBuilder.callNode(code, Operators.assignment, DispatchTypes.STATIC_DISPATCH, lineAndColumn)

    addAstChildrenAsArguments(callNode, 1, lhsNode, rhsNode)
    // Do not include imports or function pointers
    if (!codeOf(rhsNode).startsWith("import(") && codeOf(rhsNode) != s"def ${codeOf(lhsNode)}(...)")
      contextStack.considerAsGlobalVariable(lhsNode)

    callNode
  }

  protected def createAssignmentToIdentifier(
    identifierName: String,
    rhsNode: NewNode,
    lineAndColumn: LineAndColumn
  ): NewNode = {
    val identifierNode = createIdentifierNode(identifierName, Store, lineAndColumn)
    createAssignment(identifierNode, rhsNode, lineAndColumn)
  }

  protected def createAugAssignment(
    lhsNode: NewNode,
    operatorCode: String,
    rhsNode: NewNode,
    operatorFullName: String,
    lineAndColumn: LineAndColumn
  ): NewNode = {
    val code     = codeOf(lhsNode) + " " + operatorCode + " " + codeOf(rhsNode)
    val callNode = nodeBuilder.callNode(code, operatorFullName, DispatchTypes.STATIC_DISPATCH, lineAndColumn)

    addAstChildrenAsArguments(callNode, 1, lhsNode, rhsNode)

    callNode
  }

  // Always use this method to create an identifier node instead of
  // nodeBuilder.identifierNode() directly to avoid missing to add
  // the variable reference.
  protected def createIdentifierNode(
    name: String,
    memOp: MemoryOperation,
    lineAndColumn: LineAndColumn
  ): NewIdentifier = {
    val identifierNode = nodeBuilder.identifierNode(name, lineAndColumn)
    contextStack.addVariableReference(identifierNode, memOp)
    identifierNode
  }

  protected def createIndexAccess(baseNode: NewNode, indexNode: NewNode, lineAndColumn: LineAndColumn): NewNode = {
    val code = codeOf(baseNode) + "[" + codeOf(indexNode) + "]"
    val indexAccessNode =
      nodeBuilder.callNode(code, Operators.indexAccess, DispatchTypes.STATIC_DISPATCH, lineAndColumn)

    addAstChildrenAsArguments(indexAccessNode, 1, baseNode, indexNode)

    indexAccessNode
  }

  protected def createSliceCall(
    baseNode: NewNode,
    lowerIndex: Int,
    upperIndex: Option[Int],
    lineAndColumn: LineAndColumn
  ): NewNode = {
    val lowerNode = nodeBuilder.intLiteralNode(lowerIndex.toString, lineAndColumn)
    val upperNode = upperIndex match {
      case Some(idx) => nodeBuilder.intLiteralNode(idx.toString, lineAndColumn)
      case None      => nodeBuilder.literalNode("None", None, lineAndColumn)
    }
    val stepNode = nodeBuilder.intLiteralNode("1", lineAndColumn)

    val upperStr = upperIndex.map(_.toString).getOrElse("")
    val code     = s"${codeOf(baseNode)}[${lowerIndex}:${upperStr}:1]"
    val sliceCallNode =
      nodeBuilder.callNode(code, PythonOperators.slice, DispatchTypes.STATIC_DISPATCH, lineAndColumn)

    val args = baseNode :: lowerNode :: upperNode :: stepNode :: Nil
    addAstChildrenAsArguments(sliceCallNode, 1, args)

    sliceCallNode
  }

  protected def createIndexAccessChain(
    rootNode: NewNode,
    accessChain: List[Int],
    lineAndColumn: LineAndColumn
  ): NewNode = {
    accessChain match {
      case accessIndex :: tail =>
        val baseNode  = createIndexAccessChain(rootNode, tail, lineAndColumn)
        val indexNode = nodeBuilder.intLiteralNode(accessIndex.toString, lineAndColumn)

        createIndexAccess(baseNode, indexNode, lineAndColumn)
      case Nil =>
        rootNode
    }
  }

  protected def createFieldAccess(baseNode: NewNode, fieldName: String, lineAndColumn: LineAndColumn): NewCall = {
    val fieldIdNode = nodeBuilder.fieldIdentifierNode(fieldName, lineAndColumn)

    val code     = codeOf(baseNode) + "." + codeOf(fieldIdNode)
    val callNode = nodeBuilder.callNode(code, Operators.fieldAccess, DispatchTypes.STATIC_DISPATCH, lineAndColumn)

    addAstChildrenAsArguments(callNode, 1, baseNode, fieldIdNode)
    callNode
  }

  protected def createTypeRef(typeName: String, typeFullName: String, lineAndColumn: LineAndColumn): NewTypeRef = {
    nodeBuilder.typeRefNode("class " + typeName + "(...)", typeFullName, lineAndColumn)
  }

  protected def createBinding(methodNode: NewMethod, typeDeclNode: NewTypeDecl): NewBinding = {
    val bindingNode = nodeBuilder.bindingNode()
    edgeBuilder.bindsEdge(bindingNode, typeDeclNode)
    edgeBuilder.refEdge(methodNode, bindingNode)

    bindingNode
  }

  protected def createReturn(
    returnExprOption: Option[NewNode],
    codeOption: Option[String],
    lineAndColumn: LineAndColumn
  ): NewReturn = {
    val code = codeOption.getOrElse {
      returnExprOption match {
        case Some(returnExpr) =>
          "return " + codeOf(returnExpr)
        case None =>
          "return"
      }
    }
    val returnNode = nodeBuilder.returnNode(code, lineAndColumn)
    returnExprOption.foreach { returnExpr => addAstChildrenAsArguments(returnNode, 1, returnExpr) }

    returnNode
  }

  protected def addAstChildNodes(parentNode: NewNode, startIndex: AutoIncIndex, childNodes: Iterable[NewNode]): Unit = {
    childNodes.foreach { childNode =>
      val orderIndex = startIndex.getAndInc
      edgeBuilder.astEdge(childNode, parentNode, orderIndex)
    }
  }

  protected def addAstChildNodes(parentNode: NewNode, startIndex: Int, childNodes: Iterable[NewNode]): Unit = {
    addAstChildNodes(parentNode, new AutoIncIndex(startIndex), childNodes)
  }

  protected def addAstChildNodes(parentNode: NewNode, startIndex: AutoIncIndex, childNodes: NewNode*): Unit = {
    addAstChildNodes(parentNode, startIndex, childNodes)
  }

  protected def addAstChildNodes(parentNode: NewNode, startIndex: Int, childNodes: NewNode*): Unit = {
    addAstChildNodes(parentNode, new AutoIncIndex(startIndex), childNodes)
  }

  protected def addAstChildrenAsArguments(
    parentNode: NewNode,
    startIndex: AutoIncIndex,
    childNodes: Iterable[NewNode]
  ): Unit = {
    childNodes.foreach { childNode =>
      val orderAndArgIndex = startIndex.getAndInc
      edgeBuilder.astEdge(childNode, parentNode, orderAndArgIndex)
      edgeBuilder.argumentEdge(childNode, parentNode, orderAndArgIndex)
    }
  }

  protected def addAstChildrenAsArguments(parentNode: NewNode, startIndex: Int, childNodes: Iterable[NewNode]): Unit = {
    addAstChildrenAsArguments(parentNode, new AutoIncIndex(startIndex), childNodes)
  }

  protected def addAstChildrenAsArguments(parentNode: NewNode, startIndex: AutoIncIndex, childNodes: NewNode*): Unit = {
    addAstChildrenAsArguments(parentNode, startIndex, childNodes)
  }

  protected def addAstChildrenAsArguments(parentNode: NewNode, startIndex: Int, childNodes: NewNode*): Unit = {
    addAstChildrenAsArguments(parentNode, new AutoIncIndex(startIndex), childNodes)
  }
}
