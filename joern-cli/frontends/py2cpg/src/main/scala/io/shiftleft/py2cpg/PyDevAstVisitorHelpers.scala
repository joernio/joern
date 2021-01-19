package io.shiftleft.py2cpg

import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators, nodes}

trait PyDevAstVisitorHelpers { this: PyDevAstVisitor =>

  protected def codeOf(node: nodes.NewNode): String = {
    node.asInstanceOf[nodes.HasCode].code
  }

  protected def createAssignment(
      lhsNode: nodes.NewNode,
      rhsNode: nodes.NewNode,
      lineNumber: Int,
      columnNumber: Int
  ): nodes.NewNode = {
    val code = codeOf(lhsNode) + " = " + codeOf(rhsNode)
    val callNode = nodeBuilder.callNode(
      code,
      Operators.assignment,
      DispatchTypes.STATIC_DISPATCH,
      lineNumber,
      columnNumber
    )

    addAstChildrenAsArguments(callNode, 1, lhsNode, rhsNode)

    callNode
  }

  protected def createIndexAccess(
      baseNode: nodes.NewNode,
      indexNode: nodes.NewNode,
      lineNumber: Int,
      columnNumber: Int
  ): nodes.NewNode = {
    val code = codeOf(baseNode) + "[" + codeOf(indexNode) + "]"
    val indexAccessNode = nodeBuilder.callNode(
      code,
      Operators.indexAccess,
      DispatchTypes.STATIC_DISPATCH,
      lineNumber,
      columnNumber
    )

    addAstChildrenAsArguments(indexAccessNode, 1, baseNode, indexNode)

    indexAccessNode
  }

  protected def createIndexAccessChain(
      rootNode: nodes.NewNode,
      accessChain: List[Int],
      lineNumber: Int,
      columnNumber: Int
  ): nodes.NewNode = {
    accessChain match {
      case accessIndex :: tail =>
        val baseNode = createIndexAccessChain(rootNode, tail, lineNumber, columnNumber)
        val indexNode = nodeBuilder.numberLiteralNode(accessIndex, lineNumber, columnNumber)

        createIndexAccess(baseNode, indexNode, lineNumber, columnNumber)
      case Nil =>
        rootNode
    }
  }

  protected def addAstChildNodes(
      parentNode: nodes.NewNode,
      startIndex: Int,
      childNodes: Iterable[nodes.NewNode]
  ): Int = {
    var orderIndex = startIndex
    childNodes.foreach { childNode =>
      edgeBuilder.astEdge(childNode, parentNode, orderIndex)
      orderIndex += 1
    }
    orderIndex
  }

  protected def addAstChildNodes(
      parentNode: nodes.NewNode,
      startIndex: Int,
      childNodes: nodes.NewNode*
  ): Int = {
    addAstChildNodes(parentNode, startIndex, childNodes)
  }

  protected def addAstChildrenAsArguments(
      parentNode: nodes.NewNode,
      startIndex: Int,
      childNodes: Iterable[nodes.NewNode]
  ): Int = {
    var orderAndArgIndex = startIndex
    childNodes.foreach { childNode =>
      edgeBuilder.astEdge(childNode, parentNode, orderAndArgIndex)
      edgeBuilder.argumentEdge(childNode, parentNode, orderAndArgIndex)
      orderAndArgIndex += 1
    }
    orderAndArgIndex
  }

  protected def addAstChildrenAsArguments(
      parentNode: nodes.NewNode,
      startIndex: Int,
      childNodes: nodes.NewNode*
  ): Int = {
    addAstChildrenAsArguments(parentNode, startIndex, childNodes)
  }
}
