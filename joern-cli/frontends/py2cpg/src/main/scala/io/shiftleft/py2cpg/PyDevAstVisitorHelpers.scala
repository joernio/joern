package io.shiftleft.py2cpg

import io.shiftleft.codepropertygraph.generated.nodes

trait PyDevAstVisitorHelpers { this: PyDevAstVisitor =>

  protected def codeOf(node: nodes.NewNode): String = {
    node.asInstanceOf[nodes.HasCode].code
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
