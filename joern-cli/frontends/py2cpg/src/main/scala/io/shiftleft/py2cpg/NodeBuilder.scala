package io.shiftleft.py2cpg

import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.passes.DiffGraph

class NodeBuilder(diffGraph: DiffGraph.Builder) {

  private def addNodeToDiff[T <: nodes.NewNode](node: T): T = {
    diffGraph.addNode(node)
    node
  }

  def callNode(
      code: String,
      name: String,
      dispatchType: String,
      lineNumber: Int,
      columnNumber: Int
  ): nodes.NewCall = {
    val callNode = new nodes.NewCall(
      code = code,
      name = name,
      methodFullName = name,
      dispatchType = dispatchType,
      lineNumber = Some(lineNumber),
      columnNumber = Some(columnNumber)
    )
    addNodeToDiff(callNode)
  }

  def identifierNode(name: String, lineNumber: Int, columnNumber: Int): nodes.NewIdentifier = {
    val identifierNode = new nodes.NewIdentifier(
      code = name,
      name = name,
      lineNumber = Some(lineNumber),
      columnNumber = Some(columnNumber)
    )
    addNodeToDiff(identifierNode)
  }

  def numberLiteralNode(number: Int, lineNumber: Int, columnNumber: Int): nodes.NewLiteral = {
    numberLiteralNode(number.toString, lineNumber, columnNumber)
  }

  def numberLiteralNode(number: String, lineNumber: Int, columnNumber: Int): nodes.NewLiteral = {
    val literalNode = new nodes.NewLiteral(
      code = number.toString,
      lineNumber = Some(lineNumber),
      columnNumber = Some(columnNumber)
    )
    addNodeToDiff(literalNode)
  }

  def stringLiteralNode(string: String, lineNumber: Int, columnNumber: Int): nodes.NewLiteral = {
    val literalNode = new nodes.NewLiteral(
      code = string,
      lineNumber = Some(lineNumber),
      columnNumber = Some(columnNumber)
    )
    addNodeToDiff(literalNode)
  }

  def blockNode(lineNumber: Int, columnNumber: Int): nodes.NewBlock = {
    val blockNode = new nodes.NewBlock(
      lineNumber = Some(lineNumber),
      columnNumber = Some(columnNumber)
    )
    addNodeToDiff(blockNode)
  }

  def metaNode(language: String, version: String): nodes.NewMetaData = {
    val metaNode = new nodes.NewMetaData(
      language = language,
      version = version
    )
    addNodeToDiff(metaNode)
  }
}
