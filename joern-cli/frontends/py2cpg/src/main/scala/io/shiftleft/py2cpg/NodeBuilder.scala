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
      lineAndColumn: LineAndColumn
  ): nodes.NewCall = {
    val callNode = new nodes.NewCall(
      code = code,
      name = name,
      methodFullName = name,
      dispatchType = dispatchType,
      lineNumber = Some(lineAndColumn.line),
      columnNumber = Some(lineAndColumn.column)
    )
    addNodeToDiff(callNode)
  }

  def identifierNode(name: String, lineAndColumn: LineAndColumn): nodes.NewIdentifier = {
    val identifierNode = new nodes.NewIdentifier(
      code = name,
      name = name,
      lineNumber = Some(lineAndColumn.line),
      columnNumber = Some(lineAndColumn.column)
    )
    addNodeToDiff(identifierNode)
  }

  def numberLiteralNode(number: Int, lineAndColumn: LineAndColumn): nodes.NewLiteral = {
    numberLiteralNode(number.toString, lineAndColumn)
  }

  def numberLiteralNode(number: String, lineAndColumn: LineAndColumn): nodes.NewLiteral = {
    val literalNode = new nodes.NewLiteral(
      code = number.toString,
      lineNumber = Some(lineAndColumn.line),
      columnNumber = Some(lineAndColumn.column)
    )
    addNodeToDiff(literalNode)
  }

  def stringLiteralNode(string: String, lineAndColumn: LineAndColumn): nodes.NewLiteral = {
    val literalNode = new nodes.NewLiteral(
      code = string,
      lineNumber = Some(lineAndColumn.line),
      columnNumber = Some(lineAndColumn.column)
    )
    addNodeToDiff(literalNode)
  }

  def blockNode(code: String, lineAndColumn: LineAndColumn): nodes.NewBlock = {
    val blockNode = new nodes.NewBlock(
      code = code,
      lineNumber = Some(lineAndColumn.line),
      columnNumber = Some(lineAndColumn.column)
    )
    addNodeToDiff(blockNode)
  }

  def localNode(name: String): nodes.NewLocal = {
    val localNode = new nodes.NewLocal(
      code = name,
      name = name
    )
    addNodeToDiff(localNode)
  }

  def metaNode(language: String, version: String): nodes.NewMetaData = {
    val metaNode = new nodes.NewMetaData(
      language = language,
      version = version
    )
    addNodeToDiff(metaNode)
  }
}
