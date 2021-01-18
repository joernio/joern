package io.shiftleft.py2cpg

import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.codepropertygraph.generated.nodes.{NewIdentifier, NewLiteral, NewMetaData}
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
    val identifierNode = new NewIdentifier(
      code = name,
      name = name,
      lineNumber = Some(lineNumber),
      columnNumber = Some(columnNumber)
    )
    addNodeToDiff(identifierNode)
  }

  def literalNode(code: String, lineNumber: Int, columnNumber: Int): nodes.NewLiteral = {
    val literalNode = new NewLiteral(
      code = code,
      lineNumber = Some(lineNumber),
      columnNumber = Some(columnNumber)
    )
    addNodeToDiff(literalNode)
  }

  def metaNode(language: String, version: String): nodes.NewMetaData = {
    val metaNode = new NewMetaData(
      language = language,
      version = version
    )
    addNodeToDiff(metaNode)
  }
}
