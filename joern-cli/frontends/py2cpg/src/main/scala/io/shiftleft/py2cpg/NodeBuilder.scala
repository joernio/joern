package io.shiftleft.py2cpg

import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.codepropertygraph.generated.nodes.{NewIdentifier, NewLiteral}
import io.shiftleft.passes.DiffGraph

class NodeBuilder(diffGraph: DiffGraph.Builder) {

  private def addNodeToDiff[T <: nodes.NewNode](node: T): T = {
    diffGraph.addNode(node)
    node
  }

  def callNode(): nodes.NewCall = {
    val callNode = new nodes.NewCall()
    addNodeToDiff(callNode)
  }

  def identifierNode(name: String): nodes.NewIdentifier = {
    val identifierNode = new NewIdentifier(
      name = name
    )
    addNodeToDiff(identifierNode)
  }

  def literalNode(code: String): nodes.NewLiteral = {
    val literalNode = new NewLiteral(
      code = code
    )
    addNodeToDiff(literalNode)
  }
}
