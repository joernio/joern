package io.joern.swiftsrc2cpg.datastructures

import io.shiftleft.codepropertygraph.generated.nodes.NewNode

case class PendingReference(variableName: String, referenceNode: NewNode, stack: Option[ScopeElement]) {

  def tryResolve(): Option[ResolvedReference] = {
    var foundVariableOption = Option.empty[NewNode]
    val stackIterator       = new ScopeElementIterator(stack)

    while (stackIterator.hasNext && foundVariableOption.isEmpty) {
      val scopeElement = stackIterator.next()
      foundVariableOption = scopeElement.nameToVariableNode.get(variableName)
    }

    foundVariableOption.map { variableNodeId =>
      ResolvedReference(variableNodeId, this)
    }
  }

}
