package io.joern.jssrc2cpg.datastructures.scope

import io.shiftleft.codepropertygraph.generated.nodes.NewNode

case class PendingReference(variableName: String, referenceNodeId: NewNode, stack: Option[ScopeElement]) {

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
