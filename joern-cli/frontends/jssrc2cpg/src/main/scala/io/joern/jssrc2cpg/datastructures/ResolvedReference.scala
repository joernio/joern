package io.joern.jssrc2cpg.datastructures

import io.shiftleft.codepropertygraph.generated.v2.nodes.NewNode

case class ResolvedReference(variableNodeId: NewNode, origin: PendingReference)
