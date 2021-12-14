package io.joern.console

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.StoredNode
import overflowdb.traversal.Traversal


case class TraversalWithStrRep(traversal: Cpg => Traversal[_ <: StoredNode], strRep: String = "")
