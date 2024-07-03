package io.shiftleft.semanticcpg.dotgenerator

import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.StoredNode
import io.shiftleft.semanticcpg.dotgenerator.DotSerializer.Edge

import scala.jdk.CollectionConverters.*

class CdgGenerator extends CfgGenerator {

  override val edgeType: String = EdgeTypes.CDG

  override def expand(v: StoredNode): Iterator[Edge] =
    v._cdgOut.map(node => Edge(v, node, edgeType = edgeType))

}
