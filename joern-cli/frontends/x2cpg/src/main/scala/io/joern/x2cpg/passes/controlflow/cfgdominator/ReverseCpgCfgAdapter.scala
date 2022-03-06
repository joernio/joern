package io.joern.x2cpg.passes.controlflow.cfgdominator

import io.shiftleft.codepropertygraph.generated.nodes.StoredNode

import scala.jdk.CollectionConverters._

class ReverseCpgCfgAdapter extends CfgAdapter[StoredNode] {
  override def successors(node: StoredNode): IterableOnce[StoredNode] = {
    node._cfgIn.asScala
  }

  override def predecessors(node: StoredNode): IterableOnce[StoredNode] = {
    node._cfgOut.asScala
  }
}
