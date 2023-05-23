package io.joern.x2cpg.passes.controlflow.cfgdominator

import io.shiftleft.codepropertygraph.generated.nodes.StoredNode

import scala.jdk.CollectionConverters._

class CpgCfgAdapter extends CfgAdapter[StoredNode] {
  override def successors(node: StoredNode): IterableOnce[StoredNode] = {
    node._cfgOut.asScala
  }

  override def predecessors(node: StoredNode): IterableOnce[StoredNode] = {
    node._cfgIn.asScala
  }
}
