package io.joern.x2cpg.passes.controlflow.codepencegraph

import io.shiftleft.codepropertygraph.generated.nodes.StoredNode
import io.joern.x2cpg.passes.controlflow.cfgdominator.DomTreeAdapter

class CpgPostDomTreeAdapter extends DomTreeAdapter[StoredNode] {

  override def immediateDominator(cfgNode: StoredNode): Option[StoredNode] =
    cfgNode._postDominateIn.nextOption()

}
