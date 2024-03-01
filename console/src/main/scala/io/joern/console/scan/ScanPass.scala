package io.joern.console.scan

import io.joern.console.Query
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.ForkJoinParallelCpgPass

class ScanPass(cpg: Cpg, queries: List[Query]) extends ForkJoinParallelCpgPass[Query](cpg) {

  override def generateParts(): Array[Query] = queries.toArray

  override def runOnPart(diffGraph: DiffGraphBuilder, query: Query): Unit = {
    query(cpg).foreach(diffGraph.addNode)
  }

}
