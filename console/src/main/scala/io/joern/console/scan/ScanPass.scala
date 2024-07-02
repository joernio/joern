package io.joern.console.scan

import io.joern.console.Query
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.CpgPass

/** Each query runs the data-flow engine, which is already parallelized. Another layer of parallelism causes undefined
  * behaviour on the underlying database. This is why we use `CpgPass` instead of `ForkJoinParallelCpgPass` or similar.
  */
class ScanPass(cpg: Cpg, queries: List[Query]) extends CpgPass(cpg) {

  override def run(diffGraph: DiffGraphBuilder): Unit = {
    queries.flatMap(_.apply(cpg)).foreach(diffGraph.addNode)
  }

}
