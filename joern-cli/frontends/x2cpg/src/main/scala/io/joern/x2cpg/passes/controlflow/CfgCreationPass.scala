package io.joern.x2cpg.passes.controlflow

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language._
import io.joern.x2cpg.passes.controlflow.cfgcreation.CfgCreator

/** A pass that creates control flow graphs from abstract syntax trees.
  *
  * Control flow graphs can be calculated independently per method. Therefore, we inherit from
  * `ForkJoinParallelCpgPass`.
  *
  * Note: the version of OverflowDB that we currently use as a storage backend does not assign ids to edges and this
  * pass only creates edges at the moment. Therefore, we currently do without key pools.
  */
class CfgCreationPass(cpg: Cpg) extends ForkJoinParallelCpgPass[Method](cpg) {

  override def generateParts(): Array[Method] = cpg.method.toArray

  override def runOnPart(diffGraph: DiffGraphBuilder, method: Method): Unit = {
    val localDiff = Cpg.newDiffGraphBuilder
    new CfgCreator(method, localDiff).run()
    diffGraph.absorb(localDiff)
  }

}
