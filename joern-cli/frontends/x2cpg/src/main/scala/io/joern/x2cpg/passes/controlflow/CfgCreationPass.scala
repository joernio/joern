package io.joern.x2cpg.passes.controlflow

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language.*
import io.joern.x2cpg.passes.controlflow.cfgcreation.CfgCreator

/** A pass that creates control flow graphs from abstract syntax trees.
  *
  * Control flow graphs can be calculated independently per method. Therefore, we inherit from
  * `ForkJoinParallelCpgPass`.
  */
class CfgCreationPass(cpg: Cpg) extends ForkJoinParallelCpgPass[Method](cpg) {

  override def generateParts(): Array[Method] = cpg.method.toArray

  override def runOnPart(diffGraph: DiffGraphBuilder, method: Method): Unit = {
    new CfgCreator(method, diffGraph).run()
  }

}
