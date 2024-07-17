package io.joern.ghidra2cpg.passes.mips

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, PropertyNames}
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language.*
import org.slf4j.{Logger, LoggerFactory}

class MipsReturnEdgesPass(cpg: Cpg) extends CpgPass(cpg) {
  private val logger: Logger = LoggerFactory.getLogger(this.getClass)

  override def run(diffGraph: DiffGraphBuilder): Unit = {
    logger.info("Running ReturnEdgesPass")
    cpg.call.nameNot("<operator>.*").foreach { from =>
      // We expect v1|v0 as return
      // Note: this is not sufficient but a first approach without using data flow
      //        the first .cfgNext is skipping a _nop instruction after the call
      val to = from.cfgNext.cfgNext.isCall.argument.code("v(0|1)").headOption
      if (to.nonEmpty) {
        // in flatgraph an edge may have zero or one properties and they're not named...
        // in this case we know that we're dealing with ReachingDef edges which has the `variable` property
        val variableProperty = from.code
        diffGraph.addEdge(from, to.get, EdgeTypes.REACHING_DEF, variableProperty)
      }
    }
  }

}
