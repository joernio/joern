package io.joern.ghidra2cpg.passes.x86

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, PropertyNames}
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory

class ReturnEdgesPass(cpg: Cpg) extends CpgPass(cpg) {
  private val logger = LoggerFactory.getLogger(this.getClass)

  override def run(diffGraph: DiffGraphBuilder): Unit = {
    logger.info("Running ReturnEdgesPass")

    cpg.call.nameNot("<operator>.*").foreach { from =>
      // We expect RAX/EAX as return
      val to = from.cfgNext.isCall.argument.code("(R|E)AX").headOption
      if (to.nonEmpty) diffGraph.addEdge(from, to.get, EdgeTypes.REACHING_DEF, PropertyNames.VARIABLE, from.code)
    }
  }

}
