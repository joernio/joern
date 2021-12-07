package io.joern.ghidra2cpg.passes.mips

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, PropertyNames}
import io.shiftleft.passes.{CpgPass, DiffGraph}
import io.shiftleft.semanticcpg.language._
import org.slf4j.{Logger, LoggerFactory}

class MipsReturnEdgesPass(cpg: Cpg) extends CpgPass(cpg) {
  private val logger: Logger = LoggerFactory.getLogger(this.getClass)

  override def run(): Iterator[DiffGraph] = {
    logger.info("Running ReturnEdgesPass")
    implicit val diffGraph: DiffGraph.Builder = DiffGraph.newBuilder
    cpg.call.nameNot("<operator>.*").foreach { from =>
      // We expect v1|v0 as return
      // Note: this is not sufficient but a first approach without using data flow
      //        the first .cfgNext is skipping a _nop instruction after the call
      val to = from.cfgNext.cfgNext.isCall.argument.code("v(0|1)").headOption
      if (to.nonEmpty) {
        diffGraph.addEdge(from, to.get, EdgeTypes.REACHING_DEF, Seq((PropertyNames.VARIABLE, from.code)))
      }
    }
    Iterator(diffGraph.build())
  }
}
