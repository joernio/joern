package io.joern.ghidra2cpg.passes.x86

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, PropertyNames}
import io.shiftleft.passes.{CpgPass, DiffGraph}
import io.shiftleft.semanticcpg.language._
import org.slf4j.{Logger, LoggerFactory}

class ReturnEdgesPass(cpg: Cpg) extends CpgPass(cpg) {
  private val logger: Logger = LoggerFactory.getLogger(this.getClass)

  override def run(): Iterator[DiffGraph] = {
    logger.info("Running ReturnEdgesPass")
    implicit val diffGraph: DiffGraph.Builder = DiffGraph.newBuilder

    cpg.call.nameNot("<operator>.*").foreach { from =>
      // We expect RAX/EAX as return
      val to = from.cfgNext.isCall.argument.code("(R|E)AX").headOption
      if (to.nonEmpty) {
        diffGraph.addEdge(from, to.get, EdgeTypes.REACHING_DEF, Seq((PropertyNames.VARIABLE, from.code)))
      }
    }
    Iterator(diffGraph.build())
  }
}
