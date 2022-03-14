package io.joern.ghidra2cpg.passes.mips

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, PropertyNames}
import io.shiftleft.passes.SimpleCpgPass
import io.shiftleft.semanticcpg.language._
import org.slf4j.{Logger, LoggerFactory}

class LoHiPass(cpg: Cpg) extends SimpleCpgPass(cpg) {
  private val logger = LoggerFactory.getLogger(this.getClass)

  override def run(diffGraph: DiffGraphBuilder): Unit = {
    logger.info("Running LoHiPass...")

    val readFromLoHiRegsRegex = "_?(mflo|mfhi).*"

    val writeToLoHiRegsRegex = "_?(div|divu|mul|mult).*"

    def from = cpg.call.code(writeToLoHiRegsRegex).l

    def to = cpg.call.code(readFromLoHiRegsRegex).l

    // TODO: improve the pair creation to take into consideration register value overwrites.
    // e.g. in pseudo-assembly: div X Y; nop; nop; mflo Z; nop; nop; div P Q; mflo R
    // it should not add REACHING_DEF edges from X to R and Y to R, but it currently does
    def fromToPairs = for (fromNode <- from; toNode <- to) yield (fromNode, toNode)
    val localDiffGraph = new DiffGraphBuilder
    fromToPairs.foreach { pair =>
      logger.debug("Adding REACHING_DEF EDGE for node pair `" + pair._1 + " | " + pair._2 + "`.")
      localDiffGraph.addEdge(pair._1, pair._2, EdgeTypes.REACHING_DEF, PropertyNames.VARIABLE, pair._1.code)
    }
    diffGraph.absorb(localDiffGraph)
  }
}
