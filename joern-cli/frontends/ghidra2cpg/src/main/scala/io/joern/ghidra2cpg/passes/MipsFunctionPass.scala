package io.joern.ghidra2cpg.passes

import ghidra.app.decompiler.DecompInterface
import ghidra.program.model.listing.{Function, Program}
import io.joern.ghidra2cpg.processors.Processor
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, PropertyNames}
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool}
import io.shiftleft.semanticcpg.language._
import org.slf4j.{Logger, LoggerFactory}

class MipsFunctionPass(processor: Processor,
                       currentProgram: Program,
                       filename: String,
                       functions: List[Function],
                       function: Function,
                       cpg: Cpg,
                       keyPool: IntervalKeyPool,
                       decompInterface: DecompInterface)
    extends FunctionPass(processor, currentProgram, filename, functions, function, cpg, keyPool, decompInterface) {
  private val logger: Logger = LoggerFactory.getLogger(this.getClass)
  def handleLoHi() = {
    logger.info("Running LoHiPass...")

    val readFromLoHiRegsRegex = "_?(mflo|mfhi).*"
    val writeToLoHiRegsRegex = "_?(div|divu|mul|mult).*"
    def from = cpg.call.code(writeToLoHiRegsRegex).l
println("from: " + cpg.call.size)
    def to = cpg.call.code(readFromLoHiRegsRegex).l
    println("to: " + to.size)
    // TODO: improve the pair creation to take into consideration register value overwrites.
    // e.g. in pseudo-assembly: div X Y; nop; nop; mflo Z; nop; nop; div P Q; mflo R
    // it should not add REACHING_DEF edges from X to R and Y to R, but it currently does
    def fromToPairs = for (fromNode <- from; toNode <- to) yield (fromNode, toNode)

    println("AAAAAAAAAAAAAa")
    println(fromToPairs)
    fromToPairs.foreach { pair =>
      logger.debug("Adding REACHING_DEF EDGE for node pair `" + pair._1 + " | " + pair._2 + "`.")
      diffGraph.addEdge(pair._1, pair._2, EdgeTypes.REACHING_DEF, Seq((PropertyNames.VARIABLE, pair._1.code)))
    }
  }

  override def runOnPart(part: String): Iterator[DiffGraph] = {
    createMethodNode()
    handleParameters()
    handleLocals()
    handleBody()
    diffGraph.build()
    println("BBBBBBBBBBBB " + cpg.all.p)
    handleLoHi()
    Iterator(diffGraph.build())
  }
}
