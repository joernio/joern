package io.joern.ghidra2cpg.passes.mips

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, PropertyNames}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language.*

class LoHiPass(cpg: Cpg) extends ForkJoinParallelCpgPass[(Call, Call)](cpg) {
  override def generateParts(): Array[(Call, Call)] = {
    val readFromLoHiRegsRegex = "_?(mflo|mfhi).*"

    val writeToLoHiRegsRegex = "_?(div|divu|mul|mult).*"

    def from: Seq[Call] = cpg.call.code(writeToLoHiRegsRegex).l

    def to: Seq[Call] = cpg.call.code(readFromLoHiRegsRegex).l

    // TODO: improve the pair creation to take into consideration register value overwrites.
    // e.g. in pseudo-assembly: div X Y; nop; nop; mflo Z; nop; nop; div P Q; mflo R
    // it should not add REACHING_DEF edges from X to R and Y to R, but it currently does
    for (fromNode <- from; toNode <- to) yield (fromNode, toNode)
  }.toArray

  override def runOnPart(diffGraph: DiffGraphBuilder, pair: (Call, Call)): Unit = {
    diffGraph.addEdge(pair._1, pair._2, EdgeTypes.REACHING_DEF, PropertyNames.VARIABLE, pair._1.code)
  }
}
