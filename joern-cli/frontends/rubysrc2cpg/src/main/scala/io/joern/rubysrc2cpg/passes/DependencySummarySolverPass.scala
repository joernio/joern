package io.joern.rubysrc2cpg.passes;

import io.joern.rubysrc2cpg.datastructures.RubyProgramSummary
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language.*

class DependencySummarySolverPass(cpg: Cpg, dependencySummary: RubyProgramSummary)
    extends ForkJoinParallelCpgPass[Call](cpg) {
  override def generateParts(): Array[Call]                             = cpg.call.toArray
  override def runOnPart(diffGraph: DiffGraphBuilder, call: Call): Unit = {}
}
