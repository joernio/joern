package io.joern.dataflowengineoss.passes.reachingdef

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language._
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable

/** A pass that calculates reaching definitions ("data dependencies").
  */
class ReachingDefPass(cpg: Cpg, maxNumberOfDefinitions: Int = 4000) extends ForkJoinParallelCpgPass[Method](cpg) {

  private val logger: Logger = LoggerFactory.getLogger(this.getClass)

  override def generateParts(): Array[Method] = cpg.method.toArray

  override def runOnPart(dstGraph: DiffGraphBuilder, method: Method): Unit = {
    logger.info("Calculating reaching definitions for: {} in {}", method.fullName, method.filename)
    val problem = ReachingDefProblem.create(method)
    if (shouldBailOut(problem)) {
      logger.warn("Skipping.")
      return
    }

    val solution     = new DataFlowSolver().calculateMopSolutionForwards(problem)
    val ddgGenerator = new DdgGenerator()
    ddgGenerator.addReachingDefEdges(dstGraph, problem, solution)
  }

  /** Before we start propagating definitions in the graph, which is the bulk of the work, we check how many definitions
    * were are dealing with in total. If a threshold is reached, we bail out instead, leaving reaching definitions
    * uncalculated for the method in question. Users can increase the threshold if desired.
    */
  private def shouldBailOut(problem: DataFlowProblem[mutable.BitSet]): Boolean = {
    val method           = problem.flowGraph.entryNode.asInstanceOf[Method]
    val transferFunction = problem.transferFunction.asInstanceOf[ReachingDefTransferFunction]
    // For each node, the `gen` map contains the list of definitions it generates
    // We add up the sizes of these lists to obtain the total number of definitions
    val numberOfDefinitions = transferFunction.gen.foldLeft(0)(_ + _._2.size)
    logger.info("Number of definitions for {}: {}", method.fullName, numberOfDefinitions)
    if (numberOfDefinitions > maxNumberOfDefinitions) {
      logger.warn("{} has more than {} definitions", method.fullName, maxNumberOfDefinitions)
      true
    } else {
      false
    }
  }

}
