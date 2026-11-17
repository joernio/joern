package io.joern.dataflowengineoss.passes.reachingdef

import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.joern.x2cpg.Defines as X2CpgDefines
import io.shiftleft.codepropertygraph.generated.{Cpg, EvaluationStrategies, Languages}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language.*
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable

/** A pass that calculates reaching definitions ("data dependencies").
  */
class ReachingDefPass(
  cpg: Cpg,
  maxNumberOfDefinitions: Int = 4000,
  byValueArguments: Boolean = X2CpgDefines.byValueArgumentsEnabled
)(implicit semantics: Semantics)
    extends ForkJoinParallelCpgPass[Method](cpg) {

  private val logger: Logger = LoggerFactory.getLogger(this.getClass)
  // If there are any regex method full names, load them early
  semantics.initialize(cpg)

  override def generateParts(): Array[Method] = cpg.method.toArray

  override def runOnPart(dstGraph: DiffGraphBuilder, method: Method): Unit = {
    logger.info("Calculating reaching definitions for: {} in {}", method.fullName, method.filename)
    val problem = createProblem(method)
    if (shouldBailOut(method, problem)) {
      logger.warn("Skipping.")
      return
    }

    val solution     = new DataFlowSolver().calculateMopSolutionForwards(problem)
    val ddgGenerator = new DdgGenerator(semantics)
    ddgGenerator.addReachingDefEdges(dstGraph, method, problem, solution)
  }

  /** The reaching-definition problem of `method`. Creating it builds the flow graph, which with the count of
    * definitions is all that [[shouldBailOut]] reads; the `gen` and kill sets are built when the solver first needs
    * them.
    */
  private[reachingdef] def createProblem(method: Method): DataFlowProblem[CfgNode, mutable.BitSet] =
    ReachingDefProblem.create(method, readsByValueArguments)

  /** Whether a call leaves out the arguments its callee receives by value: with `byValueArguments`, and only in a C or
    * C++ graph whose strategies c2cpg decided from the parameters' types, as it does when the same switch is on.
    * Without the switch c2cpg gives every parameter `BY_VALUE`, so a graph in which every parameter but `this` is
    * `BY_VALUE` was either built without it, or has only parameters that receive a copy; the pass leaves its
    * definitions as they are.
    */
  private lazy val readsByValueArguments: Boolean =
    byValueArguments && cpg.metaData.language.headOption.exists(Set(Languages.NEWC, Languages.C).contains) &&
      cpg.parameter.exists(parameter =>
        parameter.name != "this" && parameter.evaluationStrategy != EvaluationStrategies.BY_VALUE
      )

  /** Before we start propagating definitions in the graph, which is the bulk of the work, we check how many definitions
    * were are dealing with in total. If a threshold is reached, we bail out instead, leaving reaching definitions
    * uncalculated for the method in question. Users can increase the threshold if desired.
    */
  private def shouldBailOut(method: Method, problem: DataFlowProblem[CfgNode, mutable.BitSet]): Boolean = {
    val transferFunction = problem.transferFunction.asInstanceOf[ReachingDefTransferFunction]
    // The number of definitions in the `gen` sets, counted without building them
    val numberOfDefinitions = transferFunction.numberOfDefinitions
    logger.info("Number of definitions for {}: {}", method.fullName, numberOfDefinitions)
    if (numberOfDefinitions > maxNumberOfDefinitions) {
      logger.warn("{} has more than {} definitions", method.fullName, maxNumberOfDefinitions)
      true
    } else {
      false
    }
  }

}
