package io.joern.dataflowengineoss.passes.reachingdef

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, PropertyNames}
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

    val solution = new DataFlowSolver().calculateMopSolutionForwards(problem)
    addReachingDefEdges(dstGraph, problem, method, solution)
    addEdgesFromLoneIdentifiersToExit(dstGraph, problem, method, solution)
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

  /** Once reaching definitions have been computed, we create a data dependence graph by seeing which of these reaching
    * definitions are relevant, meaning that a symbol is propagated that is used by the target node.
    */
  private def addReachingDefEdges(
    dstGraph: DiffGraphBuilder,
    problem: DataFlowProblem[mutable.BitSet],
    method: Method,
    solution: Solution[mutable.BitSet]
  ): Unit = {
    implicit val implicitDst = dstGraph
    val numberToNode         = problem.flowGraph.asInstanceOf[ReachingDefFlowGraph].numberToNode
    val in                   = solution.in
    val gen = solution.problem.transferFunction
      .asInstanceOf[ReachingDefTransferFunction]
      .gen

    val allNodes      = in.keys.toList
    val usageAnalyzer = new UsageAnalyzer(problem, in)

    allNodes.foreach {
      case call: Call =>
        // Edges between arguments of call sites
        usageAnalyzer.usedIncomingDefs(call).foreach { case (use, ins) =>
          ins.foreach { in =>
            val inNode = numberToNode(in)
            if (inNode != use) {
              addEdge(inNode, use, nodeToEdgeLabel(inNode))
            }
          }
        }

        // For all calls, assume that input arguments
        // taint corresponding output arguments
        // and the return value
        usageAnalyzer.uses(call).foreach { use =>
          gen(call).foreach { g =>
            val genNode = numberToNode(g)
            if (use != genNode && nodeMayBeSource(use)) {
              addEdge(use, genNode, nodeToEdgeLabel(use))
            }
          }
        }

      case ret: Return =>
        usageAnalyzer.usedIncomingDefs(ret).foreach { case (use, inElements) =>
          addEdge(use, ret, use.asInstanceOf[CfgNode].code)
          inElements.filter(x => numberToNode(x) != use).foreach { inElement =>
            val inElemNode = numberToNode(inElement)
            addEdge(inElemNode, ret, nodeToEdgeLabel(inElemNode))
          }
          if (inElements.isEmpty) {
            addEdge(method, ret)
          }
        }
        addEdge(ret, method.methodReturn, "<RET>")

      case exitNode: MethodReturn =>
        in(exitNode).foreach { i =>
          val iNode = numberToNode(i)
          addEdge(iNode, exitNode, nodeToEdgeLabel(iNode))
        }
      case _ =>
    }

    // Add edges from the entry node
    allNodes
      .filter(nodeMayBeSource)
      .foreach { node =>
        if (usageAnalyzer.usedIncomingDefs(node).isEmpty) {
          addEdge(method, node)
        }
      }

    // Add edges for blocks used as arguments
    allNodes
      .collect { case c: Call => c }
      .foreach { call =>
        call.argument.isBlock.foreach { block =>
          block.astChildren.lastOption match {
            case None => // Do nothing
            case Some(node: Identifier) =>
              val edgesToAdd = in(node).toList.flatMap { inDef =>
                numberToNode(inDef) match {
                  case identifier: Identifier => Some(identifier)
                  case _                      => None
                }
              }
              edgesToAdd.foreach { inNode =>
                addEdge(inNode, block, nodeToEdgeLabel(inNode))
              }
              if (edgesToAdd.nonEmpty) {
                addEdge(block, call)
              }
            case Some(node: Call) =>
              addEdge(node, call, nodeToEdgeLabel(node))
              addEdge(block, call)
            case _ => // Do nothing
          }
        }
      }
  }

  private def addEdge(fromNode: StoredNode, toNode: StoredNode, variable: String = "")(implicit
    dstGraph: DiffGraphBuilder
  ): Unit = {
    if (
      fromNode.isInstanceOf[Unknown] || toNode
        .isInstanceOf[Unknown]
    )
      return
    dstGraph.addEdge(fromNode, toNode, EdgeTypes.REACHING_DEF, PropertyNames.VARIABLE, variable)
  }

  private def nodeMayBeSource(x: StoredNode): Boolean = {
    !(
      x.isInstanceOf[Method] || x
        .isInstanceOf[ControlStructure] || x.isInstanceOf[FieldIdentifier] || x
        .isInstanceOf[JumpTarget] || x.isInstanceOf[MethodReturn] || x.isInstanceOf[Block]
    )
  }

  private def nodeToEdgeLabel(node: StoredNode): String = {
    node match {
      case n: MethodParameterIn => n.name
      case n: CfgNode           => n.code
      case _                    => ""
    }
  }

  /** This is part of the Lone-identifier optimization: as we remove lone identifiers from `gen` sets, we must now
    * retrieve them and create an edge from each lone identifier to the exit node.
    */
  def addEdgesFromLoneIdentifiersToExit(
    builder: DiffGraphBuilder,
    problem: DataFlowProblem[mutable.BitSet],
    method: Method,
    solution: Solution[mutable.BitSet]
  ): Unit = {
    val numberToNode                        = problem.flowGraph.asInstanceOf[ReachingDefFlowGraph].numberToNode
    implicit val dstGraph: DiffGraphBuilder = builder
    val exitNode                            = method.methodReturn
    val transferFunction = solution.problem.transferFunction.asInstanceOf[OptimizedReachingDefTransferFunction]
    val genOnce          = transferFunction.loneIdentifiers
    genOnce.foreach { case (_, defs) =>
      defs.foreach { d =>
        val dNode = numberToNode(d)
        addEdge(dNode, exitNode, nodeToEdgeLabel(dNode))
      }
    }
  }

}
