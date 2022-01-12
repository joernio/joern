package io.joern.dataflowengineoss.passes.reachingdef

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.edges.ReachingDef
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, PropertyNames}
import io.shiftleft.passes.{DiffGraph, ParallelCpgPass}
import io.shiftleft.semanticcpg.language._
import org.slf4j.{Logger, LoggerFactory}
import overflowdb.traversal.jIteratortoTraversal

import scala.collection.mutable

/** A pass that calculates reaching definitions ("data dependencies").
  */
class ReachingDefPass(cpg: Cpg, maxNumberOfDefinitions: Int = 4000) extends ParallelCpgPass[Method](cpg) {

  private val logger: Logger = LoggerFactory.getLogger(this.getClass)

  override def partIterator: Iterator[Method] = cpg.method.internal.iterator

  override def runOnPart(method: Method): Iterator[DiffGraph] = {
    logger.info("Calculating reaching definitions for: {} in {}", method.fullName, method.filename)
    val problem = ReachingDefProblem.create(method)

    if (shouldBailOut(problem)) {
      logger.warn("Skipping.")
      return Iterator()
    }

    val solution = new DataFlowSolver().calculateMopSolutionForwards(problem)
    val dstGraph = addReachingDefEdges(problem, method, solution)
    addEdgesFromLoneIdentifiersToExit(dstGraph, problem, method, solution)
    Iterator(dstGraph.build())
  }

  /** Before we start propagating definitions in the graph, which is the bulk
    * of the work, we check how many definitions were are dealing with in total.
    * If a threshold is reached, we bail out instead, leaving reaching definitions
    * uncalculated for the method in question. Users can increase the threshold
    * if desired.
    */
  private def shouldBailOut(problem: DataFlowProblem[mutable.BitSet]): Boolean = {
    val method = problem.flowGraph.entryNode.asInstanceOf[Method]
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

  /** Once reaching definitions have been computed, we create a data dependence graph
    * by seeing which of these reaching definitions are relevant in the sense that
    * they are used.
    */
  private def addReachingDefEdges(
      problem: DataFlowProblem[mutable.BitSet],
      method: Method,
      solution: Solution[mutable.BitSet]
  ): DiffGraph.Builder = {
    val numberToNode = problem.flowGraph.asInstanceOf[ReachingDefFlowGraph].numberToNode
    implicit val dstGraph: DiffGraph.Builder = DiffGraph.newBuilder
    val in = solution.in
    val gen = solution.problem.transferFunction
      .asInstanceOf[ReachingDefTransferFunction]
      .gen

    val allNodes = in.keys.toList
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
                  case _ => None
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
          }
        }
      }
    dstGraph
  }

  private def addEdge(fromNode: StoredNode, toNode: StoredNode, variable: String = "")(implicit
      dstGraph: DiffGraph.Builder
  ): Unit = {
    val properties = List((PropertyNames.VARIABLE, variable))
    if (
      fromNode.isInstanceOf[Unknown] || toNode
        .isInstanceOf[Unknown]
    )
      return
    dstGraph.addEdgeInOriginal(fromNode, toNode, EdgeTypes.REACHING_DEF, properties)
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

  /** This is part of the Lone-identifier optimization: as we
    * remove lone identifiers from `gen` sets, we must now
    * retrieve them and create an edge from each lone identifier
    * to the exit node.
    */
  def addEdgesFromLoneIdentifiersToExit(
      builder: DiffGraph.Builder,
      problem: DataFlowProblem[mutable.BitSet],
      method: Method,
      solution: Solution[mutable.BitSet]
  ): Unit = {
    val numberToNode = problem.flowGraph.asInstanceOf[ReachingDefFlowGraph].numberToNode
    implicit val dstGraph: DiffGraph.Builder = builder
    val exitNode = method.methodReturn
    val transferFunction = solution.problem.transferFunction.asInstanceOf[OptimizedReachingDefTransferFunction]
    val genOnce = transferFunction.loneIdentifiers
    genOnce.foreach { case (_, defs) =>
      defs.foreach { d =>
        val dNode = numberToNode(d)
        addEdge(dNode, exitNode, nodeToEdgeLabel(dNode))
      }
    }
  }

}
