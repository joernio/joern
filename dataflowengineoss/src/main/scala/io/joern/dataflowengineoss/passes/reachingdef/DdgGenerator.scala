package io.joern.dataflowengineoss.passes.reachingdef

import io.shiftleft.codepropertygraph.generated.{EdgeTypes, PropertyNames}
import io.shiftleft.codepropertygraph.generated.nodes.{
  Block,
  Call,
  CfgNode,
  ControlStructure,
  FieldIdentifier,
  Identifier,
  JumpTarget,
  Method,
  MethodParameterIn,
  MethodParameterOut,
  MethodReturn,
  Return,
  StoredNode,
  Unknown
}
import overflowdb.BatchedUpdate.DiffGraphBuilder
import io.shiftleft.semanticcpg.language._

import scala.collection.mutable

/** Creation of data dependence edges based on solution of the ReachingDefProblem.
  */
class DdgGenerator {

  /** Once reaching definitions have been computed, we create a data dependence graph by checking which reaching
    * definitions are relevant, meaning that a symbol is propagated that is used by the target node.
    *
    * @param dstGraph
    *   the diff graph to add edges to
    * @param problem
    *   the reaching definition problem
    * @param solution
    *   the solution to `problem``
    */
  def addReachingDefEdges(
    dstGraph: DiffGraphBuilder,
    problem: DataFlowProblem[mutable.BitSet],
    solution: Solution[mutable.BitSet]
  ): Unit = {
    implicit val implicitDst: DiffGraphBuilder = dstGraph
    val numberToNode                           = problem.flowGraph.asInstanceOf[ReachingDefFlowGraph].numberToNode
    val in                                     = solution.in
    val gen = solution.problem.transferFunction
      .asInstanceOf[ReachingDefTransferFunction]
      .gen

    val method        = problem.flowGraph.entryNode.asInstanceOf[Method]
    val allNodes      = in.keys.toList
    val usageAnalyzer = new UsageAnalyzer(problem, in)

    /** Add an edge from the entry node to each node that does not have other incoming definitions.
      */
    def addEdgesFromEntryNode(): Unit = {
      // Add edges from the entry node
      allNodes
        .filter(n => isDdgNode(n) && usageAnalyzer.usedIncomingDefs(n).isEmpty)
        .foreach { node =>
          addEdge(method, node)
        }
    }

    /** Adds incoming edges to arguments of call sites, including edges between arguments of the same call site.
      */
    def addEdgesToCallSite(call: Call): Unit = {
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
      // and the return value. We filter invalid
      // edges at query time (according to the given semantic).
      usageAnalyzer.uses(call).foreach { use =>
        gen(call).foreach { g =>
          val genNode = numberToNode(g)
          if (use != genNode && isDdgNode(use)) {
            addEdge(use, genNode, nodeToEdgeLabel(use))
          }
        }
      }

      // Handle block arguments
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

    def addEdgesToReturn(ret: Return): Unit = {
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
    }

    def addEdgesToMethodParameterOut(paramOut: MethodParameterOut): Unit = {
      // There is always an edge from the method input parameter
      // to the corresponding method output parameter as modifications
      // of the input parameter only affect a copy.
      paramOut.paramIn.foreach { paramIn =>
        addEdge(paramIn, paramOut, paramIn.name)
      }
      usageAnalyzer.usedIncomingDefs(paramOut).foreach { case (_, inElements) =>
        inElements.foreach { inElement =>
          val inElemNode = numberToNode(inElement)
          val edgeLabel  = nodeToEdgeLabel(inElemNode)
          addEdge(inElemNode, paramOut, edgeLabel)
        }
      }
    }

    def addEdgesToExitNode(exitNode: MethodReturn): Unit = {
      in(exitNode).foreach { i =>
        val iNode = numberToNode(i)
        addEdge(iNode, exitNode, nodeToEdgeLabel(iNode))
      }
    }

    /** This is part of the Lone-identifier optimization: as we remove lone identifiers from `gen` sets, we must now
      * retrieve them and create an edge from each lone identifier to the exit node.
      */
    def addEdgesFromLoneIdentifiersToExit(): Unit = {
      val numberToNode     = problem.flowGraph.asInstanceOf[ReachingDefFlowGraph].numberToNode
      val method           = problem.flowGraph.entryNode.asInstanceOf[Method]
      val exitNode         = method.methodReturn
      val transferFunction = solution.problem.transferFunction.asInstanceOf[OptimizedReachingDefTransferFunction]
      val genOnce          = transferFunction.loneIdentifiers
      genOnce.foreach { case (_, defs) =>
        defs.foreach { d =>
          val dNode = numberToNode(d)
          addEdge(dNode, exitNode, nodeToEdgeLabel(dNode))
        }
      }
    }

    addEdgesFromEntryNode()
    allNodes.foreach {
      case call: Call                   => addEdgesToCallSite(call)
      case ret: Return                  => addEdgesToReturn(ret)
      case paramOut: MethodParameterOut => addEdgesToMethodParameterOut(paramOut)
      case _                            =>
    }
    addEdgesToExitNode(method.methodReturn)
    addEdgesFromLoneIdentifiersToExit()
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

  /** There are a few node types that (a) are not to be considered in the DDG, or (b) are not standalone DDG nodes, or
    * (c) have a special meaning in the DDG. This function indicates whether the given node is just a regular DDG node
    * instead.
    */
  private def isDdgNode(x: StoredNode): Boolean = {
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

}
