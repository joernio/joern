package io.joern.dataflowengineoss.passes.reachingdef

import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, PropertyNames}
import io.shiftleft.semanticcpg.language._
import overflowdb.BatchedUpdate.DiffGraphBuilder

import scala.collection.{Set, mutable}

/** Creation of data dependence edges based on solution of the ReachingDefProblem.
  */
class DdgGenerator(semantics: Semantics) {

  implicit val s: Semantics = semantics

  /** Once reaching definitions have been computed, we create a data dependence graph by checking which reaching
    * definitions are relevant, meaning that a symbol is propagated that is used by the target node.
    *
    * @param dstGraph
    *   the diff graph to add edges to
    * @param problem
    *   the reaching definition problem
    * @param solution
    *   the solution to `problem`
    */
  def addReachingDefEdges(
    dstGraph: DiffGraphBuilder,
    problem: DataFlowProblem[StoredNode, mutable.BitSet],
    solution: Solution[StoredNode, mutable.BitSet]
  ): Unit = {

    implicit val implicitDst: DiffGraphBuilder = dstGraph

    val numberToNode = problem.flowGraph.asInstanceOf[ReachingDefFlowGraph].numberToNode
    val in           = solution.in
    val gen          = solution.problem.transferFunction.asInstanceOf[ReachingDefTransferFunction].gen

    val method   = problem.flowGraph.entryNode.asInstanceOf[Method]
    val allNodes = in.keys.toList

    def addEdgesFromEntryNode(): Unit = {
      // Add edges from the entry node
      for {
        node <- allNodes
        if isDdgNode(node) && incomingDefsForNode(node).isEmpty
      } addEdge(method, node)
    }

    def incomingDefsForNode(node: StoredNode): Set[StoredNode] = {
      in(node).toSet
        .map(numberToNode)
        .filter { inNode =>
          UsageAnalyzer.sameVariable(node, inNode) ||
          UsageAnalyzer.isContainer(node, inNode) ||
          UsageAnalyzer.isPart(node, inNode) ||
          UsageAnalyzer.isAlias(node, inNode)
        }
    }

    def addEdgesToNode(node: StoredNode): Unit = {
      for {
        inNode <- incomingDefsForNode(node)
        if inNode != node
      } addEdge(inNode, node, nodeToEdgeLabel(inNode))
    }

    /** Adds incoming edges to arguments of call sites, including edges between arguments of the same call site.
      */
    def addEdgesToCallSite(call: Call): Unit = {
      // Edges between arguments of call sites
      call.argument.foreach(addEdgesToNode)

      // For all calls, assume that input arguments
      // taint corresponding output arguments
      // and the return value. We filter invalid
      // edges at query time (according to the given semantic).
      for {
        use <- call.argument.toList
        g   <- gen(call).toList
        genNode = numberToNode(g)
        if use != genNode && isDdgNode(use)
      } addEdge(use, genNode, nodeToEdgeLabel(use))

      // Handle block arguments
      // This handles `foo(new Bar())`, which is lowered to
      // `foo({Bar tmp = Bar.alloc(); tmp.init(); tmp})`
      for {
        block <- call.argument.isBlock
        last  <- block.astChildren.lastOption
      } {
        addEdgesToNode(last)
        addEdge(last, block, nodeToEdgeLabel(last))
        addEdge(block, call)
      }

    }

    def addEdgesToReturn(ret: Return): Unit = {
      val uses = ret.astChildren.collect { case x: Expression => x }.toList
      uses.foreach(use => addEdge(use, ret, use.code))
      uses.foreach(addEdgesToNode)
      addEdge(ret, method.methodReturn, "<RET>")
    }

    def addEdgesToMethodParameterOut(paramOut: MethodParameterOut): Unit = {
      // There is always an edge from the method input parameter
      // to the corresponding method output parameter as modifications
      // of the input parameter only affect a copy.
      paramOut.paramIn.foreach { paramIn =>
        addEdge(paramIn, paramOut, paramIn.name)
      }
      addEdgesToNode(paramOut)
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
    if (fromNode.isInstanceOf[Unknown] || toNode.isInstanceOf[Unknown])
      return

    (fromNode, toNode) match {
      case (parentNode: CfgNode, childNode: CfgNode) if EdgeValidator.isValidEdge(childNode, parentNode) =>
        dstGraph.addEdge(fromNode, toNode, EdgeTypes.REACHING_DEF, PropertyNames.VARIABLE, variable)
      case _ =>

    }
  }

  /** There are a few node types that (a) are not to be considered in the DDG, or (b) are not standalone DDG nodes, or
    * (c) have a special meaning in the DDG. This function indicates whether the given node is just a regular DDG node
    * instead.
    */
  private def isDdgNode(x: StoredNode): Boolean = {
    x match {
      case _: Method           => false
      case _: ControlStructure => false
      case _: FieldIdentifier  => false
      case _: JumpTarget       => false
      case _: MethodReturn     => false
      case _                   => true
    }
  }

  private def nodeToEdgeLabel(node: StoredNode): String = {
    node match {
      case n: MethodParameterIn => n.name
      case n: CfgNode           => n.code
      case _                    => ""
    }
  }
}
