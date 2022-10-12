package io.joern.dataflowengineoss.passes.reachingdef

import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, PropertyNames}
import io.shiftleft.semanticcpg.language._
import overflowdb.BatchedUpdate.DiffGraphBuilder

import scala.collection.mutable

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

    def incomingDefsForNode(current: StoredNode, usedNode: StoredNode): List[StoredNode] = {
      for {
        i <- in(current).toList
        inNode = numberToNode(i)
        if inNode != usedNode
        if UsageAnalyzer.check(usedNode, inNode)
      } yield inNode
    }

    def addEdgesToUsedNodes(current: StoredNode, usedNodes: List[StoredNode]): Unit = {
      for {
        usedNode <- usedNodes
        inNode   <- incomingDefsForNode(current, usedNode)
      } addEdge(inNode, usedNode, edgeLabel(inNode))
    }

    def addEdgesFromEntryNode(): Unit = {
      // Add edges from the entry node
      for {
        node <- allNodes
        if isDdgNode(node) && incomingDefsForNode(node, node).isEmpty
      } addEdge(method, node)
    }

    /** Adds incoming edges to arguments of call sites, including edges between arguments of the same call site.
      */
    def addEdgesToCallSite(call: Call): Unit = {
      // Edges between arguments of call sites
      val usedNodesOfCall = call.argument.toList
      addEdgesToUsedNodes(call, usedNodesOfCall)

      // For all calls, assume that input arguments
      // taint corresponding output arguments
      // and the return value. We filter invalid
      // edges at query time (according to the given semantic).
      for {
        use <- call.argument.toList
        g   <- gen(call).toList
        genNode = numberToNode(g)
        if use != genNode && isDdgNode(use)
      } addEdge(use, genNode, edgeLabel(use))

      // Handle block arguments
      // This handles `foo(new Bar())`, which is lowered to
      // `foo({Bar tmp = Bar.alloc(); tmp.init(); tmp})`
      for {
        block <- call.argument.isBlock
        last  <- block.astChildren.lastOption
        if last.isInstanceOf[Expression]
      } {
        val usedNodesOfLast = List(last)
        addEdgesToUsedNodes(last, usedNodesOfLast)
        addEdge(last, block, edgeLabel(last))
      }

    }

    // example code: {a = 1; b = fn(a); return b}
    // step1 used incoming node --> b
    // step2 b                  --> Return
    // step3 Return             --> MethodReturn
    def addEdgesToReturn(ret: Return): Unit = {
      val usedNodesOfRet = ret.astChildren.collectAll[Expression].toList
      // step1: link used incoming defs to expression
      addEdgesToUsedNodes(ret, usedNodesOfRet)
      // step2: link returned expressions to Return node
      usedNodesOfRet.foreach(use => addEdge(use, ret, edgeLabel(use)))
      // step3: link Return node to MethodReturn node
      addEdge(ret, method.methodReturn, "<RET>")
    }

    def addEdgesToMethodParameterOut(paramOut: MethodParameterOut): Unit = {
      // There is always an edge from the method input parameter
      // to the corresponding method output parameter as modifications
      // of the input parameter only affect a copy.
      paramOut.paramIn.foreach { paramIn =>
        addEdge(paramIn, paramOut, paramIn.name)
      }
      val usedNodesOfParamOut = List(paramOut)
      addEdgesToUsedNodes(paramOut, usedNodesOfParamOut)
    }

    def addEdgesToExitNode(exitNode: MethodReturn): Unit = {
      in(exitNode).foreach { i =>
        val iNode = numberToNode(i)
        addEdge(iNode, exitNode, edgeLabel(iNode))
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
          addEdge(dNode, exitNode, edgeLabel(dNode))
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

  private def edgeLabel(node: StoredNode): String = {
    node match {
      case n: MethodParameterIn => n.name
      case n: Identifier        => n.name
      case n: CfgNode           => n.code
      case _                    => ""
    }
  }
}
