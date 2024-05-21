package io.joern.dataflowengineoss.passes.reachingdef

import io.joern.dataflowengineoss.identifierToFirstUsages
import io.joern.dataflowengineoss.queryengine.AccessPathUsage.toTrackedBaseAndAccessPathSimple
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, Operators, PropertyNames}
import io.shiftleft.semanticcpg.accesspath.MatchResult
import io.shiftleft.semanticcpg.language.*
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
    method: Method,
    problem: DataFlowProblem[StoredNode, mutable.BitSet],
    solution: Solution[StoredNode, mutable.BitSet]
  ): Unit = {
    implicit val implicitDst: DiffGraphBuilder = dstGraph

    val numberToNode = problem.flowGraph.asInstanceOf[ReachingDefFlowGraph].numberToNode
    val in           = solution.in
    val gen          = solution.problem.transferFunction.asInstanceOf[ReachingDefTransferFunction].gen

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

    // This handles `foo(new Bar()) or return new Bar()`
    def addEdgeForBlock(block: Block, towards: StoredNode): Unit = {
      block.astChildren.lastOption match {
        case None => // Do nothing
        case Some(node: Identifier) =>
          val edgesToAdd = in(node).toList
            .flatMap(numberToNode.get)
            .filter(inDef => usageAnalyzer.isUsing(node, inDef))
            .collect {
              case identifier: Identifier => identifier
              case call: Call             => call
            }
          edgesToAdd.foreach { inNode =>
            addEdge(inNode, block, nodeToEdgeLabel(inNode))
          }
          if (edgesToAdd.nonEmpty) {
            addEdge(block, towards)
          }
        case Some(node: Call) =>
          addEdge(node, block, nodeToEdgeLabel(node))
          addEdge(block, towards)
        case _ => // Do nothing
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

      // This handles `foo(new Bar())`, which is lowered to
      // `foo({Bar tmp = Bar.alloc(); tmp.init(); tmp})`
      call.argument.isBlock.foreach { block => addEdgeForBlock(block, call) }
    }

    def addEdgesToReturn(ret: Return): Unit = {
      // This handles `return new Bar()`, which is lowered to
      // `return {Bar tmp = Bar.alloc(); tmp.init(); tmp}`
      usageAnalyzer.uses(ret).collectAll[Block].foreach(block => addEdgeForBlock(block, ret))
      usageAnalyzer.usedIncomingDefs(ret).foreach { case (use: CfgNode, inElements) =>
        addEdge(use, ret, use.code)
        inElements
          .filterNot(x => numberToNode.get(x).contains(use))
          .flatMap(numberToNode.get)
          .foreach { inElemNode =>
            addEdge(inElemNode, use, nodeToEdgeLabel(inElemNode))
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
    def addEdgesFromLoneIdentifiersToExit(method: Method): Unit = {
      val numberToNode     = problem.flowGraph.asInstanceOf[ReachingDefFlowGraph].numberToNode
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

    def addEdgesToCapturedIdentifiersAndParameters(): Unit = {
      val identifierDestPairs =
        method._identifierViaContainsOut
          .flatMap { identifier =>
            identifierToFirstUsages(identifier).map(usage => (identifier, usage))
          }
          .l
          .distinctBy(_._2.method)

      identifierDestPairs
        .foreach { case (src, dst) =>
          addEdge(src, dst, nodeToEdgeLabel(src))
        }
      method.parameter.foreach { param =>
        param.capturedByMethodRef.referencedMethod.ast.isIdentifier.foreach { identifier =>
          addEdge(param, identifier, nodeToEdgeLabel(param))
        }
      }

      // NOTE: Below connects REACHING_DEF edges between method boundaries of closures. In the case of PARENT -> CHILD
      // this brings no inconsistent flows, but from CHILD -> PARENT we have observed inconsistencies. This form of
      // modelling data-flow is unsound as the engine assumes REACHING_DEF edges are intraprocedural.
      // See PR #3735 on Joern for details
      val globalIdentifiers =
        (method._callViaContainsOut ++ method._returnViaContainsOut).ast.isLiteral
          .flatMap(_.start.where(_.method.isMethod).inAssignment.argument(1))
          .collectAll[Identifier]
          .l
      globalIdentifiers
        .foreach { global =>
          identifierToFirstUsages(global).map { identifier =>
            addEdge(global, identifier, nodeToEdgeLabel(global))
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

    addEdgesToCapturedIdentifiersAndParameters()
    addEdgesToExitNode(method.methodReturn)
    addEdgesFromLoneIdentifiersToExit(method)
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

/** Upon calculating reaching definitions, we find ourselves with a set of incoming definitions `in(n)` for each node
  * `n` of the flow graph. This component determines those of the incoming definitions that are relevant as the value
  * they define is actually used by `n`.
  */
private class UsageAnalyzer(
  problem: DataFlowProblem[StoredNode, mutable.BitSet],
  in: Map[StoredNode, Set[Definition]]
) {

  val numberToNode: Map[Definition, StoredNode] = problem.flowGraph.asInstanceOf[ReachingDefFlowGraph].numberToNode

  private val allNodes = in.keys.toList
  private val containerSet =
    Set(Operators.fieldAccess, Operators.indexAccess, Operators.indirectIndexAccess, Operators.indirectFieldAccess)
  private val indirectionAccessSet = Set(Operators.addressOf, Operators.indirection)
  val usedIncomingDefs: Map[StoredNode, Map[StoredNode, Set[Definition]]] = initUsedIncomingDefs()

  def initUsedIncomingDefs(): Map[StoredNode, Map[StoredNode, Set[Definition]]] = {
    allNodes.map { node =>
      node -> usedIncomingDefsForNode(node)
    }.toMap
  }

  private def usedIncomingDefsForNode(node: StoredNode): Map[StoredNode, Set[Definition]] = {
    uses(node).map { use =>
      use -> in(node).filter { inElement =>
        val inElemNode = numberToNode(inElement)
        isUsing(use, inElemNode)
      }
    }.toMap
  }

  def isUsing(use: StoredNode, inElemNode: StoredNode): Boolean =
    sameVariable(use, inElemNode) || isContainer(use, inElemNode) || isPart(use, inElemNode) || isAlias(use, inElemNode)

  /** Determine whether the node `use` describes a container for `inElement`, e.g., use = `ptr` while inElement =
    * `ptr->foo`.
    */
  private def isContainer(use: StoredNode, inElement: StoredNode): Boolean = {
    inElement match {
      case call: Call if containerSet.contains(call.name) =>
        call.argument.headOption.exists { base =>
          nodeToString(use) == nodeToString(base)
        }
      case _ => false
    }
  }

  /** Determine whether `use` is a part of `inElement`, e.g., use = `argv[0]` while inElement = `argv`
    */
  private def isPart(use: StoredNode, inElement: StoredNode): Boolean = {
    use match {
      case call: Call if containerSet.contains(call.name) =>
        inElement match {
          case param: MethodParameterIn =>
            call.argument.headOption.exists { base =>
              nodeToString(base).contains(param.name)
            }
          case identifier: Identifier =>
            call.argument.headOption.exists { base =>
              nodeToString(base).contains(identifier.name)
            }
          case _ => false
        }
      case _ => false
    }
  }

  private def isAlias(use: StoredNode, inElement: StoredNode): Boolean = {
    use match {
      case useCall: Call =>
        inElement match {
          case inCall: Call =>
            val (useBase, useAccessPath) = toTrackedBaseAndAccessPathSimple(useCall)
            val (inBase, inAccessPath)   = toTrackedBaseAndAccessPathSimple(inCall)
            useBase == inBase && useAccessPath.matchAndDiff(inAccessPath.elements)._1 == MatchResult.EXACT_MATCH
          case _ => false
        }
      case _ => false
    }
  }

  def uses(node: StoredNode): Set[StoredNode] = {
    val n: Set[StoredNode] = node match {
      case ret: Return                  => ret.astChildren.collect { case x: Expression => x }.toSet
      case call: Call                   => call.argument.toSet
      case paramOut: MethodParameterOut => Set(paramOut)
      case _                            => Set()
    }
    n.filterNot(_.isInstanceOf[FieldIdentifier])
  }

  /** Compares arguments of calls with incoming definitions to see if they refer to the same variable
    */
  private def sameVariable(use: StoredNode, inElement: StoredNode): Boolean = {
    inElement match {
      case param: MethodParameterIn =>
        nodeToString(use).contains(param.name)
      case call: Call if indirectionAccessSet.contains(call.name) =>
        call.argumentOption(1).exists(x => nodeToString(use).contains(x.code))
      case call: Call =>
        nodeToString(use).contains(call.code)
      case identifier: Identifier => nodeToString(use).contains(identifier.name)
      case _                      => false
    }
  }

  private def nodeToString(node: StoredNode): Option[String] = {
    node match {
      case ident: Identifier     => Some(ident.name)
      case exp: Expression       => Some(exp.code)
      case p: MethodParameterIn  => Some(p.name)
      case p: MethodParameterOut => Some(p.name)
      case _                     => None
    }
  }

}
