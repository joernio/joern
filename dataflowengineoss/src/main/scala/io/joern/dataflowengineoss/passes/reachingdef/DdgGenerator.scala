package io.joern.dataflowengineoss.passes.reachingdef

import io.joern.dataflowengineoss.{globalFromLiteral, identifierToFirstUsages}
import io.joern.dataflowengineoss.queryengine.AccessPathUsage.toTrackedBaseAndAccessPathSimple
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, Operators}
import io.shiftleft.semanticcpg.accesspath.{
  AccessElement,
  AccessPath,
  AddressOf,
  Elements,
  IndirectionAccess,
  MatchResult,
  PointerShift,
  TrackedBase,
  VariablePointerShift
}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.codepropertygraph.generated.DiffGraphBuilder

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
    problem: DataFlowProblem[CfgNode, mutable.BitSet],
    solution: Solution[CfgNode, mutable.BitSet]
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
    def addEdgeForBlock(block: Block, towards: CfgNode): Unit = {
      block.astChildren.lastOption match {
        case None                   => // Do nothing
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
      paramOut.start.paramIn.foreach { paramIn =>
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
        method._identifierViaContainsOut.flatMap { identifier =>
          identifierToFirstUsages(identifier).map(usage => (identifier, usage))
        }.l

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
          .flatMap(globalFromLiteral(_))
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
    addAddressDerefEdges(method)
  }

  /** Intraprocedural only: one address-of operand per Local/param; no heap or interprocedural tracking. */
  private def addAddressDerefEdges(method: Method)(implicit dstGraph: DiffGraphBuilder): Unit = {
    val addressOfMap: Map[Long, CfgNode] = method.ast.isCall
      .nameExact(Operators.assignment)
      .flatMap { assign =>
        val lhs = assign.argumentOption(1).collect { case id: Identifier => id }
        val rhs = assign.argumentOption(2).collect { case c: Call if c.name == Operators.addressOf => c }
        for {
          id      <- lhs
          addr    <- rhs
          decl    <- id.refsTo.collect { case local: Local => local; case param: MethodParameterIn => param }.headOption
          operand <- addr.argumentOption(1).collect {
            case operand: Identifier                                                                   => operand
            case c: Call if c.name == Operators.indirectIndexAccess || c.name == Operators.indexAccess => c
          }
        } yield decl.id -> operand
      }
      .groupBy(_._1)
      .collect { case (declId, pairs) if pairs.size == 1 => declId -> pairs.head._2 }
      .toMap

    // Design note: one RHS per Local/param only; reassigned locals drop the alias (same ceiling as addressOfMap).
    val pointerRhsMap: Map[Long, CfgNode] = method.ast.isCall
      .nameExact(Operators.assignment)
      .flatMap { assign =>
        for {
          id   <- assign.argumentOption(1).collect { case id: Identifier => id }
          rhs  <- assign.argumentOption(2).collect { case call: Call if call.name == Operators.cast => call: CfgNode }
          decl <- id.refsTo.collect { case local: Local => local; case param: MethodParameterIn => param }.headOption
        } yield decl.id -> rhs
      }
      .groupBy(_._1)
      .collect { case (declId, pairs) if pairs.size == 1 => declId -> pairs.head._2 }
      .toMap

    if (addressOfMap.isEmpty && pointerRhsMap.isEmpty)
      return

    // Only matches `*identifier` where identifier refs a Local or MethodParameterIn.
    // Skips complex nesting like `(*p)`, `*&x`, or `*f()`.
    method.ast.isCall.nameExact(Operators.indirection).foreach { derefCall =>
      derefCall.argumentOption(1).collect { case id: Identifier => id }.foreach { id =>
        id.refsTo
          .collect { case local: Local => local; case param: MethodParameterIn => param }
          .headOption
          .foreach { decl =>
            addressOfMap.get(decl.id).foreach { sourceNode =>
              addEdge(sourceNode, derefCall, nodeToEdgeLabel(sourceNode))
            }
            pointerRhsMap.get(decl.id).foreach { sourceNode =>
              addEdge(sourceNode, derefCall, nodeToEdgeLabel(sourceNode))
            }
          }
      }
    }
  }

  private def addEdge(fromNode: CfgNode, toNode: CfgNode, variable: String = "")(implicit
    dstGraph: DiffGraphBuilder
  ): Unit = {
    if (fromNode.isInstanceOf[Unknown] || toNode.isInstanceOf[Unknown])
      return

    (fromNode, toNode) match {
      case (parentNode: CfgNode, childNode: CfgNode) if EdgeValidator.isValidEdge(childNode, parentNode) =>
        dstGraph.addEdge(fromNode, toNode, EdgeTypes.REACHING_DEF, variable)
      case _ =>

    }
  }

  /** There are a few node types that (a) are not to be considered in the DDG, or (b) are not standalone DDG nodes, or
    * (c) have a special meaning in the DDG. This function indicates whether the given node is just a regular DDG node
    * instead.
    */
  private def isDdgNode(x: CfgNode): Boolean = {
    x match {
      case _: Method           => false
      case _: ControlStructure => false
      case _: FieldIdentifier  => false
      case _: JumpTarget       => false
      case _: MethodReturn     => false
      case _                   => true
    }
  }

  private def nodeToEdgeLabel(node: CfgNode): String = {
    node match {
      case n: MethodParameterIn => n.name
      case n: CfgNode           => n.code
    }
  }
}

/** Intraprocedural `p = &s` aliases, keyed by local or parameter id so shadowed names stay distinct.
  *
  * A pointer that is assigned more than once keeps its alias only when every assignment takes the address of the same
  * variable. Assignment to a different target drops the alias.
  *
  * Flow-insensitive and no pointer arithmetic: conservative over-approximation that keeps the engine fast and sound for
  * the common case; deep alias chains are out of scope. A `p->field` write before `p = &s` in program order is still
  * treated as a write through that alias.
  *
  * Index-access aliasing is not tracked.
  */
private object PointerAliasMap {

  private val NotAnAddress = -1L

  private val memberAccessOps =
    Set(Operators.fieldAccess, Operators.indirectFieldAccess, Operators.indexAccess, Operators.indirectIndexAccess)

  def fromMethod(method: Method): Map[Long, Long] = {
    val targetsByPointer = mutable.Map.empty[Long, mutable.Set[Long]]
    method.call
      .nameExact(Operators.assignment)
      .foreach { assign =>
        declarationId(assign).foreach { pointerDecl =>
          val targetId = addressTarget(assign).getOrElse(NotAnAddress)
          targetsByPointer.getOrElseUpdate(pointerDecl, mutable.Set.empty) += targetId
        }
      }

    targetsByPointer.collect {
      case (pointerDecl, targets) if targets.size == 1 && !targets.contains(NotAnAddress) =>
        pointerDecl -> targets.head
    }.toMap
  }

  /** Local or parameter this identifier refers to. Empty when the reference is unresolved. */
  def declarationId(ident: Identifier): Option[Long] =
    ident.refsTo.collect { case local: Local => local.id; case param: MethodParameterIn => param.id }.headOption

  private def declarationId(assign: Call): Option[Long] =
    assign.argumentOption(1).collect { case ident: Identifier => declarationId(ident) }.flatten

  private def addressTarget(assign: Call): Option[Long] = {
    val rhs = assign.argumentOption(2)
    rhs.flatMap {
      case addrOf: Call if addrOf.name == Operators.addressOf =>
        addressOfOperand(addrOf)
      case cast: Call if cast.name == Operators.cast =>
        cast.argumentOption(1).flatMap {
          case addrOf: Call if addrOf.name == Operators.addressOf => addressOfOperand(addrOf)
          case ident: Identifier                                  => declarationId(ident)
          case _                                                  => None
        }
      case _ => None
    }
  }

  private def addressOfOperand(addrOf: Call): Option[Long] =
    addrOf.argumentOption(1).flatMap {
      case ident: Identifier                                              => declarationId(ident)
      case fieldAccess: Call if fieldAccess.name == Operators.fieldAccess =>
        rootIdentifier(fieldAccess).flatMap(declarationId)
      case _ => None
    }

  /** Walk member/index access bases until an identifier.
    *
    * Depth-4 cutoff: bounds traversal so a cyclic argument graph cannot loop forever. A deeper access chain returns
    * None and the caller falls back to the name-based check.
    */
  def rootIdentifier(node: CfgNode, depth: Int = 4): Option[Identifier] =
    if (depth < 0) None
    else
      node match {
        case ident: Identifier                                 => Some(ident)
        case call: Call if memberAccessOps.contains(call.name) =>
          call.argumentOption(1).collect { case next: CfgNode => next }.flatMap(rootIdentifier(_, depth - 1))
        case _ => None
      }

  def rootDeclarationId(node: CfgNode, depth: Int = 4): Option[Long] =
    rootIdentifier(node, depth).flatMap(declarationId)

}

/** Upon calculating reaching definitions, we find ourselves with a set of incoming definitions `in(n)` for each node
  * `n` of the flow graph. This component determines those of the incoming definitions that are relevant as the value
  * they define is actually used by `n`.
  */
private[reachingdef] class UsageAnalyzer(
  problem: DataFlowProblem[CfgNode, mutable.BitSet],
  in: Map[CfgNode, Set[Definition]]
) {

  val numberToNode: Map[Definition, CfgNode] = problem.flowGraph.asInstanceOf[ReachingDefFlowGraph].numberToNode

  private val method         = problem.flowGraph.asInstanceOf[ReachingDefFlowGraph].method
  private val pointerAliases = PointerAliasMap.fromMethod(method)

  private val rootDeclCache: mutable.Map[CfgNode, Option[Long]] = mutable.HashMap.empty

  private def rootDeclOf(node: CfgNode): Option[Long] =
    rootDeclCache.getOrElseUpdate(node, PointerAliasMap.rootDeclarationId(node))

  private val allNodes     = in.keys.toList
  private val containerSet =
    Set(Operators.fieldAccess, Operators.indexAccess, Operators.indirectIndexAccess, Operators.indirectFieldAccess)
  private val indirectionAccessSet = Set(Operators.addressOf, Operators.indirection)

  /** `isUsing` matches a use with a definition under any one of five conditions, and each of them requires the two to
    * share a key. These maps index the definitions by those keys, so that a use is tested against the definitions that
    * share at least one key with it.
    */
  private val definitionsByString    = mutable.HashMap.empty[String, mutable.ArrayBuffer[Definition]]
  private val containersByBaseString = mutable.HashMap.empty[Option[String], mutable.ArrayBuffer[Definition]]
  private val definitionsByName      = mutable.HashMap.empty[String, mutable.ArrayBuffer[Definition]]
  private val callsByAliasKey        =
    mutable.HashMap.empty[(TrackedBase, Seq[AccessElement]), mutable.ArrayBuffer[Definition]]
  private val callsByRootDeclaration = mutable.HashMap.empty[Long, mutable.ArrayBuffer[Definition]]
  private val pointersTo             = pointerAliases.groupMap(_._2)(_._1)

  numberToNode.toSeq.sortBy(_._1).foreach { case (definition, node) =>
    def add[K](index: mutable.HashMap[K, mutable.ArrayBuffer[Definition]], key: K): Unit =
      index.getOrElseUpdate(key, mutable.ArrayBuffer.empty) += definition
    node match {
      case param: MethodParameterIn =>
        add(definitionsByString, param.name)
        add(definitionsByName, param.name)
      case identifier: Identifier =>
        add(definitionsByString, identifier.name)
        add(definitionsByName, identifier.name)
      case call: Call =>
        if (indirectionAccessSet.contains(call.name)) {
          call.argumentOption(1).foreach(argument => add(definitionsByString, argument.code))
        } else {
          add(definitionsByString, call.code)
        }
        if (containerSet.contains(call.name)) {
          call.argument.headOption.foreach(base => add(containersByBaseString, nodeToString(base)))
        }
        add(callsByAliasKey, aliasKey(call))
        if (pointerAliases.nonEmpty) rootDeclOf(call).foreach(add(callsByRootDeclaration, _))
      case _ =>
    }
  }

  val usedIncomingDefs: Map[CfgNode, Map[CfgNode, Set[Definition]]] = initUsedIncomingDefs()

  def initUsedIncomingDefs(): Map[CfgNode, Map[CfgNode, Set[Definition]]] = {
    allNodes.map { node =>
      node -> usedIncomingDefsForNode(node)
    }.toMap
  }

  private def usedIncomingDefsForNode(node: CfgNode): Map[CfgNode, Set[Definition]] = {
    val incoming          = in(node)
    lazy val incomingSize = incoming.size
    uses(node).map { use =>
      val candidates = candidatesFor(use)
      use -> {
        if (candidates.length > incomingSize) {
          incoming.filter { inElement =>
            val inElemNode = numberToNode(inElement)
            isUsing(use, inElemNode)
          }
        } else {
          usedAmong(candidates, use, incoming)
        }
      }
    }.toMap
  }

  /** The definitions of `incoming` that `use` uses, found among its candidates. */
  private[reachingdef] def usedByIndex(use: CfgNode, incoming: Set[Definition]): Set[Definition] =
    usedAmong(candidatesFor(use), use, incoming)

  private def usedAmong(candidates: Array[Definition], use: CfgNode, incoming: Set[Definition]): Set[Definition] = {
    val used = mutable.BitSet()
    candidates.foreach { candidate =>
      if (incoming.contains(candidate) && isUsing(use, numberToNode(candidate))) used.add(candidate)
    }
    used
  }

  /** The definitions that share a key with `use` (see [[definitionsByString]]), with repeats. */
  private def candidatesFor(use: CfgNode): Array[Definition] = {
    val useString = nodeToString(use)
    val parts     = use match {
      case call: Call if containerSet.contains(call.name) =>
        containerBaseForMatching(call).flatMap(nodeToString).flatMap(definitionsByName.get)
      case _ => None
    }
    val aliases = use match {
      case call: Call => callsByAliasKey.get(aliasKey(call)) ++ acrossPointer(call)
      case _          => None
    }
    val sameOrContainer = useString.flatMap(definitionsByString.get) ++ containersByBaseString.get(useString)
    (sameOrContainer ++ parts ++ aliases).flatten.toArray
  }

  /** Returns the field and index accesses rooted at a variable that `call`'s root aliases through a pointer. */
  private def acrossPointer(call: Call): Iterable[mutable.ArrayBuffer[Definition]] =
    rootDeclOf(call).toList.flatMap { declaration =>
      (pointerAliases.get(declaration) ++ pointersTo.getOrElse(declaration, Nil)).flatMap(callsByRootDeclaration.get)
    }

  /** Returns `call`'s tracked base and its access path without the trailing address-of and pointer shifts. */
  private def aliasKey(call: Call): (TrackedBase, Seq[AccessElement]) = {
    val (base, accessPath) = toTrackedBaseAndAccessPathSimple(call)
    (base, AliasKeys.head(accessPath.elements))
  }

  def isUsing(use: CfgNode, inElemNode: CfgNode): Boolean =
    sameVariable(use, inElemNode) || isContainer(use, inElemNode) || isPart(use, inElemNode) || isAlias(use, inElemNode)

  /** Base for container matching; peel `info->attrs` under `info->attrs[i]` only. */
  private def containerBaseForMatching(container: Call): Option[CfgNode] =
    (container.name, container.argument.headOption) match {
      case (Operators.indirectIndexAccess, Some(inner: Call)) if inner.name == Operators.indirectFieldAccess =>
        inner.argument.headOption.collect { case node: CfgNode => node }
      case (_, maybeBase) => maybeBase.collect { case node: CfgNode => node }
    }

  /** Determine whether the node `use` describes a container for `inElement`, e.g., use = `ptr` while inElement =
    * `ptr->foo`.
    */
  private def isContainer(use: CfgNode, inElement: CfgNode): Boolean = {
    inElement match {
      case call: Call if containerSet.contains(call.name) =>
        // Keep upstream base (e.g. `c->buf` for `c->buf[idx]`) so struct params are not containers for field writes.
        call.argument.headOption.exists { base =>
          sameRoot(use, base) && nodeToString(use) == nodeToString(base)
        }
      case _ => false
    }
  }

  /** Determine whether `use` is a part of `inElement`, e.g., use = `argv[0]` while inElement = `argv`
    */
  private def isPart(use: CfgNode, inElement: CfgNode): Boolean = {
    use match {
      case call: Call if containerSet.contains(call.name) =>
        inElement match {
          case param: MethodParameterIn =>
            containerBaseForMatching(call).exists { base =>
              sameRoot(base, param) && nodeToString(base).contains(param.name)
            }
          case identifier: Identifier =>
            containerBaseForMatching(call).exists { base =>
              sameRoot(base, identifier) && nodeToString(base).contains(identifier.name)
            }
          case _ => false
        }
      case _ => false
    }
  }

  private def isAlias(use: CfgNode, inElement: CfgNode): Boolean = {
    use match {
      case useCall: Call =>
        inElement match {
          case inCall: Call =>
            val (useBase, useAccessPath) = toTrackedBaseAndAccessPathSimple(useCall)
            val (inBase, inAccessPath)   = toTrackedBaseAndAccessPathSimple(inCall)
            if (useBase == inBase && useAccessPath.matchAndDiff(inAccessPath.elements)._1 == MatchResult.EXACT_MATCH) {
              sameRoot(useCall, inCall)
            } else {
              isFieldAliasAcrossPointer(useCall, useAccessPath, inCall, inAccessPath)
            }
          case _ => false
        }
      case _ => false
    }
  }

  /** Reject only when both sides resolve to distinct declarations; unresolved sides keep lenient name-based matching.
    */
  private def sameRoot(use: CfgNode, defined: CfgNode): Boolean =
    (rootDeclOf(use), rootDeclOf(defined)) match {
      case (Some(useDecl), Some(definedDecl)) if useDecl != definedDecl => false
      case _                                                            => true
    }

  private def isFieldAliasAcrossPointer(
    useCall: Call,
    useAccessPath: AccessPath,
    inCall: Call,
    inAccessPath: AccessPath
  ): Boolean = {
    if (pointerAliases.isEmpty)
      return false
    (rootDeclOf(useCall), rootDeclOf(inCall)) match {
      case (Some(useDecl), Some(inDecl)) if pointerAliases.get(inDecl).contains(useDecl) =>
        fieldPathsMatchAfterOneDeref(inAccessPath, useAccessPath)
      case (Some(useDecl), Some(inDecl)) if pointerAliases.get(useDecl).contains(inDecl) =>
        fieldPathsMatchAfterOneDeref(useAccessPath, inAccessPath)
      case _ => false
    }
  }

  /** Peel one leading `IndirectionAccess`. Keep the pointer path's exclusions; an empty exclusion list would drop them.
    */
  private def fieldPathsMatchAfterOneDeref(pointerPath: AccessPath, structPath: AccessPath): Boolean =
    pointerPath.elements.elements.toList match {
      case IndirectionAccess :: tail =>
        AccessPath(Elements.normalized(tail), pointerPath.exclusions)
          .matchAndDiff(structPath.elements)
          ._1 == MatchResult.EXACT_MATCH
      case _ => false
    }

  def uses(node: CfgNode): Set[CfgNode] = {
    val n: Set[CfgNode] = node match {
      case ret: Return                  => ret.astChildren.collect { case x: Expression => x }.toSet
      case call: Call                   => call.argument.toSet
      case paramOut: MethodParameterOut => Set(paramOut)
      case _                            => Set()
    }
    n.filterNot(_.isInstanceOf[FieldIdentifier])
  }

  /** Compares arguments of calls with incoming definitions to see if they refer to the same variable
    */
  private def sameVariable(use: CfgNode, inElement: CfgNode): Boolean = {
    inElement match {
      case param: MethodParameterIn =>
        nodeToString(use).contains(param.name)
      case call: Call if indirectionAccessSet.contains(call.name) =>
        call.argumentOption(1).exists(arg => nodeToString(use).contains(arg.code)) && sameRoot(use, call)
      case call: Call =>
        nodeToString(use).contains(call.code) && sameRoot(use, call)
      case identifier: Identifier =>
        nodeToString(use).contains(identifier.name) && sameRoot(use, identifier)
      case _ => false
    }
  }

  private def nodeToString(node: CfgNode): Option[String] = {
    node match {
      case ident: Identifier     => Some(ident.name)
      case exp: Expression       => Some(exp.code)
      case p: MethodParameterIn  => Some(p.name)
      case p: MethodParameterOut => Some(p.name)
      case _                     => None
    }
  }

}

private[reachingdef] object AliasKeys {

  /** Returns `elements` without its trailing address-of and pointer shifts. */
  def head(elements: Elements): Seq[AccessElement] = {
    val all = elements.elements
    all.take(all.lastIndexWhere(element => !isInvertible(element)) + 1).toSeq
  }

  private def isInvertible(element: AccessElement): Boolean = element match {
    case AddressOf | VariablePointerShift | _: PointerShift => true
    case _                                                  => false
  }
}
