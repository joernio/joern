package io.joern.dataflowengineoss.passes.reachingdef

import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, Operators}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.utils.MemberAccess.{isFieldAccess, isGenericMemberAccessName}
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.{Set, mutable}

/** The variables defined/used in the reaching def problem can all be represented via nodes in the graph, however,
  * that's pretty confusing because it is then unclear that variables and nodes are actually two separate domains. To
  * make the definition domain visible, we use the type alias `Definition`. From a computational standpoint, this is not
  * necessary, but it greatly improves readability.
  */
object Definition {
  def fromNode(node: StoredNode, nodeToNumber: Map[StoredNode, Int]): Definition = {
    nodeToNumber(node)
  }
}

object ReachingDefProblem {
  def create(method: Method): DataFlowProblem[StoredNode, mutable.BitSet] = {
    val flowGraph = new ReachingDefFlowGraph(method)
    val transfer  = new OptimizedReachingDefTransferFunction(flowGraph)
    val init      = new ReachingDefInit(transfer.gen)
    def meet: (mutable.BitSet, mutable.BitSet) => mutable.BitSet =
      (x: mutable.BitSet, y: mutable.BitSet) => { x.union(y) }

    new DataFlowProblem[StoredNode, mutable.BitSet](flowGraph, transfer, meet, init, true, mutable.BitSet())
  }

}

/** The control flow graph as viewed by the data flow solver.
  */
class ReachingDefFlowGraph(val method: Method) extends FlowGraph[StoredNode] {

  private val logger: Logger = LoggerFactory.getLogger(this.getClass)

  val entryNode: StoredNode = method
  val exitNode: StoredNode  = method.methodReturn

  private val params           = method.parameter.sortBy(_.index)
  private val firstParam       = params.headOption
  private val lastParam        = params.lastOption
  private val firstOutputParam = firstParam.flatMap(_.asOutput.headOption)
  private val lastOutputParam  = method.parameter.sortBy(_.index).asOutput.lastOption

  private val lastActualCfgNode = exitNode._cfgIn.nextOption()

  val allNodesReversePostOrder: List[StoredNode] =
    List(entryNode) ++ method.parameter.toList ++ method.reversePostOrder.toList.filter(x =>
      x.id != entryNode.id && x.id != exitNode.id
    ) ++ method.parameter.asOutput.toList ++ List(exitNode)

  private val allNodesEvenUnreachable =
    allNodesReversePostOrder ++ method.cfgNode.l.filterNot(x => allNodesReversePostOrder.contains(x))
  val nodeToNumber: Map[StoredNode, Int] = allNodesEvenUnreachable.zipWithIndex.map { case (x, i) => x -> i }.toMap
  val numberToNode: Map[Int, StoredNode] = allNodesEvenUnreachable.zipWithIndex.map { case (x, i) => i -> x }.toMap

  val allNodesPostOrder: List[StoredNode] = allNodesReversePostOrder.reverse

  private val _succ: Map[StoredNode, List[StoredNode]] = initSucc(allNodesReversePostOrder)
  private val _pred: Map[StoredNode, List[StoredNode]] = initPred(allNodesReversePostOrder, method)

  override def succ(node: StoredNode): IterableOnce[StoredNode] = {
    _succ.apply(node)
  }

  override def pred(node: StoredNode): IterableOnce[StoredNode] = {
    _pred.apply(node)
  }

  /** Create a map that allows CFG successors to be retrieved for each node
    */
  private def initSucc(ns: List[StoredNode]): Map[StoredNode, List[StoredNode]] = {
    ns.map {
      case n: Method   => n   -> firstParamOrBody(n)
      case ret: Return => ret -> List(firstOutputParam.getOrElse(exitNode))
      case param: MethodParameterIn =>
        param -> nextParamOrBody(param)
      case paramOut: MethodParameterOut => paramOut -> nextParamOutOrExit(paramOut)
      case cfgNode: CfgNode             => cfgNode  -> cfgNextOrFirstOutParam(cfgNode)
      case n =>
        logger.warn(s"Node type ${n.getClass.getSimpleName} should not be part of the CFG")
        n -> List()
    }.toMap
  }

  /** Create a map that allows CFG predecessors to be retrieved for each node
    */
  private def initPred(ns: List[StoredNode], method: Method): Map[StoredNode, List[StoredNode]] = {
    ns.map {
      case param: MethodParameterIn     => param    -> previousParamOrEntry(param)
      case paramOut: MethodParameterOut => paramOut -> previousOutputParamOrLastNodeOfBody(paramOut)
      case n: CfgNode if method.cfgFirst.headOption.contains(n) => n -> List(lastParam.getOrElse(method))
      case n if n == exitNode                                   => n -> lastOutputParamOrLastNodeOfBody()
      case n @ (cfgNode: CfgNode)                               => n -> cfgNode.cfgPrev.l
      case n =>
        logger.warn(s"Node type ${n.getClass.getSimpleName} should not be part of the CFG")
        n -> List()
    }.toMap
  }

  private def firstParamOrBody(n: Method): List[StoredNode] = {
    if (firstParam.isDefined) {
      firstParam.toList
    } else {
      cfgNext(n)
    }
  }

  private def cfgNext(n: CfgNode): List[StoredNode] =
    n.out(EdgeTypes.CFG).map(_.asInstanceOf[StoredNode]).l

  private def nextParamOrBody(param: MethodParameterIn): List[StoredNode] = {
    val nextParam = param.method.parameter.index(param.index + 1).headOption
    if (nextParam.isDefined) { nextParam.toList }
    else { param.method.cfgFirst.l }
  }

  private def nextParamOutOrExit(paramOut: MethodParameterOut): List[StoredNode] = {
    val nextParam = paramOut.method.parameter.index(paramOut.index + 1).asOutput.headOption
    if (nextParam.isDefined) { nextParam.toList }
    else { List(exitNode) }
  }

  private def cfgNextOrFirstOutParam(cfgNode: CfgNode): List[StoredNode] = {
    // `.cfgNext` would be wrong here because it filters `METHOD_RETURN`
    val successors = cfgNode.out(EdgeTypes.CFG).map(_.asInstanceOf[StoredNode]).l
    if (successors == List(exitNode) && firstOutputParam.isDefined) {
      List(firstOutputParam.get)
    } else {
      successors
    }
  }

  private def previousParamOrEntry(param: MethodParameterIn): List[StoredNode] = {
    val prevParam = param.method.parameter.index(param.index - 1).headOption
    if (prevParam.isDefined) { prevParam.toList }
    else { List(method) }
  }

  private def previousOutputParamOrLastNodeOfBody(paramOut: MethodParameterOut): List[StoredNode] = {
    val prevParam = paramOut.method.parameter.index(paramOut.index - 1).asOutput.headOption
    if (prevParam.isDefined) { prevParam.toList }
    else { lastActualCfgNode.toList }
  }

  private def lastOutputParamOrLastNodeOfBody(): List[StoredNode] = {
    if (lastOutputParam.isDefined) { lastOutputParam.toList }
    else { lastActualCfgNode.toList }
  }

}

/** For each node of the graph, this transfer function defines how it affects the propagation of definitions.
  */
class ReachingDefTransferFunction(flowGraph: ReachingDefFlowGraph)
    extends TransferFunction[StoredNode, mutable.BitSet] {

  private val nodeToNumber = flowGraph.nodeToNumber

  val method: Method = flowGraph.method

  val gen: Map[StoredNode, mutable.BitSet] =
    initGen(method).withDefaultValue(mutable.BitSet())

  val kill: Map[StoredNode, mutable.BitSet] =
    initKill(method, gen).withDefaultValue(mutable.BitSet())

  /** For a given flow graph node `n` and set of definitions, apply the transfer function to obtain the updated set of
    * definitions, considering `gen(n)` and `kill(n)`.
    */
  override def apply(n: StoredNode, x: mutable.BitSet): mutable.BitSet = {
    gen(n).union(x.diff(kill(n)))
  }

  /** Initialize the map `gen`, a map that contains generated definitions for each flow graph node.
    */
  def initGen(method: Method): Map[StoredNode, mutable.BitSet] = {

    val defsForParams = method.parameter.l.map { param =>
      param -> mutable.BitSet(Definition.fromNode(param.asInstanceOf[StoredNode], nodeToNumber))
    }

    // We filter out field accesses to ensure that they propagate
    // taint unharmed.

    val defsForCalls = method.call
      .filterNot(x => isFieldAccess(x.name))
      .l
      .map { call =>
        call -> {
          val retVal = List(call)
          val args = call.argument
            .filter(hasValidGenType)
            .l
          mutable.BitSet(
            (retVal ++ args)
              .collect {
                case x if nodeToNumber.contains(x) =>
                  Definition.fromNode(x.asInstanceOf[StoredNode], nodeToNumber)
              }*
          )
        }
      }
    (defsForParams ++ defsForCalls).toMap
  }

  /** Restricts the types of nodes that represent definitions.
    */
  private def hasValidGenType(node: Expression): Boolean = {
    node match {
      case _: Call       => true
      case _: Identifier => true
      case _             => false
    }
  }

  /** Initialize the map `kill`, a map that contains killed definitions for each flow graph node.
    *
    * All operations in our graph are represented by calls and non-operations such as identifiers or field-identifiers
    * have empty gen and kill sets, meaning that they just pass on definitions unaltered.
    */
  private def initKill(method: Method, gen: Map[StoredNode, mutable.BitSet]): Map[StoredNode, mutable.BitSet] = {

    val allIdentifiers: Map[String, List[CfgNode]] = {
      val results             = mutable.Map.empty[String, List[CfgNode]]
      val identifierName2Node = method._identifierViaContainsOut.map { identifier => (identifier.name, identifier) }
      val paramName2Node      = method.parameter.map { parameter => (parameter.name, parameter) }
      (identifierName2Node ++ paramName2Node)
        .foreach { case (name, node) =>
          val oldValues = results.getOrElse(name, Nil)
          results.put(name, node :: oldValues)
        }
      results.toMap
    }

    val allCalls: Map[String, List[Call]] =
      method.call.l
        .groupBy(_.code)
        .withDefaultValue(List.empty[Call])

    // We filter out field accesses to ensure that they propagate
    // taint unharmed.

    method.call
      .filterNot(x => isGenericMemberAccessName(x.name))
      .map { call =>
        call -> killsForGens(gen(call), allIdentifiers, allCalls)
      }
      .toMap
  }

  /** The only way in which a call can kill another definition is by generating a new definition for the same variable.
    * Given the set of generated definitions `gens`, we calculate definitions of the same variable for each, that is, we
    * calculate kill(call) based on gen(call).
    */
  private def killsForGens(
    genOfCall: mutable.BitSet,
    allIdentifiers: Map[String, List[CfgNode]],
    allCalls: Map[String, List[Call]]
  ): mutable.BitSet = {

    def definitionsOfSameVariable(definition: Definition): Iterator[Definition] = {
      val definedNodes = flowGraph.numberToNode(definition) match {
        case param: MethodParameterIn =>
          allIdentifiers(param.name).iterator
            .filter(x => x.id != param.id)
        case identifier: Identifier =>
          val sameIdentifiers = allIdentifiers(identifier.name).iterator
            .filter(x => x.id != identifier.id)

          /** Killing an identifier should also kill field accesses on that identifier. For example, a reassignment `x =
            * new Box()` should kill any previous calls to `x.value`, `x.length()`, etc.
            */
          val sameObjects: Iterator[Call] = allCalls.valuesIterator.flatten
            .filter(_.name == Operators.fieldAccess)
            .filter(_.ast.isIdentifier.nameExact(identifier.name).nonEmpty)

          sameIdentifiers ++ sameObjects
        case call: Call =>
          allCalls(call.code).iterator
            .filter(x => x.id != call.id)
        case _ => Iterator.empty
      }
      definedNodes
        // It can happen that the CFG is broken and contains isolated nodes,
        // in which case they are not in `nodeToNumber`. Let's filter those.
        .collect { case x if nodeToNumber.contains(x) => Definition.fromNode(x, nodeToNumber) }
    }

    val res = mutable.BitSet()
    for (definition <- genOfCall) {
      res.addAll(definitionsOfSameVariable(definition))
    }
    res
  }

}

/** Lone Identifier Optimization: we first determine and store all identifiers that neither refer to a local nor a
  * parameter and that appear only once as a call argument and not also in a return statement. For these identifiers, we
  * know that they are not used in any other location in the code, and so, we remove them from `gen` sets so that they
  * need not be propagated through the entire graph only to determine that they reach the exit node. Instead, when
  * creating reaching definition edges, we simply create edges from the identifier to the exit node.
  */
class OptimizedReachingDefTransferFunction(flowGraph: ReachingDefFlowGraph)
    extends ReachingDefTransferFunction(flowGraph) {

  lazy val loneIdentifiers: Map[Call, List[Definition]] = {
    val identifiersInReturns = method._returnViaContainsOut.ast.isIdentifier.name.l
    val paramAndLocalNames   = method.parameter.name.l ++ method.local.name.l
    val callArgPairs = method.call.flatMap { call =>
      call.argument.isIdentifier
        .filterNot(i => paramAndLocalNames.contains(i.name))
        .filterNot(i => identifiersInReturns.contains(i.name))
        .map(arg => (arg.name, call, arg))
    }.l

    callArgPairs
      .groupBy(_._1)
      .collect { case (_, v) if v.size == 1 => v.map { case (_, call, arg) => (call, arg) }.head }
      .toList
      .groupBy(_._1)
      .map { case (k, v) =>
        (
          k,
          v.filter(x => flowGraph.nodeToNumber.contains(x._2))
            .map(x => Definition.fromNode(x._2, flowGraph.nodeToNumber))
        )
      }
  }

  override def initGen(method: Method): Map[StoredNode, mutable.BitSet] =
    withoutLoneIdentifiers(super.initGen(method))

  private def withoutLoneIdentifiers(g: Map[StoredNode, mutable.BitSet]): Map[StoredNode, mutable.BitSet] = {
    g.map { case (k, defs) =>
      k match {
        case call: Call if loneIdentifiers.contains(call) =>
          (call, defs.filterNot(loneIdentifiers(call).contains(_)))
        case _ => (k, defs)
      }
    }
  }
}

class ReachingDefInit(gen: Map[StoredNode, mutable.BitSet]) extends InOutInit[StoredNode, mutable.BitSet] {
  override def initIn: Map[StoredNode, mutable.BitSet] =
    Map
      .empty[StoredNode, mutable.BitSet]
      .withDefaultValue(mutable.BitSet())

  override def initOut: Map[StoredNode, mutable.BitSet] = gen
}
