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
  def fromNode(node: CfgNode, nodeToNumber: Map[CfgNode, Int]): Definition = {
    nodeToNumber(node)
  }
}

object ReachingDefProblem {
  def create(method: Method): DataFlowProblem[CfgNode, mutable.BitSet] = {
    val flowGraph = new ReachingDefFlowGraph(method)
    create(flowGraph, new OptimizedReachingDefTransferFunction(flowGraph))
  }

  /** The problem of `flowGraph` with `transfer`, whose `gen` is built when the solver first reads it. */
  private[reachingdef] def create(
    flowGraph: ReachingDefFlowGraph,
    transfer: ReachingDefTransferFunction
  ): DataFlowProblem[CfgNode, mutable.BitSet] = {
    val init = new ReachingDefInit(Map.empty) {
      override def initOut: Map[CfgNode, mutable.BitSet] = transfer.gen
    }
    def meet: (mutable.BitSet, mutable.BitSet) => mutable.BitSet =
      (x: mutable.BitSet, y: mutable.BitSet) => { x.union(y) }

    new DataFlowProblem(flowGraph, transfer, meet, init, true, mutable.BitSet())
  }

}

/** The control flow graph as viewed by the data flow solver.
  */
class ReachingDefFlowGraph(val method: Method) extends FlowGraph[CfgNode] {

  private val logger: Logger = LoggerFactory.getLogger(this.getClass)

  val entryNode: CfgNode = method
  val exitNode: CfgNode  = method.methodReturn

  private val params           = method.parameter.sortBy(_.index)
  private val firstParam       = params.headOption
  private val lastParam        = params.lastOption
  private val firstOutputParam = firstParam.flatMap(_.asOutput.headOption)
  private val lastOutputParam  = method.parameter.sortBy(_.index).asOutput.lastOption

  private val lastActualCfgNode = exitNode.cfgIn.nextOption()

  override val allNodesReversePostOrder: List[CfgNode] =
    List(entryNode) ++ method.parameter.toList ++ method.reversePostOrder.toList.filter(x =>
      x.id != entryNode.id && x.id != exitNode.id
    ) ++ method.parameter.asOutput.toList ++ List(exitNode)

  private val allNodesEvenUnreachable = {
    val reachable = allNodesReversePostOrder.toSet
    allNodesReversePostOrder ++ method.cfgNode.l.filterNot(node => reachable.contains(node))
  }
  val nodeToNumber: Map[CfgNode, Int] = allNodesEvenUnreachable.zipWithIndex.map { case (x, i) => x -> i }.toMap
  val numberToNode: Map[Int, CfgNode] = allNodesEvenUnreachable.zipWithIndex.map { case (x, i) => i -> x }.toMap

  override val allNodesPostOrder: List[CfgNode] = allNodesReversePostOrder.reverse

  private val _succ: Map[CfgNode, List[CfgNode]] = initSucc(allNodesReversePostOrder)
  private val _pred: Map[CfgNode, List[CfgNode]] = initPred(allNodesReversePostOrder, method)

  override def succ(node: CfgNode): IterableOnce[CfgNode] = {
    _succ.apply(node)
  }

  override def pred(node: CfgNode): IterableOnce[CfgNode] = {
    _pred.apply(node)
  }

  /** Create a map that allows CFG successors to be retrieved for each node
    */
  private def initSucc(ns: List[CfgNode]): Map[CfgNode, List[CfgNode]] = {
    ns.map {
      case n: Method                => n   -> firstParamOrBody(n)
      case ret: Return              => ret -> List(firstOutputParam.getOrElse(exitNode))
      case param: MethodParameterIn =>
        param -> nextParamOrBody(param)
      case paramOut: MethodParameterOut => paramOut -> nextParamOutOrExit(paramOut)
      case cfgNode: CfgNode             => cfgNode  -> cfgNextOrFirstOutParam(cfgNode)
    }.toMap
  }

  /** Create a map that allows CFG predecessors to be retrieved for each node
    */
  private def initPred(ns: List[CfgNode], method: Method): Map[CfgNode, List[CfgNode]] = {
    ns.map {
      case param: MethodParameterIn     => param    -> previousParamOrEntry(param)
      case paramOut: MethodParameterOut => paramOut -> previousOutputParamOrLastNodeOfBody(paramOut)
      case n: CfgNode if method.cfgFirst.headOption.contains(n) => n -> List(lastParam.getOrElse(method))
      case n if n == exitNode                                   => n -> lastOutputParamOrLastNodeOfBody()
      case n @ (cfgNode: CfgNode)                               => n -> cfgNode.cfgPrev.l
    }.toMap
  }

  private def firstParamOrBody(n: Method): List[CfgNode] = {
    if (firstParam.isDefined) {
      firstParam.toList
    } else {
      cfgNext(n)
    }
  }

  private def cfgNext(n: CfgNode): List[CfgNode] =
    n.out(EdgeTypes.CFG).map(_.asInstanceOf[CfgNode]).l

  private def nextParamOrBody(param: MethodParameterIn): List[CfgNode] = {
    val nextParam = param.method.parameter.index(param.index + 1).headOption
    if (nextParam.isDefined) { nextParam.toList }
    else { param.method.cfgFirst.l }
  }

  private def nextParamOutOrExit(paramOut: MethodParameterOut): List[CfgNode] = {
    val nextParam = paramOut.method.parameter.index(paramOut.index + 1).asOutput.headOption
    if (nextParam.isDefined) { nextParam.toList }
    else { List(exitNode) }
  }

  private def cfgNextOrFirstOutParam(cfgNode: CfgNode): List[CfgNode] = {
    // `.cfgNext` would be wrong here because it filters `METHOD_RETURN`
    val successors = cfgNode.out(EdgeTypes.CFG).map(_.asInstanceOf[CfgNode]).l
    if (successors == List(exitNode) && firstOutputParam.isDefined) {
      List(firstOutputParam.get)
    } else {
      successors
    }
  }

  private def previousParamOrEntry(param: MethodParameterIn): List[CfgNode] = {
    val prevParam = param.method.parameter.index(param.index - 1).headOption
    if (prevParam.isDefined) { prevParam.toList }
    else { List(method) }
  }

  private def previousOutputParamOrLastNodeOfBody(paramOut: MethodParameterOut): List[CfgNode] = {
    val prevParam = paramOut.method.parameter.index(paramOut.index - 1).asOutput.headOption
    if (prevParam.isDefined) { prevParam.toList }
    else { lastActualCfgNode.toList }
  }

  private def lastOutputParamOrLastNodeOfBody(): List[CfgNode] = {
    if (lastOutputParam.isDefined) { lastOutputParam.toList }
    else { lastActualCfgNode.toList }
  }

}

/** For each node of the graph, this transfer function defines how it affects the propagation of definitions.
  */
class ReachingDefTransferFunction(flowGraph: ReachingDefFlowGraph) extends TransferFunction[CfgNode, mutable.BitSet] {

  private val nodeToNumber = flowGraph.nodeToNumber

  val method: Method = flowGraph.method

  /** Built on first use. Each call's set is a bit set as long as the call's node number, so `gen` grows with the square
    * of the method's size; [[ReachingDefPass]] decides from [[numberOfDefinitions]] whether to skip a method, so a
    * method it skips never builds it.
    */
  lazy val gen: Map[CfgNode, mutable.BitSet] =
    initGen(method).withDefaultValue(mutable.BitSet())

  /** The number of definitions in [[gen]] as [[initGen]] builds it, counted without building it. */
  private[reachingdef] lazy val numberOfDefinitions: Int =
    method.parameter.size + definingCalls(method).iterator.map(definitionsOf(_).size).sum

  /** For each call, the definitions it kills: for each definition it generates, the other definitions of the same
    * variable. Built on first use, from [[killIndex]]. The transfer function does not read it; it reads [[killMasks]].
    */
  lazy val kill: Map[CfgNode, mutable.BitSet] =
    initKill(method, gen).withDefaultValue(mutable.BitSet())

  /** For each call, one array per variable it defines, holding all definitions of that variable, the call's own
    * included. The calls that define a variable share its array, so the masks take space in proportion to the number of
    * definitions in the method. Removing a call's masks from `in` and adding `gen` back gives `gen ∪ (in \ kill)`: a
    * definition outside `gen` is removed exactly when `kill` holds it, and `gen` is in both. Built on first use;
    * [[ReachingDefPass]] decides from [[numberOfDefinitions]] whether to skip a method, so a method it skips never
    * builds them.
    */
  private[reachingdef] lazy val killMasks: Map[CfgNode, Array[Array[Definition]]] =
    initKillMasks(method, gen).withDefaultValue(Array.empty)

  /** For a given flow graph node `n` and set of definitions `x`, apply the transfer function to obtain the updated set
    * of definitions: remove `n`'s [[killMasks]] from `x` and add `gen(n)`, which gives `gen(n) ∪ (x \ kill(n))`.
    */
  override def apply(n: CfgNode, x: mutable.BitSet): mutable.BitSet = {
    val result = x.clone()
    killMasks(n).foreach(mask => mask.foreach(result -= _))
    result |= gen(n)
  }

  /** Initialize the map `gen`, a map that contains generated definitions for each flow graph node.
    */
  def initGen(method: Method): Map[CfgNode, mutable.BitSet] = {

    val defsForParams = method.parameter.l.map { param =>
      param -> mutable.BitSet(Definition.fromNode(param.asInstanceOf[CfgNode], nodeToNumber))
    }

    // We filter out field accesses to ensure that they propagate
    // taint unharmed.

    val defsForCalls = definingCalls(method).map { call =>
      call -> mutable.BitSet(definitionsOf(call)*)
    }
    (defsForParams ++ defsForCalls).toMap
  }

  /** The calls that have a `gen` set: all but field accesses. */
  private def definingCalls(method: Method): List[Call] =
    method.call.filterNot(call => isFieldAccess(call.name)).l

  /** The definitions `call` generates: its own, and its identifier and call arguments' unless it is a ternary. */
  private[reachingdef] def definitionsOf(call: Call): List[Definition] = {
    val retVal = List(call)
    // Ternary only reads its operands; gen on args kills prior defs.
    val args =
      if (isReadOnlyShortCircuitOperator(call)) Iterator.empty
      else call.argument.filter(hasValidGenType)
    (retVal ++ args).collect {
      case node if nodeToNumber.contains(node) =>
        Definition.fromNode(node.asInstanceOf[CfgNode], nodeToNumber)
    }
  }

  /** Ternary must not gen its operand arguments. */
  private def isReadOnlyShortCircuitOperator(call: Call): Boolean =
    call.name == Operators.conditional

  /** Restricts the types of nodes that represent definitions.
    */
  private def hasValidGenType(node: Expression): Boolean = {
    node match {
      case _: Call       => true
      case _: Identifier => true
      case _             => false
    }
  }

  /** What a definition defines, for the kill sets: a parameter's name, an identifier's name, or a call's code. */
  private enum Variable {
    case ParameterName(name: String)
    case IdentifierName(name: String)
    case CallCode(code: String)
  }

  private def variableOf(definition: Definition): Option[Variable] =
    flowGraph.numberToNode(definition) match {
      case param: MethodParameterIn => Some(Variable.ParameterName(param.name))
      case identifier: Identifier   => Some(Variable.IdentifierName(identifier.name))
      case call: Call               => Some(Variable.CallCode(call.code))
      case _                        => None
    }

  /** Per variable, the definitions of it in the method, built once from indexes of the method's identifiers and
    * parameters by name, its field accesses by the identifier names in them, and its calls by code: for a parameter's
    * name, the parameters and identifiers of that name; for an identifier's name, those and the field accesses on it
    * (killing an identifier also kills field accesses on it: a reassignment `x = new Box()` kills `x.value`); for a
    * call's code, the calls with that code.
    */
  private lazy val killIndex: Variable => Array[Definition] = {
    val allIdentifiers: Map[String, List[CfgNode]] = {
      val results             = mutable.Map.empty[String, List[CfgNode]]
      val identifierName2Node = method._identifierViaContainsOut.map { identifier => (identifier.name, identifier) }
      val paramName2Node      = method.parameter.map { parameter => (parameter.name, parameter) }
      (identifierName2Node ++ paramName2Node)
        .foreach { case (name, node) =>
          val oldValues = results.getOrElse(name, Nil)
          results.put(name, node :: oldValues)
        }
      results.toMap.withDefaultValue(Nil)
    }
    val allCalls: Map[String, List[Call]] =
      method.call.l.groupBy(_.code).withDefaultValue(List.empty[Call])
    val fieldAccesses: Map[String, List[Call]] =
      method.call
        .nameExact(Operators.fieldAccess)
        .l
        .flatMap(call => call.ast.isIdentifier.name.distinct.map(_ -> call))
        .groupMap(_._1)(_._2)
        .withDefaultValue(List.empty[Call])
    val definitionsOf = mutable.HashMap.empty[Variable, Array[Definition]]
    variable =>
      definitionsOf.getOrElseUpdate(
        variable, {
          val nodes = variable match {
            case Variable.ParameterName(name)  => allIdentifiers(name).iterator
            case Variable.IdentifierName(name) => allIdentifiers(name).iterator ++ fieldAccesses(name)
            case Variable.CallCode(code)       => allCalls(code).iterator
          }
          nodes
            // It can happen that the CFG is broken and contains isolated nodes,
            // in which case they are not in `nodeToNumber`. Let's filter those.
            .collect { case node if nodeToNumber.contains(node) => Definition.fromNode(node, nodeToNumber) }.toArray
        }
      )
  }

  /** The calls whose kill sets are built: field and other member accesses are left out, so that they propagate taint
    * unharmed.
    */
  private def killingCalls(method: Method): Iterator[Call] =
    method.call.filterNot(call => isGenericMemberAccessName(call.name))

  /** Initialize the map `kill`, a map that contains killed definitions for each flow graph node: the only way in which
    * a call can kill another definition is by generating a new definition for the same variable, so for each definition
    * in `gen(call)`, the other definitions of its variable.
    *
    * All operations in our graph are represented by calls and non-operations such as identifiers or field-identifiers
    * have empty gen and kill sets, meaning that they just pass on definitions unaltered.
    */
  private[reachingdef] def initKill(method: Method, gen: Map[CfgNode, mutable.BitSet]): Map[CfgNode, mutable.BitSet] =
    killingCalls(method).map { call =>
      val killed = mutable.BitSet()
      gen(call).foreach { definition =>
        variableOf(definition).foreach(variable =>
          killIndex(variable).foreach(other => if (other != definition) killed += other)
        )
      }
      call -> killed
    }.toMap

  /** Initialize [[killMasks]]. A variable whose only definition is the call's own gets no mask. */
  private[reachingdef] def initKillMasks(
    method: Method,
    gen: Map[CfgNode, mutable.BitSet]
  ): Map[CfgNode, Array[Array[Definition]]] =
    killingCalls(method).map { call =>
      call -> gen(call).iterator
        .flatMap(definition => variableOf(definition).map(_ -> definition))
        .distinctBy(_._1)
        .flatMap { case (variable, definition) =>
          val definitions = killIndex(variable)
          if (definitions.forall(_ == definition)) None else Some(definitions)
        }
        .toArray
    }.toMap

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
    val callArgPairs         = method.call.flatMap { call =>
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

  override private[reachingdef] def definitionsOf(call: Call): List[Definition] = {
    val definitions = super.definitionsOf(call)
    loneIdentifiers.get(call) match {
      case Some(lone) => definitions.filterNot(lone.contains(_))
      case None       => definitions
    }
  }
}

class ReachingDefInit(gen: Map[CfgNode, mutable.BitSet]) extends InOutInit[CfgNode, mutable.BitSet] {
  override def initIn: Map[CfgNode, mutable.BitSet] =
    Map
      .empty[CfgNode, mutable.BitSet]
      .withDefaultValue(mutable.BitSet())

  override def initOut: Map[CfgNode, mutable.BitSet] = gen
}
