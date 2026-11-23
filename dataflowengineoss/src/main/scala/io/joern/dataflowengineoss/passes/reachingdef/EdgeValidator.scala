package io.joern.dataflowengineoss.passes.reachingdef

import io.joern.dataflowengineoss.language.nodemethods.ArgumentFlows
import io.joern.dataflowengineoss.queryengine.Engine
import io.joern.dataflowengineoss.semanticsloader.{FlowPath, FlowSemantic, ParameterNode, Semantics}
import io.joern.dataflowengineoss.semanticsloader.FlowPath.{FlowMapping, PassThroughMapping}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, CfgNode, Expression, StoredNode}
import io.shiftleft.semanticcpg.language.*

import scala.collection.mutable

object EdgeValidator {

  /** Determines whether the edge from `parentNode`to `childNode` is valid, according to the given semantics.
    */
  def isValidEdge(childNode: CfgNode, parentNode: CfgNode)(implicit semantics: Semantics): Boolean =
    new CachedEdgeValidator().isValidEdge(childNode, parentNode)
}

/** [[EdgeValidator.isValidEdge]], with what it reads about a node computed once per instance: the call the node is an
  * argument of, the call that is its AST parent, the semantics of that call, and whether they use or define the node.
  * [[DdgGenerator]] keeps one instance for all the edges of a method, which it writes without changing the graph.
  */
private[reachingdef] final class CachedEdgeValidator(implicit semantics: Semantics) {

  private final class CallFacts(call: Call) {
    val semantics: List[FlowSemantic] = Engine.semanticsForCall(call)

    /** Whether the call has semantics, none of which taint its return value. */
    val isRetval: Boolean = semantics.exists(semantic => !semantic.mappings.exists(explicitlyFlowsToReturnValue))

    lazy val isInternalWithoutSemantics: Boolean =
      Engine.methodsForCall(call).internal.isNotStub.nonEmpty && semantics.isEmpty

    lazy val arguments: Set[Expression] = call.argument.toSet
  }

  private final class ExpressionFacts(node: Expression) {
    val inCall: Option[Call]                      = node.inCall.nextOption()
    val semantics: List[FlowSemantic]             = inCall.map(callFacts(_).semantics).getOrElse(Nil)
    lazy val astParentCall: Option[Call]          = node.astParent.start.collectAll[Call].headOption
    lazy val isUsed: Boolean                      = ArgumentFlows.isUsed(semantics, node)
    lazy val isDefined: Boolean                   = ArgumentFlows.isDefined(semantics, node)
    lazy val isOutputArgOfInternalMethod: Boolean = inCall.exists(callFacts(_).isInternalWithoutSemantics)
  }

  private val calls       = mutable.HashMap.empty[Call, CallFacts]
  private val expressions = mutable.HashMap.empty[Expression, ExpressionFacts]

  private def callFacts(call: Call): CallFacts = calls.getOrElseUpdate(call, new CallFacts(call))

  private def facts(node: Expression): ExpressionFacts = expressions.getOrElseUpdate(node, new ExpressionFacts(node))

  private def isCallRetval(node: StoredNode): Boolean = node match {
    case call: Call => callFacts(call).isRetval
    case _          => false
  }

  def isValidEdge(childNode: CfgNode, parentNode: CfgNode): Boolean =
    (childNode, parentNode) match {
      case (childNode: Expression, parentNode)
          if isCallRetval(parentNode) || !isValidEdgeToExpression(parentNode, childNode) =>
        false
      case (childNode: Call, parentNode: Expression)
          if isCallRetval(childNode) && callFacts(childNode).arguments.contains(parentNode) =>
        // e.g. foo(x), but there are semantics for `foo` that don't taint its return value
        // in which case we don't want `x` to taint `foo(x)`.
        false
      case (childNode: Expression, parentNode: Expression)
          if facts(parentNode).astParentCall == facts(childNode).astParentCall && facts(childNode).isDefined &&
            facts(parentNode).isUsed =>
        ArgumentFlows.hasDefinedFlow(facts(parentNode).semantics, parentNode, childNode)
      case (_: Expression, _: Expression)                         => true
      case (childNode: Expression, _) if !facts(childNode).isUsed => false
      case (_: Expression, _)                                     => true
      case (_, parentNode)                                        => !isCallRetval(parentNode)
    }

  /** The definitions `defs` that a call generates, indexed so that, for a use of an argument of the call, the pairs
    * that [[isValidEdge]] rejects in its third case are not enumerated: see [[EdgeTargets.candidates]].
    */
  def edgeTargets(defs: Array[CfgNode]): EdgeTargets = new EdgeTargets(defs)

  /** The call that is the lone AST parent of `node`, if any, as [[ExpressionFacts.astParentCall]] finds it; empty when
    * `node` does not have exactly one AST parent, whose [[ExpressionFacts.astParentCall]] throws.
    */
  private def loneAstParentCall(node: Expression): Option[Option[Call]] =
    node._astIn.take(2).toList match {
      case List(parent) => Some(parent match { case call: Call => Some(call); case _ => None })
      case _            => None
    }

  /** See [[edgeTargets]]. A definition `child` is decided by the third case of [[isValidEdge]] for a use `parent` when
    * both have the same AST parent call, `child` is defined and `parent` used. The case returns `hasDefinedFlow`, and
    * the cases before it only reject, so for such pairs the edge is valid only if the semantics of the call map
    * `parent` to `child`. Every mapping that can do so names `child` by its argument index (a flow mapping's
    * destination index, or `parent`'s own index for a pass-through) or by its argument name (a flow mapping's
    * destination name), so the definitions found under those keys are a superset of the ones it can accept. The
    * candidates come in the order of `defs`, so edges are added in the order the full enumeration adds them.
    */
  final class EdgeTargets(defs: Array[CfgNode]) {

    /** Positions in `defs` of the definitions of one AST parent call, by argument index and by argument name. */
    private final class Group(members: Array[Int]) {
      val all: Array[Int]                 = members
      val byIndex: Map[Int, Array[Int]]   = members.groupBy(position => argument(position).argumentIndex)
      val byName: Map[String, Array[Int]] =
        members.filter(argument(_).argumentName.isDefined).groupBy(argument(_).argumentName.get)
    }

    private def argument(position: Int): Expression = defs(position).asInstanceOf[Expression]

    private val keyOf: Array[Option[Option[Call]]] = defs.map {
      case child: Expression if facts(child).isDefined => loneAstParentCall(child)
      case _                                           => None
    }

    private val undecided: Array[Int] = defs.indices.filter(keyOf(_).isEmpty).toArray

    /** The groups by AST parent call, in the order of their first definition in `defs`. */
    private val groups: Array[(Option[Call], Group)] = {
      val decided = defs.indices.filter(keyOf(_).isDefined).toArray
      val members = decided.groupBy(keyOf(_).get)
      decided.map(keyOf(_).get).distinct.map(key => key -> new Group(members(key)))
    }

    /** The definitions whose edge from `use` [[isValidEdge]] can accept, in the order of `defs`: every definition
      * except those that the third case decides and that no mapping of the semantics names.
      */
    def candidates(use: CfgNode): Iterator[CfgNode] = use match {
      case parent: Expression if facts(parent).isUsed && facts(parent).semantics.nonEmpty =>
        loneAstParentCall(parent) match {
          case Some(key) =>
            val positions = mutable.BitSet()
            positions ++= undecided
            groups.foreach { case (other, group) =>
              if (other == key) named(parent, group, positions) else positions ++= group.all
            }
            positions.iterator.map(defs)
          case None => defs.iterator
        }
      case _ => defs.iterator
    }

    private def named(parent: Expression, group: Group, positions: mutable.BitSet): Unit =
      facts(parent).semantics.foreach(_.mappings.foreach {
        case PassThroughMapping => group.byIndex.get(parent.argumentIndex).foreach(positions ++= _)
        case FlowMapping(_, ParameterNode(dstIndex, dstName)) =>
          group.byIndex.get(dstIndex).foreach(positions ++= _)
          dstName.flatMap(group.byName.get).foreach(positions ++= _)
        case _ =>
      })
  }

  private def isValidEdgeToExpression(parNode: CfgNode, curNode: Expression): Boolean =
    parNode match {
      case parentNode: Expression =>
        val parent       = facts(parentNode)
        val current      = facts(curNode)
        val sameCallSite = parent.inCall == current.inCall
        !(sameCallSite && parent.isOutputArgOfInternalMethod) &&
        (sameCallSite && parent.isUsed && current.isDefined || !sameCallSite && current.isUsed)
      case _ =>
        facts(curNode).isUsed
    }

  private def explicitlyFlowsToReturnValue(flowPath: FlowPath): Boolean = flowPath match {
    // Some frontends (e.g. python) denote named arguments using `-1` as the argument index. As such
    // `-1` denotes the return value only if there's no argument name.
    case FlowMapping(_, ParameterNode(-1, None)) => true
    case PassThroughMapping                      => true
    case _                                       => false
  }
}
