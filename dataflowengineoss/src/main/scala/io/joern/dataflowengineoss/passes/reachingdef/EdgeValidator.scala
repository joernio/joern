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
