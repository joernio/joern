package io.joern.dataflowengineoss.language

import io.joern.dataflowengineoss.queryengine.condition.{CallRule, ConditionalCallAnalyzer, ConditionalCallResult}
import io.shiftleft.codepropertygraph.generated.nodes.{Method, MethodParameterIn}

/** Traversal steps that find calls reachable from a method only under a given constant environment, pruning paths that
  * hit a known dead end.
  *
  * Usage (OpenBSD `M_WAITOK` during suspend — sleeps are illegal in that path):
  * {{{
  *   val method = cpg.method.name("test_activate")
  *   val action = method.parameter.name("action").head
  *   val mallocWaitOk = CallRule("malloc", argument = Some((3, 1)), expanded = true)
  *   val panic        = CallRule("panic")
  *   method.conditionallyReachesCalls(
  *     conditions  = Map(action -> BigInt(2)), // DVACT_SUSPEND
  *     targetCalls = Vector(mallocWaitOk),
  *     hardStops   = Vector(panic)
  *   )
  * }}}
  *
  * Returns an [[Iterator]] of [[ConditionalCallResult]]. (`flatgraph.help.Traversal` is a documentation annotation, not
  * a container type; this matches `reachableBy`.)
  */
class MethodConditionalReachabilityTraversal(val traversal: Iterator[Method]) extends AnyVal {

  /** For each method in this traversal, walk its CFG with `conditions` as a constant environment and yield every call
    * matching `targetCalls` that is reached without first matching `hardStops`.
    *
    * See [[ConditionalCallAnalyzer]] for the walker, guard evaluation, and pruning semantics.
    *
    * @param conditions
    *   incoming parameter values used to resolve `if` guards. An identifier whose name is not in this map is treated as
    *   unknown (both branches kept); the walker never throws on a missing key.
    * @param targetCalls
    *   "bad call" matchers (e.g. `malloc(..., M_WAITOK)`)
    * @param hardStops
    *   known dead ends (e.g. `panic()`); matching calls prune the path
    */
  def conditionallyReachesCalls(
    conditions: Map[MethodParameterIn, BigInt],
    targetCalls: Vector[CallRule],
    hardStops: Vector[CallRule]
  ): Iterator[ConditionalCallResult] =
    traversal.flatMap { method =>
      new ConditionalCallAnalyzer(method, conditions, targetCalls, hardStops).analyze()
    }

}
