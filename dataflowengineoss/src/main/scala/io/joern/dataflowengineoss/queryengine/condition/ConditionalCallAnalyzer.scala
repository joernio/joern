package io.joern.dataflowengineoss.queryengine.condition

import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

import scala.collection.mutable

/** One path from a method entry to a call that matched a target [[CallRule]] under the supplied initial conditions.
  *
  * @param root
  *   the method whose CFG was walked
  * @param target
  *   the matching call (the "bad call")
  * @param path
  *   CFG nodes from the method's first successor through `target`, in visit order
  */
final case class ConditionalCallResult(root: Method, target: Call, path: List[CfgNode])

/** Localized, single-method CFG walker that finds calls matching `targetCalls` which remain reachable after:
  *
  *   1. seeding a constant environment from `initialConditions` (parameter name → integer);
  *   1. evaluating `if` guards of the form `==` / `!=` against that environment and keeping only the live branch;
  *   1. pruning any path that hits a call matching `hardStops` (the "known dead end").
  *
  * The walk is a forward BFS over `cfgNext`, starting at `root.cfgNext` (semanticcpg has no `cfgFirst` step; a
  * `Method`'s CFG entry is its outgoing CFG successors). Each node id is recorded in a `visited` set the first time it
  * is enqueued. That set is what keeps the walker from looping forever on C `goto` / `while` cycles — OpenBSD kernel
  * CFGs are full of both. Because the environment is constant, visiting a node once is sound: a second visit cannot
  * observe a different state.
  *
  * Branch evaluation (v1):
  *
  *   - c2cpg does not put the `IF` [[ControlStructure]] on the CFG. The branch point is the condition call
  *     (`<operator>.equals` / `<operator>.notEquals`), whose `cfgNext` is `[then-entry, else-entry]`.
  *   - Operands are reduced with the same literal / inline-macro rules as [[CallRule]]: an [[Identifier]] looks up
  *     `state` by name; a [[Literal]] is parsed; an `INLINED` call follows `macroExpansion`.
  *   - If either operand cannot be reduced, or the identifier is absent from `initialConditions`, the guard is
  *     '''unknown''' and '''both''' successors are kept. Lookups never throw.
  *   - A known-true guard drops successors that belong to the `whenFalse` body; a known-false guard drops the
  *     `whenTrue` body.
  *
  * Pruning:
  *
  *   - A `hardStops` match records nothing and does not enqueue `cfgNext` (the rest of that path is a dead end).
  *   - A `targetCalls` match '''is not a stop''': the result is recorded and the walk continues, so a later hard-stop
  *     on the same path still prunes whatever follows the target.
  *
  * Limitations (v1, intentional): constant environment (no reassignment), only `==` / `!=` guards, intra-procedural, no
  * interprocedural descent.
  *
  * @param root
  *   method whose CFG is walked
  * @param initialConditions
  *   known integer values of incoming parameters, keyed by the parameter node
  * @param targetCalls
  *   rules identifying the "bad calls" to report
  * @param hardStops
  *   rules identifying known dead ends (`panic`, `exit`, …)
  */
class ConditionalCallAnalyzer(
  root: Method,
  initialConditions: Map[MethodParameterIn, BigInt],
  targetCalls: Vector[CallRule],
  hardStops: Vector[CallRule]
) {

  /** Parameter-name → value. Absent names are unknown, never a lookup exception. */
  private val state: Map[String, BigInt] =
    initialConditions.map { case (param, value) => param.name -> value }

  /** Condition-call id → (then-body CFG ids, else-body CFG ids). Used to filter `cfgNext` at an `if`. */
  private val branchBodies: Map[Long, (Set[Long], Set[Long])] = {
    root.controlStructure
      .filter(_.controlStructureType == ControlStructureTypes.IF)
      .flatMap { cs =>
        val trueIds  = Iterator.single(cs).whenTrue.ast.collectAll[CfgNode].id.toSet
        val falseIds = Iterator.single(cs).whenFalse.ast.collectAll[CfgNode].id.toSet
        Iterator.single(cs).condition.collectAll[Call].map(cond => cond.id -> (trueIds, falseIds))
      }
      .toMap
  }

  /** Walk `root`'s CFG and return every target call that is reachable under `initialConditions` without crossing a
    * hard-stop.
    */
  def analyze(): Vector[ConditionalCallResult] = {
    val results = mutable.ArrayBuffer.empty[ConditionalCallResult]
    val visited = mutable.HashSet.empty[Long]
    val queue   = mutable.Queue.empty[(CfgNode, List[CfgNode])]

    def enqueue(node: CfgNode, path: List[CfgNode]): Unit = {
      if (visited.add(node.id)) queue.enqueue((node, path))
    }

    root.cfgNext.foreach(entry => enqueue(entry, List(entry)))

    while (queue.nonEmpty) {
      val (node, path) = queue.dequeue()
      node match {
        case call: Call if hardStops.exists(_.matches(call)) =>
        // known dead end: do not expand cfgNext
        case call: Call =>
          if (targetCalls.exists(_.matches(call))) {
            results += ConditionalCallResult(root, call, path)
          }
          successors(call).foreach(succ => enqueue(succ, path :+ succ))
        case other =>
          successors(other).foreach(succ => enqueue(succ, path :+ succ))
      }
    }
    results.toVector
  }

  /** `cfgNext` of `node`, restricted to the live branch when `node` is a resolvable `if` condition. */
  private def successors(node: CfgNode): Iterator[CfgNode] = {
    val next = node.cfgNext
    node match {
      case call: Call if branchBodies.contains(call.id) =>
        evalGuard(call) match {
          case Some(true) =>
            val falseIds = branchBodies(call.id)._2
            next.filterNot(successor => falseIds.contains(successor.id))
          case Some(false) =>
            val trueIds = branchBodies(call.id)._1
            next.filterNot(successor => trueIds.contains(successor.id))
          case None =>
            next
        }
      case _ => next
    }
  }

  /** Reduce an `==` / `!=` condition to [[Some]] boolean, or [[None]] if either operand is unknown. */
  private def evalGuard(call: Call): Option[Boolean] = {
    if (call.name != Operators.equals && call.name != Operators.notEquals) return None
    val lhs = call.argumentOption(1).flatMap(evalExpr)
    val rhs = call.argumentOption(2).flatMap(evalExpr)
    (lhs, rhs) match {
      case (Some(l), Some(r)) =>
        val eq = l == r
        Some(if (call.name == Operators.equals) eq else !eq)
      case _ => None
    }
  }

  /** Best-effort integer reduction. Missing identifiers yield [[None]] (unknown), never an exception. */
  private def evalExpr(expr: Expression): Option[BigInt] = expr match {
    case id: Identifier =>
      state.get(id.name)
    case lit: Literal =>
      CallRule.parseNumericLiteral(lit.code)
    case call: Call if call.dispatchType == DispatchTypes.INLINED =>
      call.macroExpansion.collectFirst { case lit: Literal => CallRule.parseNumericLiteral(lit.code) }.flatten
    case _ => None
  }

}
