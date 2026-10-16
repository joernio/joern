package io.joern.dataflowengineoss.queryengine.condition

import io.joern.x2cpg.passes.controlflow.cfgcreation.{Cfg, CfgCreator, CfgEdge}
import io.joern.x2cpg.passes.controlflow.cfgcreation.Cfg.*
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, DispatchTypes, ModifierTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.{ICallResolver, NoResolve}

import scala.collection.mutable

final case class ConditionMatch(
  call: Call,
  arguments: Vector[ScalarValue],
  origins: Vector[Set[Call]],
  uncertain: Boolean,
  path: Vector[Call],
  context: Int,
  provenanceUnknown: Vector[Boolean]
)

final case class ConditionContext(id: Int, method: Method, arguments: Vector[ScalarValue], widened: Boolean)
final case class ConditionCallEdge(from: Int, call: Call, to: Int, uncertain: Boolean)
final case class ConditionResult(
  matches: Vector[ConditionMatch],
  unknownCalls: Vector[Call],
  contexts: Vector[ConditionContext],
  edges: Vector[ConditionCallEdge]
)

final class ConditionAwareReachability(
  cpg: Cpg,
  integerTypes: Map[String, (Int, Boolean)] = Map("int" -> (32, true), "unsigned int" -> (32, false)),
  source: Call => Boolean = _ => true
) {
  private val evaluation                           = new ScalarEvaluation(integerTypes)
  private implicit val callResolver: ICallResolver = NoResolve
  private case class MethodPlan(
    cfg: Cfg,
    successors: Map[Long, List[CfgEdge]],
    conditions: Set[Long],
    switches: Set[Long],
    escaped: Set[Long],
    parameters: Vector[MethodParameterIn],
    locals: Vector[Local]
  )
  private val plans               = mutable.Map.empty[Method, MethodPlan]
  private val expandedLiterals    = mutable.Map.empty[Long, Boolean]
  private val literals            = mutable.Map.empty[Long, Tracked]
  private val consumedExpressions = mutable.Map.empty[Long, Set[Long]]
  private val sharedLocals        = mutable.Map.empty[Long, Boolean]
  private val unknown             = Tracked(ScalarValue.Unknown)

  private case class Tracked(value: ScalarValue, origins: Set[Call] = Set.empty, provenanceUnknown: Boolean = false) {
    def join(other: Tracked): Tracked =
      if (this == other) this
      else Tracked(value.join(other.value), origins ++ other.origins, provenanceUnknown || other.provenanceUnknown)
  }

  private case class State(
    locals: Map[Long, Tracked],
    values: Map[Long, Tracked] = Map.empty,
    returned: Option[Tracked] = None,
    uncertain: Boolean = false
  ) {
    def updated(ids: Iterable[Long], update: Tracked => Tracked): State = {
      var next = locals
      ids.foreach { id =>
        next.get(id).foreach { previous =>
          val value = update(previous)
          if (value != previous) next = next.updated(id, value)
        }
      }
      if (next eq locals) this else copy(locals = next)
    }

    def join(other: State): State = {
      def merged(a: Map[Long, Tracked], b: Map[Long, Tracked]): Map[Long, Tracked] = {
        if (a eq b) a
        else {
          var result = a
          a.foreach { case (key, value) =>
            if (!b.contains(key)) {
              val joined = value.join(unknown)
              if (joined != value) result = result.updated(key, joined)
            }
          }
          b.foreach { case (key, value) =>
            val joined = a.getOrElse(key, unknown).join(value)
            if (!a.get(key).contains(joined)) result = result.updated(key, joined)
          }
          result
        }
      }
      State(
        merged(locals, other.locals),
        merged(values, other.values),
        (returned, other.returned) match {
          case (Some(a), Some(b)) => Some(a.join(b))
          case (None, None)       => None
          case _                  => Some(unknown)
        },
        uncertain || other.uncertain
      )
    }
  }

  private def plan(method: Method): MethodPlan = plans.getOrElseUpdate(
    method, {
      val cfg =
        new CfgCreator(method, io.shiftleft.codepropertygraph.generated.Cpg.newDiffGraphBuilder, forExecution = true)
          .build()
      MethodPlan(
        cfg,
        cfg.edges.groupBy(_.src.id),
        method.controlStructure.condition.map(_.id).toSet,
        method.controlStructure.isSwitch.condition.map(_.id).toSet,
        method.call.nameExact(Operators.addressOf).argument(1).ast.isIdentifier.refsTo.map(_.id).toSet,
        method.parameter.toVector.sortBy(_.index),
        method.local.toVector
      )
    }
  )

  def value(expression: Expression): ScalarValue = read(expression, State(Map.empty), Set.empty).value

  private def shared(local: Local): Boolean = sharedLocals.getOrElseUpdate(
    local.id,
    local.closureBindingId.nonEmpty || local.tag
      .nameExact("STORAGE_CLASS")
      .value
      .exists(value => value == "static" || value == "extern")
  )

  private def literal(node: Literal): Tracked = literals.getOrElseUpdate(
    node.id,
    Tracked(
      evaluation.literal(node),
      provenanceUnknown = expandedLiterals.getOrElseUpdate(
        node.id,
        Iterator(node: AstNode)
          .repeat(_._astIn)(_.emit.dedup)
          .collect { case call: Call if call.dispatchType == DispatchTypes.INLINED => call }
          .nonEmpty
      )
    )
  )

  private def completed(call: Call, state: State, value: Tracked): State = {
    val consumed = consumedExpressions.getOrElseUpdate(call.id, call.argument.ast.map(_.id).toSet)
    val values   = state.values.removedAll(consumed)
    val used     = call.astParent match {
      case block: Block =>
        block.astParent match {
          case parent: Call if parent.dispatchType == DispatchTypes.INLINED => true
          case _                                                            => false
        }
      case _ => true
    }
    state.copy(values = if (used) values.updated(call.id, value) else values - call.id)
  }

  private def constant(member: Member, seen: Set[Long]): Tracked = {
    if (
      seen(member.id) || member.astChildren
        .collect { case modifier: Modifier => modifier }
        .forall(_.modifierType != ModifierTypes.FINAL)
    ) unknown
    else {
      val initializers = member.astParent.astChildren
        .collect { case method: Method if method.name == "<clinit>" => method }
        .flatMap(_.call.nameExact(Operators.assignment))
        .filter(assignment => Iterator(assignment.argument(1)).isIdentifier.refsTo.contains(member))
        .map(_.argument(2))
        .toVector
      initializers match {
        case Vector(expression) => read(expression, State(Map.empty), seen + member.id)
        case _                  => unknown
      }
    }
  }

  private def read(expression: Expression, state: State, seen: Set[Long]): Tracked = {
    if (seen(expression.id)) unknown
    else if (expression.isInstanceOf[Call] && expression.asInstanceOf[Call].dispatchType == DispatchTypes.INLINED) {
      val call  = expression.asInstanceOf[Call]
      val style = call.tag.nameExact("MACRO_STYLE").value.toVector
      call.macroExpansion
        .collect { case node: Expression => node }
        .toVector
        .lastOption
        .map(node => read(node, state, seen + expression.id))
        .map(result =>
          result.copy(
            origins = if (source(call)) result.origins + call else result.origins,
            provenanceUnknown = result.provenanceUnknown && style != Vector("object")
          )
        )
        .getOrElse(unknown)
    } else
      state.values.getOrElse(
        expression.id,
        expression match {
          case node: Literal          => literal(node)
          case identifier: Identifier =>
            identifier.refsTo.toVector match {
              case Vector(local: Local) if shared(local) => unknown
              case Vector(member: Member)                => constant(member, seen)
              case Vector(declaration)                   => state.locals.getOrElse(declaration.id, unknown)
              case _                                     => unknown
            }
          case call: Call
              if call.name.startsWith("<operator") && call.dispatchType != DispatchTypes.DYNAMIC_DISPATCH &&
                !call.name.startsWith("<operator>.assignment") =>
            val arguments = call.argument.toVector.sortBy(_.argumentIndex).map(read(_, state, seen + expression.id))
            operator(call, arguments)
          case _ => unknown
        }
      )
  }

  private def operator(call: Call, arguments: Vector[Tracked]): Tracked = {
    val result  = evaluation.operator(call, arguments.map(_.value))
    val sources = call.name match {
      case Operators.equals | Operators.notEquals | Operators.lessThan | Operators.lessEqualsThan |
          Operators.greaterThan | Operators.greaterEqualsThan | Operators.logicalAnd | Operators.logicalOr |
          Operators.logicalNot =>
        Set.empty[Call]
      case Operators.assignment  => arguments.lift(1).map(_.origins).getOrElse(Set.empty)
      case Operators.conditional =>
        arguments.headOption.flatMap(_.value.truth) match {
          case Some(true)  => arguments.lift(1).map(_.origins).getOrElse(Set.empty)
          case Some(false) => arguments.lift(2).map(_.origins).getOrElse(Set.empty)
          case None        => arguments.drop(1).flatMap(_.origins).toSet
        }
      case _ => arguments.flatMap(_.origins).toSet
    }
    val provenanceUnknown = call.name match {
      case Operators.equals | Operators.notEquals | Operators.lessThan | Operators.lessEqualsThan |
          Operators.greaterThan | Operators.greaterEqualsThan | Operators.logicalAnd | Operators.logicalOr |
          Operators.logicalNot =>
        false
      case Operators.assignment  => arguments.lift(1).exists(_.provenanceUnknown)
      case Operators.conditional =>
        arguments.headOption.flatMap(_.value.truth) match {
          case Some(true)  => arguments.lift(1).exists(_.provenanceUnknown)
          case Some(false) => arguments.lift(2).exists(_.provenanceUnknown)
          case None        => arguments.drop(1).exists(_.provenanceUnknown)
        }
      case _ => arguments.exists(_.provenanceUnknown)
    }
    Tracked(result, sources, provenanceUnknown)
  }

  def reachable(root: Method, assumptions: Map[MethodParameterIn, BigInt], sinks: Call => Boolean): ConditionResult =
    reachable(root, assumptions, sinks, _ => false)

  def reachable(
    root: Method,
    assumptions: Map[MethodParameterIn, BigInt],
    sinks: Call => Boolean,
    terminates: Call => Boolean
  ): ConditionResult = {
    require(assumptions.keys.forall(_.method.id == root.id))
    case class Key(method: Method, inputs: Vector[Tracked], recursive: Boolean = false)
    class Context(val id: Int, val key: Key, var inputs: Vector[Tracked], val spine: Vector[Call]) {
      var result: Option[Tracked] = None
      var resultUncertain         = false
      val parents                 = mutable.Set.empty[Int]
      var uncertain               = key.recursive
      val matches                 = mutable.Map.empty[Long, ConditionMatch]
    }
    val contexts     = mutable.ArrayBuffer.empty[Context]
    val indexed      = mutable.Map.empty[Key, Context]
    val pending      = mutable.LinkedHashSet.empty[Int]
    val edges        = mutable.LinkedHashMap.empty[(Int, Long, Int), ConditionCallEdge]
    val unknownCalls = mutable.LinkedHashSet.empty[Call]

    def context(key: Key, inputs: Vector[Tracked], spine: Vector[Call], uncertain: Boolean): Context = {
      val result = indexed.getOrElseUpdate(
        key, {
          val created = new Context(contexts.size, key, inputs, spine)
          contexts += created
          pending += created.id
          created
        }
      )
      val joined = result.inputs.zip(inputs).map((a, b) => a.join(b))
      if (joined != result.inputs || (uncertain && !result.uncertain)) {
        result.inputs = joined
        result.uncertain ||= uncertain
        pending += result.id
      }
      result
    }

    val rootInputs = root.parameter.toVector.sortBy(_.index).map { parameter =>
      val value = assumptions.get(parameter).map(ScalarValue.exact(_)).getOrElse(ScalarValue.Unknown)
      Tracked(evaluation.convert(value, parameter.typeFullName))
    }
    context(Key(root, rootInputs), rootInputs, Vector.empty, false)

    def ancestor(current: Context, method: Method): Option[Context] = {
      val visited = mutable.Set.empty[Int]
      val queue   = mutable.Queue(current.id)
      var found   = Option.empty[Context]
      while (queue.nonEmpty && found.isEmpty) {
        val next = queue.dequeue()
        if (visited.add(next)) {
          val candidate = contexts(next)
          if (candidate.key.method == method) found = Some(candidate)
          else queue ++= candidate.parents
        }
      }
      found
    }

    def invoke(owner: Context, call: Call, method: Method, inputs: Vector[Tracked], uncertain: Boolean): Context = {
      val sameMethod        = ancestor(owner, method)
      val constantArguments = call.argument.forall(_.ast.isIdentifier.isEmpty)
      val (key, initial)    = sameMethod match {
        case Some(previous) if previous.inputs != inputs && !constantArguments =>
          Key(method, Vector.empty, recursive = true) -> previous.inputs.zip(inputs).map((a, b) => a.join(b))
        case _ => Key(method, inputs) -> inputs
      }
      val target = context(key, initial, owner.spine :+ call, uncertain || key.recursive)
      if (target.parents.add(owner.id)) pending += target.id
      edges.update(
        (owner.id, call.id, target.id),
        ConditionCallEdge(owner.id, call, target.id, uncertain || target.uncertain)
      )
      target
    }

    def invalidate(state: State, ids: Set[Long]): State =
      state.updated(ids, _.copy(value = ScalarValue.Unknown, provenanceUnknown = true))

    def inputsFor(method: Method, call: Call, arguments: Vector[Tracked]): Vector[Tracked] = {
      val actuals = call.argument.toVector
        .sortBy(_.argumentIndex)
        .zip(arguments)
        .map((node, value) => node.argumentIndex -> value)
        .toMap
      plan(method).parameters.map { parameter =>
        val actual = actuals.getOrElse(parameter.index, unknown)
        actual.copy(value = evaluation.convert(actual.value, parameter.typeFullName))
      }
    }

    def refine(edge: CfgEdge, state: State, truth: Boolean): State = edge.src match {
      case identifier: Identifier if !truth =>
        val ids = identifier.refsTo.map(_.id).toSet
        state.updated(ids, _.copy(value = ScalarValue.exact(0)))
      case call: Call if call.name == Operators.equals || call.name == Operators.notEquals =>
        val equal     = truth == (call.name == Operators.equals)
        val arguments = call.argument.toVector.sortBy(_.argumentIndex)
        if (!equal) state
        else
          arguments.zip(arguments.reverse).foldLeft(state) {
            case (result, (target: Identifier, actual)) =>
              val value = read(actual, result, Set.empty)
              if (value.value.constant.isEmpty) result
              else {
                val ids        = target.refsTo.map(_.id).toSet
                val targetType = target.refsTo.toVector match {
                  case Vector(local: Local)                 => local.typeFullName
                  case Vector(parameter: MethodParameterIn) => parameter.typeFullName
                  case Vector(member: Member)               => member.typeFullName
                  case _                                    => target.typeFullName
                }
                val converted = evaluation.convert(value.value, targetType)
                if (converted.constant.nonEmpty) result.updated(ids, _.copy(value = converted)) else result
              }
            case (result, _) => result
          }
      case call: Call if call.name == Operators.and =>
        val arguments = call.argument.toVector.sortBy(_.argumentIndex)
        arguments.zip(arguments.reverse).foldLeft(state) {
          case (result, (identifier: Identifier, mask)) =>
            read(mask, result, Set.empty).value.constant.filter(bits => !truth || bits.bitCount == 1) match {
              case Some(bits) =>
                val ids = identifier.refsTo.map(_.id).toSet
                result.updated(
                  ids,
                  tracked => {
                    val value = tracked.value
                    tracked.copy(value =
                      if (truth) ScalarValue(value.knownZero & ~bits, value.knownOne | bits)
                      else ScalarValue(value.knownZero | bits, value.knownOne & ~bits)
                    )
                  }
                )
              case _ => result
            }
          case (result, _) => result
        }
      case _ => state
    }

    def caseExpression(node: CfgNode): Option[Expression] = node match {
      case label: JumpTarget if label.name != "default" =>
        label.astParent.astChildren.filter(_.order == label.order + 1).collectFirst { case expr: Expression => expr }
      case _ => None
    }

    while (pending.nonEmpty) {
      val current = contexts(pending.head)
      pending -= current.id
      val method           = current.key.method
      val procedure        = plan(method)
      val cfg              = procedure.cfg
      val successors       = procedure.successors
      val switchConditions = procedure.switches
      val escapedIds       = procedure.escaped
      val states           = mutable.Map.empty[Long, State]
      val work             = mutable.LinkedHashMap.empty[Long, CfgNode]
      val parameters       = procedure.parameters
      val initialLocals    = procedure.locals
        .map(local => local.id -> Tracked(evaluation.convert(ScalarValue.Unknown, local.typeFullName)))
        .toMap ++
        parameters.zip(current.inputs).map((parameter, input) => parameter.id -> input)
      var returned        = Option.empty[Tracked]
      var returnUncertain = false

      def enqueue(node: CfgNode, state: State): Unit = {
        val joined = states.get(node.id).map(_.join(state)).getOrElse(state)
        if (!states.get(node.id).contains(joined)) {
          states.update(node.id, joined)
          work.update(node.id, node)
        }
      }

      cfg.entryNode.foreach(node => enqueue(node, State(initialLocals, uncertain = current.uncertain)))

      while (work.nonEmpty) {
        val (nodeId, node) = work.head
        work -= nodeId
        val before               = states(nodeId)
        val after: Option[State] = node match {
          case identifier: Identifier =>
            val tracked = identifier.refsTo.toVector match {
              case Vector(local: Local) if shared(local) => unknown
              case Vector(member: Member)                => constant(member, Set.empty)
              case Vector(declaration)                   => before.locals.getOrElse(declaration.id, unknown)
              case _                                     => unknown
            }
            Some(before.copy(values = before.values.updated(node.id, tracked)))
          case _: Literal                     => Some(before)
          case call: Call if terminates(call) => None
          case call: Call                     =>
            val actualNodes = call.argument.toVector.sortBy(_.argumentIndex)
            val actuals     = actualNodes.map(read(_, before, Set.empty))
            if (sinks(call)) {
              val next = ConditionMatch(
                call,
                actuals.map(_.value),
                actuals.map(_.origins),
                before.uncertain,
                current.spine :+ call,
                current.id,
                actuals.map(_.provenanceUnknown)
              )
              current.matches.updateWith(call.id) {
                case Some(previous) =>
                  Some(
                    previous.copy(
                      arguments = previous.arguments.zip(next.arguments).map((a, b) => a.join(b)),
                      origins = previous.origins.zip(next.origins).map(_ ++ _),
                      provenanceUnknown = previous.provenanceUnknown.zip(next.provenanceUnknown).map(_ || _),
                      uncertain = previous.uncertain || next.uncertain
                    )
                  )
                case None => Some(next)
              }
              Some(completed(call, before, unknown))
            } else if (call.dispatchType == DispatchTypes.INLINED) {
              Some(before)
            } else if (call.name.startsWith("<operator") && call.dispatchType != DispatchTypes.DYNAMIC_DISPATCH) {
              val result     = operator(call, actuals)
              val assignment = call.name.startsWith("<operator>.assignment") ||
                call.name.startsWith("<operators>.assignment")
              val increment =
                Set(Operators.preIncrement, Operators.postIncrement, Operators.preDecrement, Operators.postDecrement)(
                  call.name
                )
              val targets =
                if (assignment || increment)
                  actualNodes.headOption.toVector
                    .collect { case identifier: Identifier =>
                      identifier.refsTo.map(_.id).toVector
                    }
                    .flatten
                    .toSet
                else Set.empty[Long]
              val modified =
                if (increment) invalidate(before, targets)
                else if (assignment && targets.nonEmpty)
                  before.updated(targets, _ => result)
                else if (assignment) invalidate(before, escapedIds)
                else before
              Some(completed(call, modified, result))
            } else {
              val targets     = call.callee.isNotStub.filterNot(_.isExternal).toVector.distinct
              val invalidated = invalidate(before, escapedIds)
              if (targets.isEmpty) {
                unknownCalls += call
                Some(completed(call, invalidated, unknown).copy(uncertain = true))
              } else {
                val invoked = targets.map { target =>
                  invoke(current, call, target, inputsFor(target, call, actuals), before.uncertain || targets.size != 1)
                }
                val results = invoked.flatMap(_.result)
                results
                  .reduceOption(_.join(_))
                  .map(result =>
                    completed(call, invalidated, result).copy(uncertain =
                      invalidated.uncertain || targets.size != 1 ||
                        invoked.exists(target => target.resultUncertain || target.result.isEmpty)
                    )
                  )
              }
            }
          case ret: Return =>
            val value = ret.astChildren
              .collectFirst { case expression: Expression => read(expression, before, Set.empty) }
              .getOrElse(unknown)
            Some(
              before.copy(returned =
                Some(value.copy(value = evaluation.convert(value.value, method.methodReturn.typeFullName)))
              )
            )
          case _: MethodReturn =>
            val value = before.returned.getOrElse(unknown)
            returned = Some(returned.map(_.join(value)).getOrElse(value))
            returnUncertain ||= before.uncertain
            None
          case _: Unknown => Some(before.copy(uncertain = true))
          case control: ControlStructure
              if control.controlStructureType == ControlStructureTypes.GOTO &&
                successors.getOrElse(node.id, List.empty).size > 1 =>
            Some(before.copy(uncertain = true))
          case _ => Some(before)
        }

        after.foreach { state =>
          val outgoing = successors.getOrElse(node.id, List.empty)
          val selector = node match {
            case expression: Expression => read(expression, state, Set.empty)
            case _                      => unknown
          }
          val cases      = outgoing.filter(_.edgeType == CaseEdge)
          val caseValues = cases.flatMap(edge => caseExpression(edge.dst).map(read(_, state, Set.empty).value))
          val equalsCase = caseValues.map(value => for (a <- selector.value.constant; b <- value.constant) yield a == b)
          outgoing.foreach { edge =>
            val selected: Option[Boolean] = edge.edgeType match {
              case TrueEdge                               => selector.value.truth
              case FalseEdge if switchConditions(node.id) =>
                if (equalsCase.exists(_.contains(true))) Some(false)
                else Option.when(equalsCase.forall(_.contains(false)))(true)
              case FalseEdge => selector.value.truth.map(!_)
              case CaseEdge  =>
                caseExpression(edge.dst) match {
                  case Some(expression) =>
                    for {
                      a <- selector.value.constant
                      b <- read(expression, state, Set.empty).value.constant
                    } yield a == b
                  case None =>
                    if (equalsCase.exists(_.contains(true))) Some(false)
                    else Option.when(equalsCase.forall(_.contains(false)))(true)
                }
              case _ => Some(true)
            }
            if (!selected.contains(false)) {
              val refined = edge.edgeType match {
                case TrueEdge                                => refine(edge, state, true)
                case FalseEdge if !switchConditions(node.id) => refine(edge, state, false)
                case _                                       => state
              }
              val values = if (procedure.conditions(node.id)) refined.values - node.id else refined.values
              enqueue(edge.dst, refined.copy(values = values, uncertain = refined.uncertain || selected.isEmpty))
            }
          }
        }
      }
      val joined = (current.result, returned) match {
        case (Some(a), Some(b)) => Some(a.join(b))
        case (None, value)      => value
        case (value, None)      => value
      }
      if (joined != current.result || (returnUncertain && !current.resultUncertain)) {
        current.result = joined
        current.resultUncertain ||= returnUncertain
        pending ++= current.parents
      }
    }

    ConditionResult(
      contexts.toVector.flatMap(_.matches.values).sortBy(result => (result.context, result.call.id)),
      unknownCalls.toVector,
      contexts.toVector.map(ctx =>
        ConditionContext(ctx.id, ctx.key.method, ctx.inputs.map(_.value), ctx.key.recursive)
      ),
      edges.values.toVector
    )
  }
}
