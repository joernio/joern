package io.joern.dataflowengineoss.passes.reachingdef

import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes._
import io.joern.dataflowengineoss.queryengine.AccessPathUsage.toTrackedBaseAndAccessPathSimple
import io.shiftleft.semanticcpg.accesspath.MatchResult
import io.shiftleft.semanticcpg.language._

import scala.collection.{Set, mutable}

/**
  * Upon calculating reaching definitions, we find ourselves with
  * a set of incoming definitions `in(n)` for each node `n` of the
  * flow graph. This component determines those of the incoming
  * definitions that are relevant as the value they define is
  * actually used by `n`.
  * */
class UsageAnalyzer(problem: DataFlowProblem[mutable.BitSet], in: Map[StoredNode, Set[Definition]]) {

  val numberToNode = problem.flowGraph.asInstanceOf[ReachingDefFlowGraph].numberToNode
  private val allNodes = in.keys.toList
  private val containerSet = Set(Operators.fieldAccess, Operators.indexAccess, Operators.indirectIndexAccess)
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
        sameVariable(use, inElemNode) || isContainer(use, inElemNode) || isPart(use, inElemNode) || isAlias(use,
                                                                                                            inElemNode)
      }
    }.toMap
  }

  /**
    * Determine whether the node `use` describes a container for `inElement`, e.g.,
    * use = `ptr` while inElement = `ptr->foo`.
    * */
  private def isContainer(use: Expression, inElement: StoredNode): Boolean = {
    inElement match {
      case call: Call if containerSet.contains(call.name) =>
        call.argument.headOption.exists { base =>
          base.code == use.code
        }
      case _ => false
    }
  }

  /**
    * Determine whether `use` is a part of `inElement`, e.g.,
    * use = `argv[0]` while inElement = `argv`
    * */
  private def isPart(use: Expression, inElement: StoredNode): Boolean = {
    use match {
      case call: Call if containerSet.contains(call.name) =>
        inElement match {
          case param: MethodParameterIn =>
            call.argument.headOption.exists { base =>
              base.code == param.name
            }
          case identifier: Identifier =>
            call.argument.headOption.exists { base =>
              base.code == identifier.name
            }
          case _ => false
        }
      case _ => false
    }
  }

  private def isAlias(use: Expression, inElement: StoredNode): Boolean = {
    use match {
      case useCall: Call =>
        inElement match {
          case inCall: Call =>
            val (useBase, useAccessPath) = toTrackedBaseAndAccessPathSimple(useCall)
            val (inBase, inAccessPath) = toTrackedBaseAndAccessPathSimple(inCall)
            useBase == inBase && useAccessPath.matchAndDiff(inAccessPath.elements)._1 == MatchResult.EXACT_MATCH
          case _ => false
        }
      case _ => false
    }
  }

  def uses(node: StoredNode): Set[Expression] = {
    val n: Set[Expression] = node match {
      case ret: Return =>
        ret.astChildren.collect { case x: Expression => x }.toSet
      case call: Call =>
        call.argument.toSet
      case _ => Set()
    }
    n.filterNot(_.isInstanceOf[FieldIdentifier])
  }

  /**
    * Compares arguments of calls with incoming definitions
    * to see if they refer to the same variable
    * */
  def sameVariable(use: Expression, incoming: StoredNode): Boolean = {
    incoming match {
      case param: MethodParameterIn =>
        use.code == param.name
      case call: Call =>
        use.code == call.code
      case identifier: Identifier => use.code == identifier.code
      case _                      => false
    }
  }

}
