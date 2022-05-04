package io.joern.x2cpg.passes.typerelations

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, Operators}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.Traversal

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/** Creates an intraprocedural pointer assignment graph. This provides may point to information for each identifier or
  * field identifier.
  */
class PointerAssignmentPass(cpg: Cpg) extends ForkJoinParallelCpgPass[Method](cpg) {

  override def generateParts(): Array[Method] = cpg.method.toArray

  override def runOnPart(builder: DiffGraphBuilder, method: Method): Unit = {
    val allocSites = method.ast.isCall.name(Operators.alloc, Operators.arrayInitializer).l
    if (allocSites.nonEmpty) {
      val assignmentGraph = buildAssignmentGraph(method)
      val pointsToGraph   = buildPointsToGraph(assignmentGraph)
      // All references to nodes in the method body should now have their
      // "may point to" edge added to an allocation site if it is in the map
      method.ast
        .collect {
          case x: Identifier      => x
          case y: FieldIdentifier => y
        }
        .map(new VarInCtx(_))
        .foreach { varDecl =>
          pointsToGraph.get(varDecl) match {
            case Some(tgtNodes) =>
              tgtNodes.foreach { tgt => builder.addEdge(varDecl.node, tgt.node, EdgeTypes.POINTS_TO) }
            case None =>
          }
        }
    }
  }

  /** Builds a graph where identifiers on the left of assignment statements are mapped to nodes on the right hand side.
    */
  private def buildAssignmentGraph(m: Method): Map[VarInCtx, Set[VarInCtx]] = {
    val assignmentGraph               = mutable.HashMap.empty[VarInCtx, Set[VarInCtx]]
    var worklist: ListBuffer[CfgNode] = mutable.ListBuffer.from(m.ast.isExpression.assignment.l)

    while (worklist.nonEmpty) {
      val assignment = worklist.head
      worklist = worklist.tail
      // Extract LHS and RHS of each expression
      val allocLhs: Set[VarInCtx] = assignment.astChildren
        .orderLte(1)
        .collect { case x: Identifier => VarInCtx(x) }
        .toSet
      val allocRhs: Set[VarInCtx] = assignment.astChildren
        .orderGt(1)
        .collect {
          case y: Call if y.methodFullName == Operators.alloc || y.methodFullName == Operators.arrayInitializer =>
            VarInCtx(y, isAllocNode = true)
          case y: Call                                          => VarInCtx(y)
          case y: Identifier if !allocLhs.contains(VarInCtx(y)) => VarInCtx(y)
        }
        .toSet
      // Map identifiers to their assignment/definition sites
      allocLhs.foreach { idAtAlloc =>
        assignmentGraph.put(idAtAlloc, assignmentGraph.getOrElse(idAtAlloc, Set()) ++ allocRhs)
      }
      // Point all instances of identifiers to where they may have been assigned/defined
      Traversal(assignment).method.ast
        .collect {
          case x: Identifier      => x
          case x: FieldIdentifier => x
        }
        .foreach { identifier =>
          val varDecl                 = VarInCtx(identifier)
          val siblings: List[CfgNode] = identifier.astParent.astChildren.isCfgNode.l
          if (siblings.indexOf(identifier) >= 0 && allocLhs.contains(varDecl)) {
            val assignee = VarInCtx(siblings.head)
            if (!assignmentGraph.contains(assignee)) {
              assignmentGraph.put(assignee, assignmentGraph.getOrElse(assignee, Set()) ++ Set(varDecl))
              worklist += assignee.node
            }
          }
        }
    }

    assignmentGraph.toMap
  }

  /** Builds a graph where identifiers are mapped to memory allocation sites.
    */
  private def buildPointsToGraph(assignmentGraph: Map[VarInCtx, Set[VarInCtx]]): Map[VarInCtx, Set[VarInCtx]] = {
    val pointsToGraph                  = mutable.HashMap.empty[VarInCtx, Set[VarInCtx]]
    var worklist: ListBuffer[VarInCtx] = mutable.ListBuffer.from(assignmentGraph.keySet)

    while (worklist.nonEmpty) {
      val varNode = worklist.head
      worklist = worklist.tail
      findAllocations(varNode, assignmentGraph) match {
        case allocSite if allocSite.nonEmpty => pointsToGraph.put(varNode, allocSite)
        case _                               =>
      }
    }
    pointsToGraph.toMap
  }

  /** Using the start node and the assignment graph, will traverse the assignment graph to find root nodes where start
    * node may point to allocated memory.
    */
  private def findAllocations(startNode: VarInCtx, assignmentGraph: Map[VarInCtx, Set[VarInCtx]]): Set[VarInCtx] = {
    var worklist: ListBuffer[VarInCtx] = mutable.ListBuffer(startNode)
    val allocs: ListBuffer[VarInCtx]   = mutable.ListBuffer.empty
    while (worklist.nonEmpty) {
      val varNode = worklist.head
      worklist = worklist.tail
      val assignments: Set[VarInCtx] = assignmentGraph.getOrElse(varNode, Set())
      allocs ++= assignments.filter(_.isAllocNode)
      worklist ++= assignments.filterNot(_.isAllocNode)
    }
    allocs.toSet
  }

  /** Wraps around a node and provides scope given the label.
    */
  class VarInCtx(val node: CfgNode, val isAllocNode: Boolean = false) {

    /** To separate a member shadowing a locally defined identifier.
      */
    val label: String = node.label

    def canEqual(other: Any): Boolean = other.isInstanceOf[VarInCtx]

    override def equals(other: Any): Boolean = other match {
      case that: VarInCtx =>
        (that canEqual this) &&
        node.code == that.node.code &&
        label == that.label &&
        isAllocNode == that.isAllocNode
      case _ => false
    }

    override def hashCode(): Int = {
      val state = Seq(node.code, label, isAllocNode)
      state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    }

    override def toString: String = node.code
  }

  object VarInCtx {
    def apply(node: CfgNode, isAllocNode: Boolean = false): VarInCtx = new VarInCtx(node, isAllocNode)
  }
}
