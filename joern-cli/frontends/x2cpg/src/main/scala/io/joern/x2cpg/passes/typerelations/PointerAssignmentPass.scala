package io.joern.x2cpg.passes.typerelations

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, NodeTypes, Operators}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language._
import org.slf4j.{Logger, LoggerFactory}
import overflowdb.traversal.Traversal

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/** Creates an intraprocedural pointer assignment graph. This provides may point to information for each identifier or
  * field identifier.
  */
class PointerAssignmentPass(cpg: Cpg) extends ForkJoinParallelCpgPass[Method](cpg) {

  private val logger: Logger = LoggerFactory.getLogger(this.getClass)

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
            case Some(tgtNode) =>
              builder.addEdge(varDecl.node, tgtNode.node, EdgeTypes.POINTS_TO)
            case None =>
          }
        }
    }
  }

  private def buildAssignmentGraph(m: Method): Map[VarInCtx, VarInCtx] = {
    val assignmentGraph = mutable.HashMap.empty[VarInCtx, VarInCtx]
    var worklist: ListBuffer[CfgNode] =
      mutable.ListBuffer.from(m.ast.isCall.name(Operators.alloc, Operators.arrayInitializer).l)

    while (worklist.nonEmpty) {
      val alloc = worklist.head
      worklist = worklist.tail
      val allocLhs = alloc.astParent.astChildren.collect {
        case x: Identifier      => VarInCtx(x)
        case y: FieldIdentifier => VarInCtx(y)
      }.toSet

      allocLhs.foreach { idAtAlloc =>
        if (!assignmentGraph.contains(idAtAlloc)) {
          assignmentGraph.put(idAtAlloc, VarInCtx(alloc))
        }
      }

      Traversal(alloc).method.ast
        .collect {
          case x: Identifier      => x
          case y: FieldIdentifier => y
        }
        .l
        .reverse
        .foreach { identifier =>
          val varDecl                 = VarInCtx(identifier)
          val siblings: List[CfgNode] = identifier.astParent.astChildren.isCfgNode.l
          if (siblings.indexOf(identifier) > 0 && allocLhs.contains(varDecl)) {
            val assignee = VarInCtx(siblings.head)
            if (!assignmentGraph.contains(assignee)) {
              assignmentGraph.put(assignee, varDecl)
              worklist += assignee.node
            }
          }
        }
    }

    assignmentGraph.toMap
  }

  private def buildPointsToGraph(assignmentGraph: Map[VarInCtx, VarInCtx]): Map[VarInCtx, VarInCtx] = {
    val pointsToGraph                  = mutable.HashMap.empty[VarInCtx, VarInCtx]
    var worklist: ListBuffer[VarInCtx] = mutable.ListBuffer.from(assignmentGraph.keySet)

    while (worklist.nonEmpty) {
      val varNode = worklist.head
      worklist = worklist.tail
      findRootAllocation(varNode, assignmentGraph) match {
        case Some(allocSite) => pointsToGraph.put(varNode, allocSite)
        case None            =>
      }
    }
    pointsToGraph.toMap
  }

  private def findRootAllocation(startNode: VarInCtx, assignmentGraph: Map[VarInCtx, VarInCtx]): Option[VarInCtx] = {
    var varNode = startNode
    Iterator
      .continually(assignmentGraph.get(varNode))
      .takeWhile(_.isDefined)
      .foreach { nextNode =>
        varNode = nextNode.get
      }
    Some(varNode)
  }

  /** Wraps around a node and provides scope given the parent block node.
    */
  class VarInCtx(val node: CfgNode) {

    val block: Option[Block] = node.parentBlock.nextOption()

    def canEqual(other: Any): Boolean = other.isInstanceOf[VarInCtx]

    override def equals(other: Any): Boolean = other match {
      case that: VarInCtx =>
        (that canEqual this) &&
        node.code == that.node.code &&
        block == that.block
      case _ => false
    }

    override def hashCode(): Int = {
      val state = Seq(node.code, block)
      state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    }

    override def toString: String = node.code
  }

  object VarInCtx {
    def apply(node: CfgNode): VarInCtx = new VarInCtx(node)
  }
}
