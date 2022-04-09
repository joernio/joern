package io.joern.jimple2cpg.passes.pointsto

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language._
import org.slf4j.{Logger, LoggerFactory}
import overflowdb.traversal.Traversal

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/** Creates an intraprocedural pointer assignment graph. This provides may point to information for each identifier or
  * field identifier.
  */
class PointsToPass(cpg: Cpg) extends ForkJoinParallelCpgPass[Method](cpg) {

  val allocCall              = "<operator>.alloc"
  private val logger: Logger = LoggerFactory.getLogger(this.getClass)

  override def generateParts(): Array[Method] = cpg.method.toArray

  override def runOnPart(builder: DiffGraphBuilder, method: Method): Unit = {
    val allocSites = method.ast.isCall.name(allocCall).l
    if (allocSites.nonEmpty) {
      println(s"Creating assignment graph for ${method.fullName}")
      val assignmentGraph = buildAssignmentGraph(method)
      assignmentGraph.foreach { case (k, v) =>
        println(s"Variable $k assigned to $v")
      }
      println(s"Creating pointer assignment graph for ${method.fullName}")
      buildPointsToGraph(builder, assignmentGraph)
    }
  }

  private def buildAssignmentGraph(m: Method): Map[VarDecl, VarDecl] = {
    val assignmentGraph               = mutable.HashMap.empty[VarDecl, VarDecl]
    var worklist: ListBuffer[CfgNode] = mutable.ListBuffer.from(m.ast.isCall.name(allocCall).l)

    while (worklist.nonEmpty) {
      val alloc = worklist.head
      worklist = worklist.tail
      val allocLhs = alloc.astParent.astChildren.collect {
        case x: Identifier      => x.name
        case y: FieldIdentifier => y.canonicalName
      }.toSet
      Traversal(alloc).cfgChildren
        .collect {
          case x: Identifier      => x
          case y: FieldIdentifier => y
        }
        .l
        .reverse
        .foreach { identifier =>
          val siblings: List[CfgNode] = identifier.astParent.astChildren.isCfgNode.l
          if (siblings.indexOf(identifier) > 0 && allocLhs.contains(identifier.code)) {
            val assignee = new VarDecl(siblings.head)
            if (!assignmentGraph.contains(assignee)) {
              assignmentGraph.put(assignee, new VarDecl(identifier))
              worklist += assignee.node
            }
          }
        }
    }

    assignmentGraph.toMap
  }

  private def buildPointsToGraph(builder: DiffGraphBuilder, assignmentGraph: Map[VarDecl, VarDecl]): Unit = {
    var worklist: ListBuffer[VarDecl] = mutable.ListBuffer.from(assignmentGraph.keySet)

    while (worklist.nonEmpty) {
      val varNode = worklist.head
      worklist = worklist.tail
      findRootAllocation(varNode, assignmentGraph) match {
        case Some(allocSite) =>
          println(s"$varNode points to $allocSite")
          // Need to do more than this an find all references of the `varNode.node` in the method.
          builder.addEdge(varNode.node, allocSite.node, "DATA_FLOW")
        case None =>
      }
    }
  }

  private def findRootAllocation(startNode: VarDecl, assignmentGraph: Map[VarDecl, VarDecl]): Option[VarDecl] = {
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
    * @param node
    */
  class VarDecl(val node: CfgNode) {

    // TODO: Could make this a more permanent traversal step
    def getParentBlock(n: CfgNode): AstNode = {
      var nextBlock: AstNode = n
      while (!nextBlock.isInstanceOf[Block]) {
        nextBlock = nextBlock.astParent
      }
      nextBlock
    }

    val parentBlock: AstNode = getParentBlock(node)

    def canEqual(other: Any): Boolean = other.isInstanceOf[VarDecl]

    override def equals(other: Any): Boolean = other match {
      case that: VarDecl =>
        (that canEqual this) &&
        node.code == that.node.code &&
        parentBlock == that.parentBlock
      case _ => false
    }

    override def hashCode(): Int = {
      val state = Seq(node.code, parentBlock)
      state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    }

    override def toString: String = node.code
  }
}
