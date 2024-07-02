package io.joern.x2cpg.passes.controlflow.codepencegraph

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{
  Call,
  ControlStructure,
  Identifier,
  JumpTarget,
  Literal,
  Method,
  MethodRef,
  Unknown
}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language.*
import io.joern.x2cpg.passes.controlflow.cfgdominator.{CfgDominatorFrontier, ReverseCpgCfgAdapter}
import org.slf4j.{Logger, LoggerFactory}

/** This pass has ContainsEdgePass and CfgDominatorPass as prerequisites.
  */
class CdgPass(cpg: Cpg) extends ForkJoinParallelCpgPass[Method](cpg) {
  import CdgPass.logger

  override def generateParts(): Array[Method] = cpg.method.toArray

  override def runOnPart(dstGraph: DiffGraphBuilder, method: Method): Unit = {

    val dominanceFrontier = new CfgDominatorFrontier(new ReverseCpgCfgAdapter, new CpgPostDomTreeAdapter)

    val cfgNodes         = method._containsOut.toList
    val postDomFrontiers = dominanceFrontier.calculate(method :: cfgNodes)

    postDomFrontiers.foreach { case (node, postDomFrontierNodes) =>
      postDomFrontierNodes.foreach {
        case postDomFrontierNode @ (_: Literal | _: Identifier | _: Call | _: MethodRef | _: Unknown |
            _: ControlStructure | _: JumpTarget) =>
          dstGraph.addEdge(postDomFrontierNode, node, EdgeTypes.CDG)
        case postDomFrontierNode =>
          val nodeLabel  = postDomFrontierNode.label
          val containsIn = postDomFrontierNode._containsIn
          if (containsIn == null || !containsIn.hasNext) {
            logger.warn(s"Found CDG edge starting at $nodeLabel node. This is most likely caused by an invalid CFG.")
          } else {
            val method = containsIn.next()
            logger.warn(
              s"Found CDG edge starting at $nodeLabel node. This is most likely caused by an invalid CFG." +
                s" Method: ${method match {
                    case m: Method => m.fullName;
                    case other     => other.label
                  }}" +
                s" number of outgoing CFG edges from $nodeLabel node: ${postDomFrontierNode._cfgOut.size}"
            )
          }
      }
    }
  }
}

object CdgPass {
  private val logger: Logger = LoggerFactory.getLogger(classOf[CdgPass])
}
