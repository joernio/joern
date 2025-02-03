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

  // 10 is just an arbitrary number - we merely want to log 'a few times' but no more than that
  val hasLogged = java.util.concurrent.atomic.AtomicInteger(10)

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
          // duplicate check looks (and is) superfluous, but it's a fastpath micro optimization
          if (hasLogged.get() > 0 && hasLogged.decrementAndGet() > 0) {
            val method = containsIn.nextOption().map(_.toString).getOrElse("N/A")
            logger.warn(
              s"Found CDG edge starting at $nodeLabel node $node <-> ${postDomFrontierNode}. This is most likely caused by an invalid CFG." +
                s" Method: ${method}" +
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
