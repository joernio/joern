package io.joern.ghidra2cpg.passes

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Method}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language._

import scala.util.Try

class JumpPass(cpg: Cpg) extends ForkJoinParallelCpgPass[Method](cpg) {

  override def generateParts(): Array[Method] =
    cpg.method.toArray
  override def runOnPart(diffGraph: DiffGraphBuilder, method: Method): Unit = {
    method.ast
      .filter(_.isInstanceOf[Call])
      .map(_.asInstanceOf[Call])
      .nameExact("<operator>.goto")
      .where(_.argument.order(1).isLiteral)
      .foreach { sourceCall =>
        sourceCall.argument.order(1).code.l.headOption.flatMap(parseAddress) match {
          case Some(destinationAddress) =>
            method.ast.filter(_.isInstanceOf[Call]).lineNumber(destinationAddress).foreach { destination =>
              diffGraph.addEdge(sourceCall, destination, EdgeTypes.CFG)
            }
          case _ => // Ignore for now
          /*
            TODO:
              - Ask ghidra to resolve addresses of JMPs
           */
        }
      }
  }

  private def parseAddress(address: String): Option[Int] = {
    Try(Integer.parseInt(address.replaceFirst("0x", ""), 16)).toOption
  }
}
