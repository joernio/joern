package io.joern.ghidra2cpg.passes

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Method}
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool, ParallelCpgPass}
import io.shiftleft.semanticcpg.language._

import scala.util.Try

class JumpPass(cpg: Cpg, keyPool: IntervalKeyPool)
    extends ParallelCpgPass[Method](
      cpg,
      keyPools = Some(keyPool.split(1))
    ) {

  override def partIterator: Iterator[Method] = cpg.method.l.iterator

  private def parseAddress(address: String): Option[Int] = {
    Try(Integer.parseInt(address, 16)).toOption
  }

  override def runOnPart(method: Method): Iterator[DiffGraph] = {
    implicit val diffGraph: DiffGraph.Builder = DiffGraph.newBuilder
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
    Iterator(diffGraph.build())
  }
}
