package io.joern.ghidra2cpg.passes

import ghidra.program.flatapi.FlatProgramAPI
import ghidra.program.model.listing.Program
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool, ParallelCpgPass}

import scala.jdk.CollectionConverters._
import scala.language.implicitConversions

class LiteralPass(
  cpg: Cpg,
  address2Literal: Map[Long, String],
  currentProgram: Program,
  flatProgramAPI: FlatProgramAPI,
  keyPool: IntervalKeyPool
) extends ParallelCpgPass[String](cpg, keyPools = Some(keyPool.split(1))) {

  override def partIterator: Iterator[String] = List("").iterator

  override def runOnPart(part: String): Iterator[DiffGraph] = {
    implicit val diffGraph: DiffGraph.Builder = DiffGraph.newBuilder
    val literals = flatProgramAPI
      .findStrings(currentProgram.getAddressFactory.getAddressSet, 4, 1, false, true)
      .asScala
      .map { literal =>
        flatProgramAPI
          .getBytes(literal.getAddress, literal.getLength)
          .map(_.toChar)
          .mkString("")
      } ++ address2Literal.values

    literals.sorted.distinct.foreach { literal =>
      val node = nodes
        .NewLiteral()
        .code(literal)
        .order(-1)
        .argumentIndex(-1)
        .typeFullName(literal)
      diffGraph.addNode(node)
    }
    Iterator(diffGraph.build())
  }
}
