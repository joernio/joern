package io.joern.ghidra2cpg.passes

import ghidra.program.flatapi.FlatProgramAPI
import ghidra.program.model.listing.Program
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.passes.ConcurrentWriterCpgPass

import scala.jdk.CollectionConverters._
import scala.language.implicitConversions

class LiteralPass(cpg: Cpg, address2Literal: Map[Long, String], currentProgram: Program, flatProgramAPI: FlatProgramAPI)
    extends ConcurrentWriterCpgPass[Method](cpg) {

  override def runOnPart(diffGraph: DiffGraphBuilder, method: Method): Unit = {
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
  }
}
