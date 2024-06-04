package io.joern.ghidra2cpg.passes

import ghidra.program.flatapi.FlatProgramAPI
import ghidra.program.util.DefinedDataIterator
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.passes.ConcurrentWriterCpgPass

import scala.jdk.CollectionConverters._
import scala.language.implicitConversions

class LiteralPass(cpg: Cpg, flatProgramAPI: FlatProgramAPI) extends ConcurrentWriterCpgPass[String](cpg) {

  override def generateParts(): Array[String] = {
    val address2Literals: Map[Long, String] = DefinedDataIterator
      .definedStrings(flatProgramAPI.getCurrentProgram)
      .iterator()
      .asScala
      .toList
      .map(x => x.getAddress().getOffset -> x.getValue.toString)
      .toMap
    val literals = flatProgramAPI
      .findStrings(flatProgramAPI.getCurrentProgram.getAddressFactory.getAddressSet, 4, 1, false, true)
      .asScala
      .map { literal =>
        flatProgramAPI
          .getBytes(literal.getAddress, literal.getLength)
          .map(_.toChar)
          .mkString("")
      }
    (address2Literals ++ literals).map(_.toString).toArray
  }

  override def runOnPart(diffGraph: DiffGraphBuilder, literal: String): Unit = {
    val node = nodes
      .NewLiteral()
      .code(literal)
      .order(-1)
      .typeFullName(literal)
    diffGraph.addNode(node)
  }
}
