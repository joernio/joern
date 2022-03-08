package io.joern.ghidra2cpg.passes

import ghidra.program.flatapi.FlatProgramAPI
import ghidra.program.util.DefinedDataIterator
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.passes.ConcurrentWriterCpgPass
import io.shiftleft.semanticcpg.language._

import scala.jdk.CollectionConverters._
import scala.language.implicitConversions
class LiteralPass(cpg: Cpg, flatProgramAPI: FlatProgramAPI)
    extends ConcurrentWriterCpgPass[Method](cpg) {
  val address2Literals: Map[Long, String] = DefinedDataIterator
    .definedStrings(flatProgramAPI.getCurrentProgram)
    .iterator()
    .asScala
    .toList
    .map(x => x.getAddress().getOffset -> x.getValue.toString)
    .toMap

  override def generateParts(): Array[Method] =  cpg.method.toArray//.map { ktFileWithMeta => bbb.f.getVirtualFilePath }.toArray
  override def runOnPart(diffGraph: DiffGraphBuilder, method: Method): Unit = {
    val literals = flatProgramAPI
      .findStrings(flatProgramAPI.getCurrentProgram.getAddressFactory.getAddressSet, 4, 1, false, true)
      .asScala
      .map { literal =>
        flatProgramAPI
          .getBytes(literal.getAddress, literal.getLength)
          .map(_.toChar)
          .mkString("")
      } ++ address2Literals.values

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
