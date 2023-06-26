package io.joern.rubysrc2cpg.passes
import io.joern.rubysrc2cpg.astcreation.AstCreator
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.datastructures.Global
import io.joern.x2cpg.passes.frontend.TypeNodePass
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.ConcurrentWriterCpgPass
import org.slf4j.LoggerFactory

import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters.EnumerationHasAsScala

class AstCreationPass(inputPath: String, cpg: Cpg) extends ConcurrentWriterCpgPass[String](cpg) {

  private val logger                        = LoggerFactory.getLogger(this.getClass)
  val global                                = new Global()
  val RubySourceFileExtensions: Set[String] = Set(".rb")

  def allUsedTypes(): List[String] =
    global.usedTypes.keys().asScala.toList

  override def generateParts(): Array[String] = SourceFiles.determine(inputPath, RubySourceFileExtensions).toArray

  override def runOnPart(diffGraph: DiffGraphBuilder, fileName: String): Unit = {
    diffGraph.absorb(new AstCreator(fileName, global).createAst())
  }
}
