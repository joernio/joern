package io.joern.rubysrc2cpg.passes
import io.joern.rubysrc2cpg.astcreation.AstCreator
import io.joern.rubysrc2cpg.utils.{PackageContext, PackageTable}
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.datastructures.Global
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.ConcurrentWriterCpgPass
import org.slf4j.LoggerFactory

import scala.jdk.CollectionConverters.EnumerationHasAsScala

class AstCreationPass(inputPath: String, cpg: Cpg, global: Global, packageTable: PackageTable)
    extends ConcurrentWriterCpgPass[String](cpg) {

  private val logger                        = LoggerFactory.getLogger(this.getClass)
  val RubySourceFileExtensions: Set[String] = Set(".rb")

  def allUsedTypes(): List[String] =
    global.usedTypes.keys().asScala.toList

  override def generateParts(): Array[String] = SourceFiles.determine(inputPath, RubySourceFileExtensions).toArray

  override def runOnPart(diffGraph: DiffGraphBuilder, fileName: String): Unit = {
    diffGraph.absorb(new AstCreator(fileName, global, PackageContext(fileName, packageTable)).createAst())
  }
}
