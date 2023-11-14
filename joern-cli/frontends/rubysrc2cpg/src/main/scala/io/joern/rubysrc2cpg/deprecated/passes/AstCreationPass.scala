package io.joern.rubysrc2cpg.deprecated.passes

import io.joern.rubysrc2cpg.Config
import io.joern.rubysrc2cpg.deprecated.astcreation.{AstCreator, ResourceManagedParser}
import io.joern.rubysrc2cpg.deprecated.utils.{PackageContext, PackageTable}
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.datastructures.Global
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.ConcurrentWriterCpgPass
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate

import scala.jdk.CollectionConverters.EnumerationHasAsScala

class AstCreationPass(
  cpg: Cpg,
  global: Global,
  parser: ResourceManagedParser,
  packageTable: PackageTable,
  config: Config
) extends ConcurrentWriterCpgPass[String](cpg) {

  private val logger                        = LoggerFactory.getLogger(this.getClass)
  val RubySourceFileExtensions: Set[String] = Set(".rb")

  def allUsedTypes(): List[String] =
    global.usedTypes.keys().asScala.toList

  override def generateParts(): Array[String] =
    SourceFiles
      .determine(
        config.inputPath,
        RubySourceFileExtensions,
        ignoredFilesRegex = Option(config.ignoredFilesRegex),
        ignoredFilesPath = Option(config.ignoredFiles)
      )
      .toArray

  override def runOnPart(diffGraph: DiffGraphBuilder, fileName: String): Unit = {
    try {
      diffGraph.absorb(
        new AstCreator(fileName, global, parser, PackageContext(fileName, packageTable), cpg.metaData.root.headOption)(
          config.schemaValidation
        ).createAst()
      )
    } catch {
      case ex: Exception =>
        logger.error(s"Error while processing AST for file - $fileName - ", ex)
    }
  }
}
