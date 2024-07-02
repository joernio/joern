package io.joern.rubysrc2cpg.deprecated.passes

import io.joern.rubysrc2cpg.Config
import io.joern.rubysrc2cpg.deprecated.astcreation.{AstCreator, ResourceManagedParser}
import io.joern.rubysrc2cpg.deprecated.parser.DeprecatedRubyParser
import io.joern.rubysrc2cpg.deprecated.utils.{PackageContext, PackageTable}
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.datastructures.Global
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate

import scala.jdk.CollectionConverters.EnumerationHasAsScala

class AstCreationPass(
  cpg: Cpg,
  parsedFiles: List[(String, DeprecatedRubyParser.ProgramContext)],
  packageTable: PackageTable,
  config: Config
) extends ForkJoinParallelCpgPass[(String, DeprecatedRubyParser.ProgramContext)](cpg) {

  private val logger = LoggerFactory.getLogger(this.getClass)

  override def generateParts(): Array[(String, DeprecatedRubyParser.ProgramContext)] = parsedFiles.toArray

  override def runOnPart(
    diffGraph: DiffGraphBuilder,
    fileNameAndContext: (String, DeprecatedRubyParser.ProgramContext)
  ): Unit = {
    val (fileName, context) = fileNameAndContext
    try {
      diffGraph.absorb(
        new AstCreator(fileName, context, PackageContext(fileName, packageTable), cpg.metaData.root.headOption)(
          config.schemaValidation
        ).createAst()
      )
    } catch {
      case ex: Exception =>
        logger.error(s"Error while processing AST for file - $fileName - ", ex)
    }
  }
}
