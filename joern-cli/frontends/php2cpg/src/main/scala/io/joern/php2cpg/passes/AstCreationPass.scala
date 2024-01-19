package io.joern.php2cpg.passes

import better.files.File
import io.joern.php2cpg.Config
import io.joern.php2cpg.astcreation.AstCreator
import io.joern.php2cpg.parser.PhpParser
import io.joern.x2cpg.datastructures.Global
import io.joern.x2cpg.{SourceFiles, ValidationMode}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.ConcurrentWriterCpgPass
import org.slf4j.LoggerFactory

import scala.jdk.CollectionConverters.*

class AstCreationPass(config: Config, cpg: Cpg, parser: PhpParser)(implicit withSchemaValidation: ValidationMode)
    extends ConcurrentWriterCpgPass[String](cpg) {

  private val logger = LoggerFactory.getLogger(this.getClass)

  val PhpSourceFileExtensions: Set[String] = Set(".php")

  override def generateParts(): Array[String] = SourceFiles
    .determine(
      config.inputPath,
      PhpSourceFileExtensions,
      ignoredFilesRegex = Option(config.ignoredFilesRegex),
      ignoredFilesPath = Option(config.ignoredFiles)
    )
    .toArray

  override def runOnPart(diffGraph: DiffGraphBuilder, filename: String): Unit = {
    val relativeFilename = if (filename == config.inputPath) {
      File(filename).name
    } else {
      File(config.inputPath).relativize(File(filename)).toString
    }
    parser.parseFile(filename) match {
      case Some(parseResult) =>
        diffGraph.absorb(new AstCreator(relativeFilename, parseResult)(config.schemaValidation).createAst())

      case None =>
        logger.warn(s"Could not parse file $filename. Results will be missing!")
    }
  }
}
