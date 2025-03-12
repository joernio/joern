package io.joern.php2cpg.passes

import io.joern.php2cpg.Config
import io.joern.php2cpg.astcreation.AstCreator
import io.joern.php2cpg.parser.PhpParser
import io.joern.x2cpg.{SourceFiles, ValidationMode}
import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.ForkJoinParallelCpgPass
import org.slf4j.LoggerFactory

import java.nio.file.Paths
import scala.jdk.CollectionConverters.*

class AstCreationPass(config: Config, cpg: Cpg, parser: PhpParser)(implicit withSchemaValidation: ValidationMode)
    extends ForkJoinParallelCpgPass[Array[String]](cpg) {

  private val logger = LoggerFactory.getLogger(this.getClass)

  val PhpSourceFileExtensions: Set[String] = Set(".php")

  override def generateParts(): Array[Array[String]] = {
    val sourceFiles = SourceFiles
      .determine(
        config.inputPath,
        PhpSourceFileExtensions,
        ignoredFilesRegex = Option(config.ignoredFilesRegex),
        ignoredFilesPath = Option(config.ignoredFiles)
      )
      .toArray

    // We need to feed the php parser big groups of file in order
    // to speed up the parsing. Apparently it is some sort of slow
    // startup phase which makes single file processing prohibitively
    // slow.
    // On the other hand we need to be careful to not choose too big
    // chunks because:
    //   1. The argument length to the php executable has system
    //      dependent limits
    //   2. We want to make use of multiple CPU cores for the rest
    //      of the CPG creation.
    //
    val parts = sourceFiles.grouped(20).toArray
    parts
  }

  override def runOnPart(diffGraph: DiffGraphBuilder, filenames: Array[String]): Unit = {
    parser.parseFiles(filenames).foreach { case (filename, parseResult, infoLines) =>
      parseResult match {
        case Some(parseResult) =>
          val relativeFilename = if (filename == config.inputPath) {
            Paths.get(filename).fileName
          } else {
            Paths.get(config.inputPath).relativize(Paths.get(filename)).toString
          }
          diffGraph.absorb(
            new AstCreator(relativeFilename, filename, parseResult, config.disableFileContent)(config.schemaValidation)
              .createAst()
          )
        case None =>
          logger.warn(s"Could not parse file $filename. Results will be missing!")
      }
    }
  }
}
