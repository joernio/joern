package io.joern.php2cpg.passes

import io.joern.php2cpg.Config
import io.joern.php2cpg.parser.{Domain, PhpParseResult, PhpParser}
import io.joern.x2cpg.SourceFiles
import io.shiftleft.passes.ForkJoinParallelCpgPass
import org.slf4j.{Logger, LoggerFactory}

import java.nio.file.Paths

private[php2cpg] type BatchOfPhpScripts = Array[String]

trait AstParsingPass(config: Config, parser: PhpParser) { this: ForkJoinParallelCpgPass[BatchOfPhpScripts] =>

  protected val logger: Logger = LoggerFactory.getLogger(this.getClass)

  private val PhpSourceFileExtensions: Set[String] = Set(".php")

  override def generateParts(): Array[BatchOfPhpScripts] = {
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

  override def runOnPart(builder: DiffGraphBuilder, part: BatchOfPhpScripts): Unit = {
    parser.parseFiles(part).foreach {
      case PhpParseResult(fileName, Some(result), _) => processPart(builder, fileName, result)
      case PhpParseResult(fileName, None, _) =>
        logger.warn(s"Could not parse file $fileName. Results will be missing!")
        None
    }
  }

  protected def processPart(builder: DiffGraphBuilder, fileName: String, result: Domain.PhpFile): Unit

}
