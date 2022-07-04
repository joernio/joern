package io.joern.jssrc2cpg.utils

import better.files.File
import io.joern.jssrc2cpg.Config
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.utils.ExternalCommand
import org.slf4j.LoggerFactory

import java.nio.file.Paths
import scala.util.Failure
import scala.util.Success

object AstGenRunner {

  private val logger = LoggerFactory.getLogger(getClass)

  private val TYPE_DEFINITION_FILE_EXTENSIONS = List(".t.ts.json", ".d.ts.json")

  case class AstGenRunnerResult(
    parsedFiles: List[(String, String)] = List.empty,
    skippedFiles: List[(String, String)] = List.empty
  )

  private def skippedFiles(in: File, astgenOut: List[String]): List[String] = {
    val skipped = astgenOut.collect {
      case out if !out.startsWith("Converted") =>
        val filename = out.substring(0, out.indexOf(" "))
        val reason   = out.substring(out.indexOf(" ") + 1)
        logger.warn(s"\t- failed to parse '${in / filename}': '$reason'")
        Some(filename)
      case out =>
        logger.debug(s"\t+ $out")
        None
    }
    skipped.flatten
  }

  private def shouldBeIgnoredByUserConfig(filePath: String, config: Config, out: File): Boolean = {
    val resolvedFilePath = filePath.replace(out.pathAsString, config.inputPath)
    val isInIgnoredFiles = config.ignoredFiles.exists {
      case ignorePath if File(ignorePath).isDirectory => resolvedFilePath.startsWith(ignorePath)
      case ignorePath                                 => resolvedFilePath == ignorePath
    }
    if (isInIgnoredFiles || config.ignoredFilesRegex.matches(resolvedFilePath)) {
      logger.debug(s"'$filePath' ignored by user configuration")
      true
    } else {
      false
    }
  }

  private def filterFiles(files: List[String], config: Config, out: File): List[String] = {
    files.filter {
      case filePath if TYPE_DEFINITION_FILE_EXTENSIONS.exists(filePath.endsWith) =>
        // We are not interested in JS / TS type definition files at this stage.
        // TODO: maybe we can enable that later on and use the type definitions there
        //  for enhancing the CPG with additional type information for functions
        false
      case filePath => !shouldBeIgnoredByUserConfig(filePath.stripSuffix(".json"), config, out)
    }
  }

  def execute(config: Config, out: File): AstGenRunnerResult = {
    val in = File(config.inputPath)
    logger.debug(s"\t+ Running astgen in '$in' ...")
    ExternalCommand.run(s"astgen -t ts -o $out", in.toString()) match {
      case Success(result) =>
        val parsed  = filterFiles(SourceFiles.determine(out.toString(), Set(".json")), config, out)
        val skipped = skippedFiles(in, result.toList)
        AstGenRunnerResult(parsed.map((in.toString(), _)), skipped.map((in.toString(), _)))
      case Failure(f) =>
        logger.error("\t- running astgen failed!", f)
        AstGenRunnerResult()
    }
  }

}
