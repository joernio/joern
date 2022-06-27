package io.joern.jssrc2cpg.utils

import better.files.File
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.utils.ExternalCommand
import org.slf4j.LoggerFactory

import scala.util.Failure
import scala.util.Success

object AstGenRunner {

  private val logger = LoggerFactory.getLogger(getClass)

  private val TYPE_DEFINITION_FILE_EXTENSIONS = List(".t.ts.json", ".d.ts.json")

  case class AstGenRunnerResult(
    parsedFiles: Set[(String, String)] = Set.empty,
    skippedFiles: Set[(String, String)] = Set.empty
  )

  private def skippedFiles(in: File, astgenOut: Set[String]): Set[String] = {
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

  private def filterFiles(files: List[String]): Set[String] = {
    // We are not interested in JS / TS type definition files at this stage.
    // TODO: maybe we can enable that later on and use the type definitions there
    //  for enhancing the CPG with additional type information for functions
    files.filterNot(f => TYPE_DEFINITION_FILE_EXTENSIONS.exists(f.endsWith)).toSet
  }

  def execute(in: File, out: File): AstGenRunnerResult = {
    logger.debug(s"\t+ Running astgen in '$in' ...")
    ExternalCommand.run(s"astgen -t ts -o $out", in.toString()) match {
      case Success(result) =>
        val parsed  = filterFiles(SourceFiles.determine(out.toString(), Set(".json")))
        val skipped = skippedFiles(in, result.toSet)
        AstGenRunnerResult(parsed.map((in.toString(), _)), skipped.map((in.toString(), _)))
      case Failure(f) =>
        logger.error("\t- running astgen failed!", f)
        AstGenRunnerResult()
    }
  }

}
