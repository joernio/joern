package io.joern.jssrc2cpg.utils

import better.files.File
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.utils.ExternalCommand
import org.slf4j.LoggerFactory

import scala.util.Failure
import scala.util.Success

object AstGenRunner {

  private val logger = LoggerFactory.getLogger(getClass)

  case class AstGenRunnerResult(
    parsedFiles: Set[(String, String)] = Set.empty,
    skippedFiles: Set[(String, String)] = Set.empty
  )

  private def skippedFiles(in: File, astgenOut: Set[String]): Set[String] = {
    val skipped = astgenOut.collect {
      case out if !out.startsWith("Converted") =>
        val filename = out.substring(0, out.indexOf(" "))
        val reason   = out.substring(out.indexOf(" ") + 1)
        logger.warn(s"\t- Failed to parse '${in / filename}': '$reason'")
        Some(filename)
      case out =>
        logger.info(s"\t+ $out")
        None
    }
    skipped.flatten
  }

  def execute(in: File, out: File): AstGenRunnerResult = {
    logger.debug(s"\t+ Running astgen in '$in' ...")
    ExternalCommand.run(s"astgen -o $out", in.toString()) match {
      case Success(result) =>
        val parsed  = SourceFiles.determine(Set(out.toString()), Set(".json")).toSet
        val skipped = skippedFiles(in, result.toSet)
        AstGenRunnerResult(
          parsed.map { f: String => (in.toString(), f) },
          skipped.map { f: String => (in.toString(), f) }
        )
      case Failure(f) =>
        logger.error("\t- astgen failed!", f)
        AstGenRunnerResult()
    }
  }

}
