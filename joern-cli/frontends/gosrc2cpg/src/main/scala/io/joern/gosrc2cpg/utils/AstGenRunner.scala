package io.joern.gosrc2cpg.utils

import better.files.File
import io.joern.gosrc2cpg.Config
import org.slf4j.LoggerFactory

import scala.util.{Failure, Success, Try}

object AstGenRunner {
  private val logger = LoggerFactory.getLogger(getClass)
  case class AstGenRunnerResult(
    parsedFiles: List[(String, String)] = List.empty,
    skippedFiles: List[(String, String)] = List.empty
  )
}

class AstGenRunner(config: Config) {
  import io.joern.gosrc2cpg.utils.AstGenRunner._

  private def runAstGenNative(in: String, out: File): Try[String] = {
    ???
  }

  def execute(out: File): AstGenRunnerResult = {
    logger.info(s"Running goastgen in '$config.inputPath' ...")
    runAstGenNative(config.inputPath, out) match {
      case Success(result) =>
        AstGenRunnerResult()
//        val parsed = filterFiles(SourceFiles.determine(out.toString(), Set(".json")), out)
//        val skipped = skippedFiles(in, result.toList)
//        AstGenRunnerResult(parsed.map((in.toString(), _)), skipped.map((in.toString(), _)))
      case Failure(f) =>
        logger.error("\t- running astgen failed!", f)
        AstGenRunnerResult()
    }
  }

}
