package io.joern.jssrc2cpg.utils

import better.files.File
import io.joern.x2cpg.SourceFiles
import org.slf4j.LoggerFactory

import scala.util.Failure
import scala.util.Success

object AstGenRunner {

  private val logger = LoggerFactory.getLogger(getClass)

  def execute(in: File, out: File): Set[(String, String)] = {
    logger.debug(s"\t+ Running astgen in '$in' ...")
    ExternalCommand.run(s"astgen -o $out", in.toString()) match {
      case Success(result) =>
        val astGenOut = result.mkString("; ")
        logger.debug("\t+ " + astGenOut)
        SourceFiles.determine(Set(out.toString()), Set(".json")).toSet.map { f: String => (in.toString(), f) }
      case Failure(f) =>
        logger.error("\t- astgen failed!", f)
        Set.empty
    }
  }

}
