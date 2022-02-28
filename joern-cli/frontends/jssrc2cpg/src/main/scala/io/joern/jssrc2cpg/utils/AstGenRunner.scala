package io.joern.jssrc2cpg.utils

import better.files.File
import io.joern.x2cpg.SourceFiles
import org.slf4j.LoggerFactory

import scala.util.Failure
import scala.util.Success

object AstGenRunner {

  val ASTGEN_OUT: String = "ast_out"

  private val logger = LoggerFactory.getLogger(getClass)

  private val astGenExecutable =
    File(new java.io.File(getClass.getResource("/astgen").getPath).toString, "bin", "astgen.js").toString

  def execute(in: File): Set[String] = {
    logger.debug(s"\t+ Running astgen in '$in' ...")
    ExternalCommand.run(s"node $astGenExecutable", in.toString()) match {
      case Success(result) =>
        val astGenOut = result.mkString("; ")
        logger.debug("\t+ " + astGenOut)
        SourceFiles.determine(Set((in / ASTGEN_OUT).toString()), Set(".json")).toSet
      case Failure(f) =>
        logger.error("\t- astgen failed!", f)
        Set.empty
    }
  }

}
