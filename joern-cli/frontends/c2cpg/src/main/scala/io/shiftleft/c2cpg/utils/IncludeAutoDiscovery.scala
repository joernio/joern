package io.shiftleft.c2cpg.utils

import io.shiftleft.c2cpg.C2Cpg.Config
import org.slf4j.LoggerFactory

import java.nio.file.{Path, Paths}
import scala.util.{Failure, Success}

object IncludeAutoDiscovery {

  private val logger = LoggerFactory.getLogger(IncludeAutoDiscovery.getClass)

  private val CPP_INCLUDE_COMMAND = "gcc -xc++ -E -v /dev/null -o /dev/null"
  private val C_INCLUDE_COMMAND = "gcc -xc -E -v /dev/null -o /dev/null"

  private def extractPaths(output: Seq[String]): Set[Path] = {
    val startIndex = output.indexWhere(_.contains("#include")) + 2
    val endIndex = output.indexWhere(_.startsWith("COMPILER_PATH")) - 1
    output
      .slice(startIndex, endIndex)
      .map { p =>
        Paths.get(p.trim).toRealPath()
      }
      .toSet
  }

  private def discoverPaths(command: String): Set[Path] = {
    ExternalCommand.run(command) match {
      case Failure(exception) =>
        logger.warn(s"Unable to discover system include paths. Running '$command' failed.", exception)
        Set.empty
      case Success(output) => extractPaths(output)
    }
  }

  def discoverIncludePaths(config: Config): Set[Path] =
    if (config.includePathsAutoDiscovery) {
      val includePaths = discoverPaths(C_INCLUDE_COMMAND) ++ discoverPaths(CPP_INCLUDE_COMMAND)
      logger.info(
        "Using the following system include paths:" + includePaths
          .mkString(System.lineSeparator() + "- ", System.lineSeparator() + "- ", System.lineSeparator()))
      includePaths
    } else {
      Set.empty
    }

}
