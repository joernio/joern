package io.joern.c2cpg.utils

import io.joern.c2cpg.C2Cpg.Config
import org.slf4j.LoggerFactory

import java.nio.file.{Path, Paths}
import scala.util.Failure
import scala.util.Success

object IncludeAutoDiscovery {

  private val logger = LoggerFactory.getLogger(IncludeAutoDiscovery.getClass)

  private val GCC_VERSION_COMMAND = "gcc --version"
  private val CPP_INCLUDE_COMMAND = "gcc -xc++ -E -v /dev/null -o /dev/null"
  private val C_INCLUDE_COMMAND = "gcc -xc -E -v /dev/null -o /dev/null"

  // Only check once
  private var isGccAvailable: Option[Boolean] = None

  // Only discover them once
  private var systemIncludePaths: Set[Path] = Set.empty

  private def checkForGcc(): Boolean = {
    logger.debug("Checking gcc ...")
    ExternalCommand.run(GCC_VERSION_COMMAND) match {
      case Success(result) =>
        logger.debug(s"GCC is available: $result")
        true
      case _ =>
        logger.warn("GCC is not installed. Discovery of system include paths will not be available.")
        false
    }
  }

  private def gccAvailable(): Boolean = isGccAvailable match {
    case Some(value) =>
      value
    case None =>
      isGccAvailable = Some(checkForGcc())
      isGccAvailable.get
  }

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
      case Success(output) => extractPaths(output)
      case Failure(exception) =>
        logger.warn(s"Unable to discover system include paths. Running '$command' failed.", exception)
        Set.empty
    }
  }

  def discoverIncludePaths(config: Config): Set[Path] = {
    if (config.includePathsAutoDiscovery && systemIncludePaths.nonEmpty) {
      systemIncludePaths
    } else if (config.includePathsAutoDiscovery && systemIncludePaths.isEmpty && gccAvailable()) {
      val includePaths = discoverPaths(C_INCLUDE_COMMAND) ++ discoverPaths(CPP_INCLUDE_COMMAND)
      if (includePaths.nonEmpty) {
        logger.info(
          "Using the following system include paths:" + includePaths
            .mkString(System.lineSeparator() + "- ", System.lineSeparator() + "- ", System.lineSeparator())
        )
      }
      systemIncludePaths = includePaths
      includePaths
    } else {
      Set.empty
    }
  }

}
