package io.joern.c2cpg.utils

import io.joern.c2cpg.Config
import org.slf4j.LoggerFactory

import java.nio.file.{Path, Paths}
import scala.util.Failure
import scala.util.Success

object IncludeAutoDiscovery {

  private val logger = LoggerFactory.getLogger(IncludeAutoDiscovery.getClass)

  private val IS_WIN = scala.util.Properties.isWin

  val GCC_VERSION_COMMAND = Seq("gcc", "--version")

  private val CPP_INCLUDE_COMMAND =
    if (IS_WIN) Seq("gcc", "-xc++", "-E", "-v", ".", "-o", "nul")
    else Seq("gcc", "-xc++", "-E", "-v", "/dev/null", "-o", "/dev/null")

  private val C_INCLUDE_COMMAND =
    if (IS_WIN) Seq("gcc", "-xc", "-E", "-v", ".", "-o", "nul")
    else Seq("gcc", "-xc", "-E", "-v", "/dev/null", "-o", "/dev/null")

  // Only check once
  private var isGccAvailable: Option[Boolean] = None

  // Only discover them once
  private var systemIncludePathsC: Set[Path]   = Set.empty
  private var systemIncludePathsCPP: Set[Path] = Set.empty

  private def checkForGcc(): Boolean = {
    logger.debug("Checking gcc ...")
    ExternalCommand.run(GCC_VERSION_COMMAND, ".") match {
      case Success(result) =>
        logger.debug(s"GCC is available: ${result.mkString(System.lineSeparator())}")
        true
      case _ =>
        logger.warn("GCC is not installed. Discovery of system include paths will not be available.")
        false
    }
  }

  def gccAvailable(): Boolean = isGccAvailable match {
    case Some(value) =>
      value
    case None =>
      isGccAvailable = Option(checkForGcc())
      isGccAvailable.get
  }

  private def extractPaths(output: Seq[String]): Set[Path] = {
    val startIndex =
      output.indexWhere(_.contains("#include")) + 2
    val endIndex =
      if (IS_WIN) output.indexWhere(_.startsWith("End of search list.")) - 1
      else output.indexWhere(_.startsWith("COMPILER_PATH")) - 1
    output.slice(startIndex, endIndex).map(p => Paths.get(p.trim).toRealPath()).toSet
  }

  private def discoverPaths(command: Seq[String]): Set[Path] = ExternalCommand.run(command, ".") match {
    case Success(output) => extractPaths(output)
    case Failure(exception) =>
      logger.warn(s"Unable to discover system include paths. Running '$command' failed.", exception)
      Set.empty
  }

  def discoverIncludePathsC(config: Config): Set[Path] = {
    if (config.includePathsAutoDiscovery && systemIncludePathsC.nonEmpty) {
      systemIncludePathsC
    } else if (config.includePathsAutoDiscovery && systemIncludePathsC.isEmpty && gccAvailable()) {
      val includePathsC = discoverPaths(C_INCLUDE_COMMAND)
      if (includePathsC.nonEmpty) {
        logger.info(s"Using the following C system include paths:${includePathsC
            .mkString(s"${System.lineSeparator()}- ", s"${System.lineSeparator()}- ", System.lineSeparator())}")
      }
      systemIncludePathsC = includePathsC
      includePathsC
    } else {
      Set.empty
    }
  }

  def discoverIncludePathsCPP(config: Config): Set[Path] = {
    if (config.includePathsAutoDiscovery && systemIncludePathsCPP.nonEmpty) {
      systemIncludePathsCPP
    } else if (config.includePathsAutoDiscovery && systemIncludePathsCPP.isEmpty && gccAvailable()) {
      val includePathsCPP = discoverPaths(CPP_INCLUDE_COMMAND)
      if (includePathsCPP.nonEmpty) {
        logger.info(s"Using the following CPP system include paths:${includePathsCPP
            .mkString(s"${System.lineSeparator()}- ", s"${System.lineSeparator()}- ", System.lineSeparator())}")
      }
      systemIncludePathsCPP = includePathsCPP
      includePathsCPP
    } else {
      Set.empty
    }
  }

}
