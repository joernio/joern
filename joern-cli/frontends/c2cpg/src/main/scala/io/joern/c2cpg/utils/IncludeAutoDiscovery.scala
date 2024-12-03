package io.joern.c2cpg.utils

import better.files.File
import io.joern.c2cpg.Config
import org.slf4j.LoggerFactory

import java.nio.file.Path
import java.nio.file.Paths
import scala.util.Failure
import scala.util.Success

object IncludeAutoDiscovery {

  private val logger = LoggerFactory.getLogger(IncludeAutoDiscovery.getClass)

  private val IsWin = scala.util.Properties.isWin

  private val GccVersionCommand = Seq("gcc", "--version")

  private val CppIncludeCommand =
    if (IsWin) Seq("gcc", "-xc++", "-E", "-v", ".", "-o", "nul")
    else Seq("gcc", "-xc++", "-E", "-v", "/dev/null", "-o", "/dev/null")

  private val CIncludeCommand =
    if (IsWin) Seq("gcc", "-xc", "-E", "-v", ".", "-o", "nul")
    else Seq("gcc", "-xc", "-E", "-v", "/dev/null", "-o", "/dev/null")

  private val VsWhereCommand = Seq(
    "cmd.exe",
    "/C",
    "\"%ProgramFiles(x86)%\\Microsoft Visual Studio\\Installer\\vswhere.exe\" -property installationPath"
  )

  private val VcVarsCommand = Seq("cmd.exe", "/C", "VC\\Auxiliary\\Build\\vcvars64.bat")

  // Only check once
  private var isGccAvailable: Option[Boolean] = None

  // Only discover them once
  private var systemIncludePathsC: Set[Path]   = Set.empty
  private var systemIncludePathsCPP: Set[Path] = Set.empty

  private def checkForGcc(): Boolean = {
    ExternalCommand.run(GccVersionCommand, ".") match {
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
      if (IsWin) output.indexWhere(_.startsWith("End of search list.")) - 1
      else output.indexWhere(_.startsWith("COMPILER_PATH")) - 1
    output.slice(startIndex, endIndex).map(p => Paths.get(p.trim).toRealPath()).toSet
  }

  private def discoverPaths(command: Seq[String]): Set[Path] = ExternalCommand.run(command, ".") match {
    case Success(output) => extractPaths(output)
    case Failure(exception) =>
      logger.warn(s"Unable to discover system include paths. Running '$command' failed.", exception)
      Set.empty
  }

  private def discoverMSVCInstallPath(): Option[String] = {
    ExternalCommand.run(VsWhereCommand, ".") match {
      case Success(output) =>
        output.headOption
      case Failure(exception) =>
        logger.warn(s"Unable to discover MSVC installation path.", exception)
        None
    }
  }

  private def extractMSVCIncludePaths(resolvedInstallationPath: String): Set[Path] = {
    ExternalCommand.run(VcVarsCommand, resolvedInstallationPath, Map("VSCMD_DEBUG" -> "3")) match {
      case Success(results) =>
        val includesLine   = results.find(_.startsWith("INCLUDE="))
        val includesString = includesLine.map(_.replaceFirst("INCLUDE=", ""))
        val includes       = includesString.map(_.split(";").toSet.map(p => Paths.get(p.trim).toRealPath()))
        includes.getOrElse(Set.empty)
      case Failure(exception) =>
        logger.warn(s"Unable to discover MSVC system include paths.", exception)
        Set.empty
    }
  }

  private def discoverMSVCPaths(): Set[Path] = {
    discoverMSVCInstallPath().map(extractMSVCIncludePaths).getOrElse(Set.empty)
  }

  private def reportIncludePaths(paths: Set[Path], lang: String): Unit = {
    if (paths.nonEmpty) {
      val ls = System.lineSeparator()
      logger.info(s"Using the following $lang system include paths:${paths.mkString(s"$ls- ", s"$ls- ", ls)}")
    }
  }

  def discoverIncludePathsC(config: Config): Set[Path] = {
    if (!config.includePathsAutoDiscovery) return Set.empty
    if (systemIncludePathsC.nonEmpty) return systemIncludePathsC

    if (isMSVCProject(config)) {
      systemIncludePathsCPP = discoverMSVCPaths() // discovers paths for both languages
      systemIncludePathsC = systemIncludePathsCPP
      reportIncludePaths(systemIncludePathsC, "MSVC")
    }
    if (systemIncludePathsC.isEmpty && gccAvailable()) {
      systemIncludePathsC = discoverPaths(CIncludeCommand)
      reportIncludePaths(systemIncludePathsC, "C")
    }
    systemIncludePathsC
  }

  private def isMSVCProject(config: Config): Boolean = {
    if (!IsWin) return false
    val projectDir = File(config.inputPath)
    List(projectDir / ".vs", projectDir / ".vscode").exists(_.exists) ||
    projectDir.list.exists(_.`extension`(includeDot = false).exists(ext => ext == "sln" || ext == "vcxproj"))
  }

  def discoverIncludePathsCPP(config: Config): Set[Path] = {
    if (!config.includePathsAutoDiscovery) return Set.empty
    if (systemIncludePathsCPP.nonEmpty) return systemIncludePathsCPP

    if (isMSVCProject(config)) {
      systemIncludePathsCPP = discoverMSVCPaths() // discovers paths for both languages
      systemIncludePathsC = systemIncludePathsCPP
      reportIncludePaths(systemIncludePathsCPP, "MSVC")
    }
    if (systemIncludePathsCPP.isEmpty && gccAvailable()) {
      systemIncludePathsCPP = discoverPaths(CppIncludeCommand)
      reportIncludePaths(systemIncludePathsCPP, "CPP")
    }
    systemIncludePathsCPP
  }

}
