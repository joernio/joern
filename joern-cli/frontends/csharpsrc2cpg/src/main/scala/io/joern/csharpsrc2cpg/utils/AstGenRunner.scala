package io.joern.csharpsrc2cpg.utils

import better.files.File
import com.typesafe.config.ConfigFactory
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.utils.{Environment, ExternalCommand}
import org.slf4j.LoggerFactory
import versionsort.VersionHelper
import io.joern.csharpsrc2cpg.Config

import java.nio.file.Paths
import scala.util.{Failure, Success, Try}

object AstGenRunner {
  private val logger = LoggerFactory.getLogger(getClass)
  case class AstGenRunnerResult(parsedFiles: List[String] = List.empty, skippedFiles: List[String] = List.empty)
  lazy val DotnetAstgenWin      = "dotnetastgen-win.exe"
  lazy val DotnetAstgenWinArm   = "dotnetastgen-win-arm.exe"
  lazy val DotnetAstgenLinux    = "dotnetastgen-linux"
  lazy val DotnetAstgenLinuxArm = "dotnetastgen-linux-arm"
  lazy val DotnetAstgenMac      = "dotnetastgen-macos"
  lazy val DotnetAstgenMacArm   = "dotnetastgen-macos-arm"

  lazy private val executableName = Environment.operatingSystem match {
    case Environment.OperatingSystemType.Windows =>
      Environment.architecture match {
        case Environment.ArchitectureType.X86 => DotnetAstgenWin
        case Environment.ArchitectureType.ARM => DotnetAstgenWinArm
      }
    case Environment.OperatingSystemType.Linux =>
      Environment.architecture match {
        case Environment.ArchitectureType.X86 => DotnetAstgenLinux
        case Environment.ArchitectureType.ARM => DotnetAstgenLinuxArm
      }
    case Environment.OperatingSystemType.Mac =>
      Environment.architecture match {
        case Environment.ArchitectureType.X86 => DotnetAstgenMac
        case Environment.ArchitectureType.ARM => DotnetAstgenMac
      }
    case Environment.OperatingSystemType.Unknown =>
      logger.warn("Could not detect OS version! Defaulting to 'Linux'.")
      DotnetAstgenLinux
  }

  lazy private val executableDir: String = {
    val dir        = getClass.getProtectionDomain.getCodeSource.getLocation.toString
    val indexOfLib = dir.lastIndexOf("lib")
    val fixedDir = if (indexOfLib != -1) {
      new java.io.File(dir.substring("file:".length, indexOfLib)).toString
    } else {
      val indexOfTarget = dir.lastIndexOf("target")
      if (indexOfTarget != -1) {
        new java.io.File(dir.substring("file:".length, indexOfTarget)).toString
      } else {
        "."
      }
    }
    Paths.get(fixedDir, "/bin/astgen").toAbsolutePath.toString
  }

  private def hasCompatibleAstGenVersion(goastGenVersion: String): Boolean = {
    ExternalCommand.run("dotnetastgen -version", ".").toOption.map(_.mkString.strip()) match {
      case Some(installedVersion)
          if installedVersion != "unknown" &&
            Try(VersionHelper.compare(installedVersion, goastGenVersion)).toOption.getOrElse(-1) >= 0 =>
        logger.debug(s"Using local dotnetastgen v$installedVersion from systems PATH")
        true
      case Some(installedVersion) =>
        logger.debug(
          s"Found local dotnetastgen v$installedVersion in systems PATH but dotnetastgen requires at least v$goastGenVersion"
        )
        false
      case _ => false
    }
  }

  private lazy val astGenCommand = {
    val conf            = ConfigFactory.load
    val goastGenVersion = conf.getString("csharpsrc2cpg.dotnetastgen_version")
    if (hasCompatibleAstGenVersion(goastGenVersion)) {
      "dotnetastgen"
    } else {
      s"$executableDir/$executableName"
    }
  }
}

class AstGenRunner(config: Config) {
  import io.joern.csharpsrc2cpg.utils.AstGenRunner._

  private def isIgnoredByUserConfig(filePath: String): Boolean = {
    lazy val isInIgnoredFiles = config.ignoredFiles.exists {
      case ignorePath if File(ignorePath).isDirectory => filePath.startsWith(ignorePath)
      case ignorePath                                 => filePath == ignorePath
    }
    lazy val isInIgnoredFileRegex = config.ignoredFilesRegex.matches(filePath)
    if (isInIgnoredFiles || isInIgnoredFileRegex) {
      logger.debug(s"'$filePath' ignored by user configuration")
      true
    } else {
      false
    }
  }

  private def skippedFiles(in: File, astGenOut: List[String]): List[String] = {
    val skipped = astGenOut.collect {
      case out if !out.startsWith("Converted") =>
        val filename = out.substring(0, out.indexOf(" "))
        val reason   = out.substring(out.indexOf(" ") + 1)
        logger.warn(s"\t- failed to parse '${in}${java.io.File.separator}${filename}': '$reason'")
        Option(filename)
      case out =>
        logger.debug(s"\t+ $out")
        None
    }
    skipped.flatten
  }

  private def filterFiles(files: List[String], out: File): List[String] = {
    files.filter { file =>
      file.stripSuffix(".json").replace(out.pathAsString, config.inputPath) match {
        case filePath if isIgnoredByUserConfig(filePath) => false
        case filePath if filePath.endsWith(".csproj")    => false
        case _                                           => true
      }
    }
  }

  private def runAstGenNative(in: String, out: File, exclude: String): Try[Seq[String]] = {
    // TODO: Need to add exclude support in dotnetAstgen
    // val excludeCommand = if (exclude.isEmpty) "" else s"-exclude \"$exclude\""
    ExternalCommand.run(s"$astGenCommand -o ${out.toString()} -i $in", ".")
  }

  def execute(out: File): AstGenRunnerResult = {
    val in = File(config.inputPath)
    logger.info(s"Running dotnetastgen in '$config.inputPath' ...")
    runAstGenNative(config.inputPath, out, config.ignoredFilesRegex.toString()) match {
      case Success(result) =>
        val srcFiles = SourceFiles.determine(
          out.toString(),
          Set(".json"),
          ignoredFilesRegex = Option(config.ignoredFilesRegex),
          ignoredFilesPath = Option(config.ignoredFiles)
        )
        val parsed  = filterFiles(srcFiles, out)
        val skipped = skippedFiles(in, result.toList)
        AstGenRunnerResult(parsed, skipped)
      case Failure(f) =>
        logger.error("\t- running astgen failed!", f)
        AstGenRunnerResult()
    }
  }

}
