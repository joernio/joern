package io.joern.gosrc2cpg.utils

import better.files.File
import com.typesafe.config.ConfigFactory
import io.joern.gosrc2cpg.Config
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.utils.{Environment, ExternalCommand}
import org.slf4j.LoggerFactory
import versionsort.VersionHelper

import java.nio.file.Paths
import scala.util.{Failure, Success, Try}

object AstGenRunner {
  private val logger = LoggerFactory.getLogger(getClass)
  case class AstGenRunnerResult(
    parsedModFile: Option[String] = None,
    parsedFiles: List[String] = List.empty,
    skippedFiles: List[String] = List.empty
  )
  lazy val GoAstgenWin      = "goastgen-windows.exe"
  lazy val GoAstgenLinux    = "goastgen-linux"
  lazy val GoAstgenLinuxArm = "goastgen-linux-arm64"
  lazy val GoAstgenMac      = "goastgen-macos"
  lazy val GoAstgenMacArm   = "goastgen-macos-arm64"

  lazy private val executableName = Environment.operatingSystem match {
    case Environment.OperatingSystemType.Windows => GoAstgenWin
    case Environment.OperatingSystemType.Linux =>
      Environment.architecture match {
        case Environment.ArchitectureType.X86 => GoAstgenLinux
        case Environment.ArchitectureType.ARM => GoAstgenLinuxArm
      }
    case Environment.OperatingSystemType.Mac =>
      Environment.architecture match {
        case Environment.ArchitectureType.X86 => GoAstgenMac
        case Environment.ArchitectureType.ARM => GoAstgenMacArm
      }
    case Environment.OperatingSystemType.Unknown =>
      logger.warn("Could not detect OS version! Defaulting to 'Linux'.")
      GoAstgenLinux
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
    Paths.get(fixedDir, "/bin/goastgen").toAbsolutePath.toString
  }

  private def hasCompatibleAstGenVersion(goastGenVersion: String): Boolean = {
    ExternalCommand.run("goastgen -version", ".").toOption.map(_.mkString.strip()) match {
      case Some(installedVersion)
          if installedVersion != "unknown" &&
            Try(VersionHelper.compare(installedVersion, goastGenVersion)).toOption.getOrElse(-1) >= 0 =>
        logger.debug(s"Using local goastgen v$installedVersion from systems PATH")
        true
      case Some(installedVersion) =>
        logger.debug(
          s"Found local goastgen v$installedVersion in systems PATH but gosrc2cpg requires at least v$goastGenVersion"
        )
        false
      case _ => false
    }
  }

  private lazy val astGenCommand = {
    val conf            = ConfigFactory.load
    val goastGenVersion = conf.getString("gosrc2cpg.goastgen_version")
    if (hasCompatibleAstGenVersion(goastGenVersion)) {
      "goastgen"
    } else {
      s"$executableDir/$executableName"
    }
  }
}

class AstGenRunner(config: Config) {
  import io.joern.gosrc2cpg.utils.AstGenRunner._

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
        logger.warn(s"\t- failed to parse '${in / filename}': '$reason'")
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
        case filePath if filePath.endsWith(".mod")       => false
        case _                                           => true
      }
    }
  }

  private def filterModFile(files: List[String], out: File): List[String] = {
    files.filter { file =>
      file.stripSuffix(".json").replace(out.pathAsString, config.inputPath) match {
        case filePath if filePath.endsWith(".mod") => true
        case _                                     => false
      }
    }
  }

  private def runAstGenNative(in: String, out: File, exclude: String): Try[Seq[String]] = {
    val excludeCommand = if (exclude.isEmpty) "" else s"-exclude $exclude"
    ExternalCommand.run(s"$astGenCommand $excludeCommand -out ${out.toString()} $in", ".")
  }

  def execute(out: File): AstGenRunnerResult = {
    val in = File(config.inputPath)
    logger.info(s"Running goastgen in '$config.inputPath' ...")
    runAstGenNative(config.inputPath, out, config.ignoredFilesRegex.toString()) match {
      case Success(result) =>
        val srcFiles = SourceFiles.determine(
          out.toString(),
          Set(".json"),
          ignoredFilesRegex = Option(config.ignoredFilesRegex),
          ignoredFilesPath = Option(config.ignoredFiles)
        )
        val parsedModFile = filterModFile(srcFiles, out)
        val parsed        = filterFiles(srcFiles, out)
        val skipped       = skippedFiles(in, result.toList)
        AstGenRunnerResult(parsedModFile.headOption, parsed, skipped)
      case Failure(f) =>
        logger.error("\t- running astgen failed!", f)
        AstGenRunnerResult()
    }
  }

}
