package io.joern.x2cpg.astgen

import better.files.File
import com.typesafe.config.ConfigFactory
import io.joern.x2cpg.utils.{Environment, ExternalCommand}
import io.joern.x2cpg.{SourceFiles, X2CpgConfig}
import org.slf4j.LoggerFactory
import versionsort.VersionHelper

import java.net.URL
import java.nio.file.Paths
import scala.collection.mutable
import scala.collection.mutable.HashMap
import scala.util.{Failure, Success, Try}

object AstGenRunner {

  private val logger = LoggerFactory.getLogger(getClass)

  /** @param parsedFiles
    *   the files parsed by the runner.
    * @param skippedFiles
    *   the files skipped by the runner.
    */
  case class AstGenRunnerResult(parsedFiles: List[String] = List.empty, skippedFiles: List[String] = List.empty)

  /** @param name
    *   the name of the AST gen executable, e.g., goastgen, dotnetastgen, swiftastgen, etc.
    * @param configPrefix
    *   the prefix of the executable's respective configuration path.
    */
  case class AstGenProgramMetaData(name: String, configPrefix: String, packagePath: URL)

  private lazy val Win      = "win.exe"
  private lazy val WinArm   = "win-arm.exe"
  private lazy val Linux    = "linux"
  private lazy val LinuxArm = "linux-arm"
  private lazy val Mac      = "macos"
  private lazy val MacArm   = "macos-arm"

  def executableName(implicit metaData: AstGenProgramMetaData): String = Environment.operatingSystem match {
    case Environment.OperatingSystemType.Windows =>
      Environment.architecture match {
        case Environment.ArchitectureType.X86 => s"${metaData.name}-$Win"
        case Environment.ArchitectureType.ARM => s"${metaData.name}-$WinArm"
      }
    case Environment.OperatingSystemType.Linux =>
      Environment.architecture match {
        case Environment.ArchitectureType.X86 => s"${metaData.name}-$Linux"
        case Environment.ArchitectureType.ARM => s"${metaData.name}-$LinuxArm"
      }
    case Environment.OperatingSystemType.Mac =>
      Environment.architecture match {
        case Environment.ArchitectureType.X86 => s"${metaData.name}-$Mac"
        case Environment.ArchitectureType.ARM => s"${metaData.name}-$MacArm"
      }
    case Environment.OperatingSystemType.Unknown =>
      logger.warn("Could not detect OS version! Defaulting to 'Linux'.")
      s"${metaData.name}-$Linux"
  }

  def executableDir(implicit metaData: AstGenProgramMetaData): String = {
    val dir        = metaData.packagePath.toString
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

  def hasCompatibleAstGenVersion(compatibleVersion: String)(implicit metaData: AstGenProgramMetaData): Boolean = {
    ExternalCommand.run(s"$metaData.name -version", ".").toOption.map(_.mkString.strip()) match {
      case Some(installedVersion)
          if installedVersion != "unknown" &&
            Try(VersionHelper.compare(installedVersion, compatibleVersion)).toOption.getOrElse(-1) >= 0 =>
        logger.debug(s"Using local ${metaData.name} v$installedVersion from systems PATH")
        true
      case Some(installedVersion) =>
        logger.debug(
          s"Found local ${metaData.name} v$installedVersion in systems PATH but ${metaData.name} requires at least v$compatibleVersion"
        )
        false
      case _ => false
    }
  }

}

trait AstGenRunnerBase(config: X2CpgConfig[_] with AstGenConfig[_]) {

  private val logger = LoggerFactory.getLogger(getClass)

  import io.joern.x2cpg.astgen.AstGenRunner.*

  protected def isIgnoredByUserConfig(filePath: String): Boolean = {
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

  private def filterFiles(files: List[String], out: File): List[String] = files.filter(fileFilter(_, out))

  protected def fileFilter(file: String, out: File): Boolean = {
    file.stripSuffix(".json").replace(out.pathAsString, config.inputPath) match {
      case filePath if isIgnoredByUserConfig(filePath) => false
      case _                                           => true
    }
  }

  protected def skippedFiles(in: File, astGenOut: List[String]): List[String]

  protected def runAstGenNative(in: String, out: File, exclude: String)(implicit
    metaData: AstGenProgramMetaData
  ): Try[Seq[String]]

  protected def astGenCommand(implicit metaData: AstGenProgramMetaData): String = {
    val conf          = ConfigFactory.load
    val astGenVersion = conf.getString(s"${metaData.configPrefix}.${metaData.name}_version")
    if (hasCompatibleAstGenVersion(astGenVersion)) {
      metaData.name
    } else {
      s"$executableDir/$executableName"
    }
  }

  def execute(out: File): AstGenRunnerResult = {
    implicit val metaData: AstGenProgramMetaData = config.astGenMetaData
    val in                                       = File(config.inputPath)
    logger.info(s"Running ${metaData.name} on '${config.inputPath}'")
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
        logger.error(s"\t- running ${metaData.name} failed!", f)
        AstGenRunnerResult()
    }
  }
}
