package io.joern.x2cpg.astgen

import better.files.File
import com.typesafe.config.ConfigFactory
import io.joern.x2cpg.utils.Environment.ArchitectureType.ArchitectureType
import io.joern.x2cpg.utils.Environment.OperatingSystemType.OperatingSystemType
import io.joern.x2cpg.utils.{Environment, ExternalCommand}
import io.joern.x2cpg.{SourceFiles, X2CpgConfig}
import org.slf4j.LoggerFactory
import versionsort.VersionHelper

import java.net.URL
import java.nio.file.Paths
import scala.util.{Failure, Success, Try}

object AstGenRunner {

  private val logger = LoggerFactory.getLogger(getClass)

  trait AstGenRunnerResult {
    def parsedFiles: List[String]
    def skippedFiles: List[String]
  }

  /** @param parsedFiles
    *   the files parsed by the runner.
    * @param skippedFiles
    *   the files skipped by the runner.
    */
  case class DefaultAstGenRunnerResult(parsedFiles: List[String] = List.empty, skippedFiles: List[String] = List.empty)
      extends AstGenRunnerResult

  /** @param name
    *   the name of the AST gen executable, e.g., goastgen, dotnetastgen, swiftastgen, etc.
    * @param configPrefix
    *   the prefix of the executable's respective configuration path.
    * @param multiArchitectureBuilds
    *   whether there is a binary for specific architectures or not.
    * @param packagePath
    *   the code path for the frontend.
    */
  case class AstGenProgramMetaData(
    name: String,
    configPrefix: String,
    multiArchitectureBuilds: Boolean,
    packagePath: URL
  )

  def executableDir(implicit metaData: AstGenProgramMetaData): String =
    ExternalCommand
      .executableDir(Paths.get(metaData.packagePath.toURI))
      .resolve("astgen")
      .toString

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

trait AstGenRunnerBase(config: X2CpgConfig[?] & AstGenConfig[?]) {

  private val logger = LoggerFactory.getLogger(getClass)

  import io.joern.x2cpg.astgen.AstGenRunner.*

  // Suffixes for the binary based on OS & architecture
  protected val WinX86   = "win.exe"
  protected val WinArm   = "win-arm.exe"
  protected val LinuxX86 = "linux"
  protected val LinuxArm = "linux-arm"
  protected val MacX86   = "macos"
  protected val MacArm   = "macos-arm"

  /** All the supported combinations of architectures.
    */
  protected val SupportedBinaries: Set[(OperatingSystemType, ArchitectureType)] = Set(
    Environment.OperatingSystemType.Windows -> Environment.ArchitectureType.X86,
    Environment.OperatingSystemType.Windows -> Environment.ArchitectureType.ARMv8,
    Environment.OperatingSystemType.Linux   -> Environment.ArchitectureType.X86,
    Environment.OperatingSystemType.Linux   -> Environment.ArchitectureType.ARMv8,
    Environment.OperatingSystemType.Mac     -> Environment.ArchitectureType.X86,
    Environment.OperatingSystemType.Mac     -> Environment.ArchitectureType.ARMv8
  )

  /** Determines the name of the executable to run, based on the host system. Usually, AST GEN binaries support three
    * operating systems, and two architectures. Some binaries are multiplatform, in which case the suffix for x86 is
    * used for both architectures.
    */
  protected def executableName(implicit metaData: AstGenProgramMetaData): String = {
    if (!SupportedBinaries.contains(Environment.operatingSystem -> Environment.architecture)) {
      throw new UnsupportedOperationException(s"No compatible binary of ${metaData.name} for your operating system!")
    } else {
      Environment.operatingSystem match {
        case Environment.OperatingSystemType.Windows => executableName(WinX86, WinArm)
        case Environment.OperatingSystemType.Linux   => executableName(LinuxX86, LinuxArm)
        case Environment.OperatingSystemType.Mac     => executableName(MacX86, MacArm)
        case Environment.OperatingSystemType.Unknown =>
          logger.warn("Could not detect OS version! Defaulting to 'Linux'.")
          executableName(LinuxX86, LinuxArm)
      }
    }
  }

  private def executableName(x86Suffix: String, armSuffix: String)(implicit metaData: AstGenProgramMetaData): String = {
    if (metaData.multiArchitectureBuilds) {
      s"${metaData.name}-$x86Suffix"
    } else {
      Environment.architecture match {
        case Environment.ArchitectureType.X86   => s"${metaData.name}-$x86Suffix"
        case Environment.ArchitectureType.ARMv8 => s"${metaData.name}-$armSuffix"
      }
    }
  }

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

  protected def filterFiles(files: List[String], out: File): List[String] = files.filter(fileFilter(_, out))

  protected def fileFilter(file: String, out: File): Boolean = {
    file.stripSuffix(".json").replace(out.pathAsString, config.inputPath) match {
      case filePath if isIgnoredByUserConfig(filePath) => false
      case _                                           => true
    }
  }

  protected def skippedFiles(in: File, astGenOut: List[String]): List[String]

  protected def runAstGenNative(in: String, out: File, exclude: String, include: String)(implicit
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
    runAstGenNative(config.inputPath, out, config.ignoredFilesRegex.toString(), "") match {
      case Success(result) =>
        val srcFiles = SourceFiles.determine(
          out.toString(),
          Set(".json"),
          ignoredFilesRegex = Option(config.ignoredFilesRegex),
          ignoredFilesPath = Option(config.ignoredFiles)
        )
        val parsed  = filterFiles(srcFiles, out)
        val skipped = skippedFiles(in, result.toList)
        DefaultAstGenRunnerResult(parsed, skipped)
      case Failure(f) =>
        logger.error(s"\t- running ${metaData.name} failed!", f)
        DefaultAstGenRunnerResult()
    }
  }
}
