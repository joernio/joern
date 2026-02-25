package io.joern.x2cpg.astgen

import com.typesafe.config.ConfigFactory
import io.joern.x2cpg.astgen.AstGenRunner.AstGenProgramMetaData
import io.joern.x2cpg.utils.Environment
import io.joern.x2cpg.utils.Environment.{ArchitectureType, OperatingSystemType}
import io.joern.x2cpg.{SourceFiles, X2CpgConfig}
import io.shiftleft.semanticcpg.utils.ExternalCommand
import org.slf4j.LoggerFactory
import versionsort.VersionHelper

import java.net.URL
import java.nio.file.{Files, Path, Paths}
import scala.util.{Failure, Success, Try}

object AstGenRunner {

  private val logger = LoggerFactory.getLogger(classOf[AstGenRunner])

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
    *
    * This class must be abstract, because it determines the path of the ast gen binary from the class path location of
    * its derived class.
    */
  abstract class AstGenProgramMetaData(
    val name: String,
    val configPrefix: String,
    val multiArchitectureBuilds: Boolean = false
  ) {
    val packagePath: URL = getClass.getProtectionDomain.getCodeSource.getLocation
  }

  def isExecutableFile(filePath: String): Boolean = {
    Paths.get(filePath) match {
      case path if !Files.exists(path) =>
        logger.warn(s"File '$filePath' does not exist.")
        false
      case path if !Files.isRegularFile(path) =>
        logger.warn(s"File '$filePath' is not a regular file.")
        false
      case path if !Files.isExecutable(path) =>
        logger.warn(s"File '$filePath' is not executable.")
        false
      case _ =>
        true
    }
  }
}

abstract class AstGenRunner(metaData: AstGenProgramMetaData, config: X2CpgConfig[?]) {

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
  protected def executableName: String = {
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

  protected def executableName(x86Suffix: String, armSuffix: String): String = {
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
      case ignorePath if Files.isDirectory(Paths.get(ignorePath)) => filePath.startsWith(ignorePath)
      case ignorePath                                             => filePath == ignorePath
    }
    lazy val isInIgnoredFileRegex = config.ignoredFilesRegex.matches(filePath)
    if (isInIgnoredFiles || isInIgnoredFileRegex) {
      logger.debug(s"'$filePath' ignored by user configuration")
      true
    } else {
      false
    }
  }

  protected def filterFiles(files: List[String], out: Path): List[String] = files.filter(fileFilter(_, out))

  protected def fileFilter(file: String, out: Path): Boolean = {
    file.stripSuffix(".json").replace(out.toString, config.inputPath) match {
      case filePath if isIgnoredByUserConfig(filePath) => false
      case _                                           => true
    }
  }

  protected def skippedFiles(in: Path, astGenOut: List[String]): List[String]

  protected def runAstGenNative(in: String, out: Path, exclude: String, include: String): Try[Seq[String]]

  protected def astGenCommand: String = {
    val conf          = ConfigFactory.load
    val astGenVersion = conf.getString(s"${metaData.configPrefix}.${metaData.name}_version")
    if (hasCompatibleAstGenVersion(astGenVersion)) {
      metaData.name
    } else {
      val localPath = s"$executableDir/$executableName"
      if (AstGenRunner.isExecutableFile(localPath)) {
        localPath
      } else {
        logger.error(s"""Local ${metaData.name} binary not found at '$localPath' or is not executable!
             |Please make sure to have a compatible astgen version installed and available on this system.
             |""".stripMargin)
        scala.sys.exit(1)
      }
    }
  }

  def execute(out: Path): AstGenRunnerResult = {
    val in = Paths.get(config.inputPath)
    logger.info(s"Running ${metaData.name} on '${config.inputPath}'")
    runAstGenNative(config.inputPath, out, config.ignoredFilesRegex.toString(), "") match {
      case Success(result) =>
        val srcFiles = SourceFiles.determine(
          out.toString,
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

  def executableDir: String =
    ExternalCommand
      .executableDir(Paths.get(metaData.packagePath.toURI))
      .resolve("astgen")
      .toString

  def hasCompatibleAstGenVersion(compatibleVersion: String): Boolean = {
    ExternalCommand.run(Seq(metaData.name, "-version")).successOption.map(_.mkString.strip()) match {
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
      case _ =>
        false
    }
  }

}
