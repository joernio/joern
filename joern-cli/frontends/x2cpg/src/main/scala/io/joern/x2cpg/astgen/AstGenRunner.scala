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
    * @param binEnvVar
    *   optional environment variable that may point to a user-provided binary location, e.g. `ASTGEN_BIN`.
    * @param versionFlag
    *   the CLI flag used to query the binary's version (or any flag whose successful exit indicates a working binary).
    * @param skipVersionComparison
    *   if true, only the successful exit of the version probe is required - the output is not compared against the
    *   configured version. Useful for binaries that do not expose a comparable version string.
    * @param versionConfigKey
    *   optional override for the `application.conf` key holding the required binary version. Defaults to
    *   `${configPrefix}.${name}_version` when not provided.
    *
    * This class must be abstract, because it determines the path of the ast gen binary from the class path location of
    * its derived class.
    */
  abstract class AstGenProgramMetaData(
    val name: String,
    val configPrefix: String,
    val multiArchitectureBuilds: Boolean = false,
    val binEnvVar: Option[String] = None,
    val versionFlag: String = "-version",
    val skipVersionComparison: Boolean = false,
    versionConfigKey: Option[String] = None
  ) {
    val packagePath: URL                 = getClass.getProtectionDomain.getCodeSource.getLocation
    val resolvedVersionConfigKey: String = versionConfigKey.getOrElse(s"$configPrefix.${name}_version")
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

  // Lazy so path resolution and the version probe run at most once per runner instance.
  protected lazy val astGenCommand: String = {
    val conf          = ConfigFactory.load
    val astGenVersion = conf.getString(metaData.resolvedVersionConfigKey)
    val resolvedPath  = resolveAstGenPath(astGenVersion)
    logger.info(s"Using ${metaData.name} from '$resolvedPath'")
    resolvedPath
  }

  /** Resolution order:
    *
    *   - user-provided path via `metaData.binEnvVar` (if set and binary passes the version probe)
    *   - binary on the system `PATH` named `metaData.name` (if it passes the version probe)
    *   - bundled binary at `$executableDir/$executableName`
    */
  private def resolveAstGenPath(astGenVersion: String): String = {
    binFromEnvVar
      .filter(path => hasCompatibleAstGenVersion(astGenVersion, Option(path)))
      .orElse(Option.when(hasCompatibleAstGenVersion(astGenVersion, None))(metaData.name))
      .getOrElse {
        val localPath = s"$executableDir/$executableName"
        if (AstGenRunner.isExecutableFile(localPath)) {
          localPath
        } else {
          val envHint =
            metaData.binEnvVar
              .map(envVar => s" or set the environment variable $envVar to a working binary")
              .getOrElse("")
          logger.error(s"""Local ${metaData.name} binary not found at '$localPath' or is not executable!
               |Please make sure to have a compatible ${metaData.name} version installed and available on this system$envHint.
               |""".stripMargin)
          scala.sys.exit(1)
        }
      }
  }

  private def binFromEnvVar: Option[String] = metaData.binEnvVar.flatMap(scala.util.Properties.envOrNone).flatMap {
    case path if Files.isDirectory(Paths.get(path)) => Some(Paths.get(path).resolve(metaData.name).toString)
    case path if Files.exists(Paths.get(path))      => Some(Paths.get(path).toString)
    case _                                          => None
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

  def hasCompatibleAstGenVersion(compatibleVersion: String): Boolean =
    hasCompatibleAstGenVersion(compatibleVersion, None)

  def hasCompatibleAstGenVersion(compatibleVersion: String, path: Option[String]): Boolean = {
    val command      = path.getOrElse(metaData.name)
    val workingDir   = path.flatMap(binPath => Option(Paths.get(binPath).getParent))
    val debugMsgPath = path.getOrElse("PATH")
    ExternalCommand.run(Seq(command, metaData.versionFlag), workingDir).successOption match {
      case Some(_) if metaData.skipVersionComparison =>
        logger.debug(s"Using ${metaData.name} from '$debugMsgPath'")
        true
      case Some(out) =>
        val installedVersion = out.mkString.strip()
        if (
          installedVersion != "unknown" &&
          Try(VersionHelper.compare(installedVersion, compatibleVersion)).toOption.getOrElse(-1) >= 0
        ) {
          logger.debug(s"Using ${metaData.name} v$installedVersion from '$debugMsgPath'")
          true
        } else {
          logger.debug(
            s"Found ${metaData.name} v$installedVersion in '$debugMsgPath' but ${metaData.name} requires at least v$compatibleVersion"
          )
          false
        }
      case _ =>
        false
    }
  }

}
