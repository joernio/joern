package io.joern.swiftsrc2cpg.utils

import io.joern.swiftsrc2cpg.Config
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.utils.Environment
import io.shiftleft.semanticcpg.utils.FileUtil
import io.shiftleft.semanticcpg.utils.FileUtil.*
import org.slf4j.LoggerFactory

import java.nio.file.{Files, Path, Paths}
import java.util.regex.Pattern
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.util.matching.Regex

object AstGenRunner {

  private val logger = LoggerFactory.getLogger(getClass)

  private val AstGenDefaultIgnoreRegex: Seq[Regex] =
    List(
      s"\\..*${Pattern.quote(java.io.File.separator)}.*".r,
      s"__.*${Pattern.quote(java.io.File.separator)}.*".r,
      s"tests${Pattern.quote(java.io.File.separator)}.*".r,
      s"specs${Pattern.quote(java.io.File.separator)}.*".r,
      s"test${Pattern.quote(java.io.File.separator)}.*".r,
      s"spec${Pattern.quote(java.io.File.separator)}.*".r
    )

  case class AstGenRunnerResult(parsedFiles: List[String] = List.empty, skippedFiles: List[String] = List.empty)

  // full path to the SwiftAstGen binary from the env var SWIFTASTGEN_BIN
  private val AstGenBin: Option[String] = scala.util.Properties.envOrNone("SWIFTASTGEN_BIN").flatMap {
    case path if Files.isDirectory(Paths.get(path)) => Option((Paths.get(path) / "SwiftAstGen").toString)
    case path if Files.exists(Paths.get(path))      => Option(Paths.get(path).toString)
    case _                                          => None
  }

  lazy private val executableName = Environment.operatingSystem match {
    case Environment.OperatingSystemType.Windows => "SwiftAstGen-win.exe"
    case Environment.OperatingSystemType.Linux   => "SwiftAstGen-linux"
    case Environment.OperatingSystemType.Mac     => "SwiftAstGen-mac"
    case Environment.OperatingSystemType.Unknown =>
      logger.warn("Could not detect OS version! Defaulting to 'Linux'.")
      "SwiftAstGen-linux"
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

  private def hasCompatibleAstGenVersionAtPath(path: Option[String]): Boolean = {
    val astGenCommand = path.getOrElse("SwiftAstGen")
    val localPath     = path.flatMap(Paths.get(_).parentOption.map(_.toString)).getOrElse(".")
    val debugMsgPath  = path.getOrElse("PATH")
    ExternalCommand.run(Seq(astGenCommand, "-h"), localPath).toOption match {
      case Some(_) =>
        logger.debug(s"Using SwiftAstGen from $debugMsgPath")
        true
      case _ =>
        false
    }
  }

  /** @return
    *   the full path to the astgen binary found on the system
    */
  private def compatibleAstGenPath(): String = {
    AstGenBin match
      // 1. case: we try it at env var SWIFTASTGEN_BIN
      case Some(path) if hasCompatibleAstGenVersionAtPath(Option(path)) =>
        path
      // 2. case: we try it with the systems PATH
      case _ if hasCompatibleAstGenVersionAtPath(None) =>
        "SwiftAstGen"
      // otherwise: we use the default local SwiftAstGen executable path
      case _ =>
        logger.debug(
          s"Did not find any SwiftAstGen binary on this system (environment variable SWIFTASTGEN_BIN not set and no entry in the systems PATH)"
        )
        val localPath = s"$executableDir/$executableName"
        logger.debug(s"Using SwiftAstGen from '$localPath'")
        localPath
  }

  private lazy val astGenCommand = compatibleAstGenPath()
}

class AstGenRunner(config: Config) {

  import io.joern.swiftsrc2cpg.utils.AstGenRunner._

  private def skippedFiles(astGenOut: List[String]): List[String] = {
    val skipped = astGenOut.collect {
      case out if !out.startsWith("Generated") =>
        val filename = out.substring(out.indexOf(": `") + 3, out.indexOf("swift`") + 5)
        val reason   = out.substring(out.indexOf("` ") + 2)
        logger.warn(s"\t- failed to parse '$filename': '$reason'")
        Option(filename)
      case out =>
        logger.debug(s"\t+ $out")
        None
    }
    skipped.flatten
  }

  private def isIgnoredByUserConfig(filePath: String): Boolean = {
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

  private def filterFiles(files: List[String], out: Path): List[String] = {
    files.filter { file =>
      file.stripSuffix(".json").replace(out.toString, config.inputPath) match {
        case filePath if isIgnoredByUserConfig(filePath) => false
        case _                                           => true
      }
    }
  }

  private def runAstGenNative(in: Path, out: Path): Try[Seq[String]] =
    ExternalCommand.run(Seq(astGenCommand, "-o", out.toString), in.toString)

  private def checkParsedFiles(files: List[String], in: Path): List[String] = {
    val numOfParsedFiles = files.size
    logger.info(s"Parsed $numOfParsedFiles files.")
    if (numOfParsedFiles == 0) {
      logger.warn("You may want to check the DEBUG logs for a list of files that are ignored by default.")
      SourceFiles.determine(in.toString, Set(".swift"), ignoredDefaultRegex = Option(AstGenDefaultIgnoreRegex))
    }
    files
  }

  def execute(out: Path): AstGenRunnerResult = {
    val in = Paths.get(config.inputPath)
    logger.info(s"Running SwiftAstGen in '$in' ...")
    runAstGenNative(in, out) match {
      case Success(result) =>
        val parsed  = checkParsedFiles(filterFiles(SourceFiles.determine(out.toString, Set(".json")), out), in)
        val skipped = skippedFiles(result.toList)
        AstGenRunnerResult(parsed, skipped)
      case Failure(f) =>
        logger.error("\t- running SwiftAstGen failed!", f)
        AstGenRunnerResult()
    }
  }

}
