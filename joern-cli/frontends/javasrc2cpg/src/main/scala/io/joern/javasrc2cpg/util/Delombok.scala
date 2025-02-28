package io.joern.javasrc2cpg.util

import better.files.File
import io.joern.javasrc2cpg.util.Delombok.DelombokMode.*
import io.joern.x2cpg.utils.FileUtil
import io.joern.x2cpg.utils.FileUtil.*
import io.shiftleft.semanticcpg.utils.ExternalCommand
import org.slf4j.LoggerFactory

import java.nio.file.{Files, Path, Paths}
import scala.util.Failure
import scala.util.Success
import scala.util.Try

object Delombok {

  sealed trait DelombokMode
  // Don't run delombok at all.
  object DelombokMode {
    case object NoDelombok  extends DelombokMode
    case object Default     extends DelombokMode
    case object TypesOnly   extends DelombokMode
    case object RunDelombok extends DelombokMode
  }

  case class DelombokRunResult(path: Path, isDelombokedPath: Boolean)

  private val logger = LoggerFactory.getLogger(this.getClass)

  private def systemJavaPath: String = {
    sys.env
      .get("JAVA_HOME")
      .flatMap { javaHome =>
        val javaExecutable = Paths.get(javaHome, "bin", "java")

        Option.when(Files.exists(javaExecutable) && Files.isExecutable(javaExecutable)) {
          javaExecutable.toAbsolutePath.toString
        }
      }
      .getOrElse("java")
  }

  private def delombokToTempDirCommand(inputPath: Path, outputDir: Path, analysisJavaHome: Option[String]) = {
    val javaPath = analysisJavaHome.getOrElse(systemJavaPath)
    val classPathArg = Try(File.newTemporaryFile("classpath").deleteOnExit()) match {
      case Success(file) =>
        // Write classpath to a file to work around Windows length limits.
        file.write(System.getProperty("java.class.path"))
        s"@${file.canonicalPath}"

      case Failure(t) =>
        logger.warn(
          s"Failed to create classpath file for delombok execution. Results may be missing on Windows systems",
          t
        )
        System.getProperty("java.class.path")
    }
    val command =
      Seq(
        javaPath,
        "-cp",
        classPathArg,
        "lombok.launch.Main",
        "delombok",
        inputPath.toAbsolutePath.toString,
        "-d",
        outputDir.toAbsolutePath.toString
      )
    logger.debug(s"Executing delombok with command ${command.mkString(" ")}")
    command
  }

  def delombokPackageRoot(
    projectDir: Path,
    relativePackageRoot: Path,
    delombokTempDir: Path,
    analysisJavaHome: Option[String]
  ): Try[String] = {
    val rootIsFile = Files.isRegularFile(projectDir.resolve(relativePackageRoot))
    val relativeOutputPath =
      if (rootIsFile) Option(relativePackageRoot.getParent).map(_.toString).getOrElse(".")
      else relativePackageRoot.toString
    val inputDir = projectDir.resolve(relativePackageRoot)

    val childPath = delombokTempDir / relativeOutputPath

    Try(childPath.createWithParentsIfNotExists(asDirectory = true)).flatMap { packageOutputDir =>
      ExternalCommand
        .run(delombokToTempDirCommand(inputDir, packageOutputDir, analysisJavaHome), Some("."))
        .toTry
        .map(_ => delombokTempDir.toAbsolutePath.toString)
    }
  }

  def run(
    inputPath: Path,
    fileInfo: List[SourceParser.FileInfo],
    analysisJavaHome: Option[String]
  ): DelombokRunResult = {
    Try(Files.createTempDirectory("delombok")) match {
      case Failure(_) =>
        logger.warn(s"Could not create temporary directory for delombok output. Scanning original sources instead")
        DelombokRunResult(inputPath, false)

      case Success(tempDir) =>
        FileUtil.deleteOnExit(tempDir)
        PackageRootFinder
          .packageRootsFromFiles(inputPath, fileInfo)
          .foreach(delombokPackageRoot(inputPath, _, tempDir, analysisJavaHome))
        DelombokRunResult(tempDir, true)
    }
  }

  def parseDelombokModeOption(delombokModeStr: Option[String]): DelombokMode = {
    delombokModeStr.map(_.toLowerCase) match {
      case None                 => Default
      case Some("no-delombok")  => NoDelombok
      case Some("default")      => Default
      case Some("types-only")   => TypesOnly
      case Some("run-delombok") => RunDelombok
      case Some(value) =>
        logger.warn(s"Found unrecognised delombok mode `$value`. Using default instead.")
        Default
    }
  }
}
