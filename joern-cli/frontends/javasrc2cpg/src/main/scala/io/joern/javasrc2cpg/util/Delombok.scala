package io.joern.javasrc2cpg.util

import io.joern.javasrc2cpg.util.Delombok.DelombokMode.*
import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.shiftleft.semanticcpg.utils.{ExternalCommand, FileUtil}
import org.slf4j.LoggerFactory

import java.nio.file.{Files, Path, Paths}
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.collection.parallel.CollectionConverters.*

object Delombok {

  enum DelombokMode {
    case NoDelombok // Don't run delombok at all.
    case Default
    case TypesOnly
    case RunDelombok
  }

  case class DelombokRunResult(path: Path, isDelombokedPath: Boolean)

  /** Per-invocation inputs for [[delombokPackageRoot]]. All absolute paths are already normalised. */
  case class DelombokInvocation(
    projectDir: Path,
    relativePackageRoot: Path,
    absRoot: Path,
    absPeerRoots: Seq[Path],
    fqnIndex: DelombokStderrFilter.FqnIndex
  )

  private val logger = LoggerFactory.getLogger(this.getClass)

  private def systemJavaPath: String = {
    sys.env
      .get("JAVA_HOME")
      .flatMap { javaHome =>
        val javaExecutable = Paths.get(javaHome, "bin", "java")

        Option.when(Files.exists(javaExecutable) && Files.isExecutable(javaExecutable)) {
          javaExecutable.absolutePathAsString
        }
      }
      .getOrElse("java")
  }

  private def delombokToTempDirCommand(
    inputPath: Path,
    outputDir: Path,
    analysisJavaHome: Option[String],
    dependencies: List[String],
    peerSourceRoots: Seq[Path]
  ) = {
    val javaPath     = analysisJavaHome.getOrElse(systemJavaPath)
    val ownClasspath = System.getProperty("java.class.path")
    val fullClasspath =
      if (dependencies.isEmpty) ownClasspath
      else (dependencies :+ ownClasspath).mkString(java.io.File.pathSeparator)
    val classPathArg = Try(FileUtil.newTemporaryFile("classpath")) match {
      case Success(file) =>
        FileUtil.deleteOnExit(file)
        // Write classpath to a file to work around Windows length limits.
        Files.writeString(file, fullClasspath)
        s"@${file.absolutePathAsString}"

      case Failure(t) =>
        logger.warn(
          s"Failed to create classpath file for delombok execution. Results may be missing on Windows systems",
          t
        )
        fullClasspath
    }
    val sourcepathArgs =
      if (peerSourceRoots.isEmpty) Nil
      else
        Seq(
          "--sourcepath",
          peerSourceRoots.map(_.toString).mkString(java.io.File.pathSeparator)
        )
    val command =
      Seq(
        javaPath,
        "-cp",
        classPathArg,
        "lombok.launch.Main",
        "delombok",
        inputPath.absolutePathAsString
      ) ++ sourcepathArgs ++ Seq("-d", outputDir.absolutePathAsString)
    logger.debug(s"Executing delombok with command ${command.mkString(" ")}")
    command
  }

  def delombokPackageRoot(
    inv: DelombokInvocation,
    delombokTempDir: Path,
    analysisJavaHome: Option[String],
    dependencies: List[String]
  ): Try[String] = {
    val rootIsFile = Files.isRegularFile(inv.projectDir.resolve(inv.relativePackageRoot))
    val relativeOutputPath =
      if (rootIsFile) Option(inv.relativePackageRoot.getParent).map(_.toString).getOrElse(".")
      else inv.relativePackageRoot.toString
    val inputDir = inv.projectDir.resolve(inv.relativePackageRoot)

    val childPath = (delombokTempDir / relativeOutputPath).toAbsolutePath.normalize()

    Try(childPath.createWithParentsIfNotExists(asDirectory = true)).flatMap { packageOutputDir =>
      val result =
        ExternalCommand.run(
          delombokToTempDirCommand(inputDir, packageOutputDir, analysisJavaHome, dependencies, inv.absPeerRoots)
        )
      if (!result.successful) {
        // Always log the stdout/stderr if delombok exited with a non-zero code
        result.logIfFailed()
      } else {
        // Exit 0: filter out `cannot find symbol` records that reference peer roots (false
        // positives for this invocation), then log the remainder as a WARN if non-empty. Fail-open:
        // if the filter itself throws, fall back to logging the original stderr.
        val filteredStdErr =
          Try(DelombokStderrFilter.filter(inv.absPeerRoots.toSet, inv.fqnIndex, result.stdErr)) match {
            case Success(filtered) => filtered
            case Failure(err) =>
              logger.warn("DelombokStderrFilter threw; falling back to unfiltered stderr", err)
              result.stdErr
          }
        if (filteredStdErr.nonEmpty) {
          logger.warn(s"Delombok emitted diagnostics for $inputDir:\n${filteredStdErr.mkString("\n")}")
        }
      }
      // Call `toTry` on the original result so the failure message (used downstream) still
      // contains the full unfiltered stderr.
      result.toTry.map(_ => delombokTempDir.absolutePathAsString)
    }
  }

  def run(
    inputPath: Path,
    fileInfo: List[SourceParser.FileInfo],
    analysisJavaHome: Option[String],
    dependencies: List[String]
  ): DelombokRunResult = {
    if (dependencies.isEmpty) {
      logger.warn(
        "Running delombok without any project dependencies on the classpath. Delombok may fail to resolve " +
          "symbols from third-party libraries used in Lombok-annotated code. Re-run with --fetch-dependencies " +
          "to make project dependencies available to delombok."
      )
    }
    Try(Files.createTempDirectory("delombok")) match {
      case Failure(_) =>
        logger.warn(s"Could not create temporary directory for delombok output. Scanning original sources instead")
        DelombokRunResult(inputPath, false)

      case Success(tempDir) =>
        FileUtil.deleteOnExit(tempDir)
        val packageRoots = PackageRootFinder.packageRootsFromFiles(inputPath, fileInfo).distinct
        val fqnIndex     = DelombokStderrFilter.FqnIndex.build(inputPath, fileInfo, packageRoots)
        val absRoots     = fqnIndex.absoluteRoots
        packageRoots.zip(absRoots).par.foreach { case (relativeRoot, absRoot) =>
          val absPeerRoots = absRoots.filterNot(_ == absRoot)
          delombokPackageRoot(
            DelombokInvocation(inputPath, relativeRoot, absRoot, absPeerRoots, fqnIndex),
            tempDir,
            analysisJavaHome,
            dependencies
          )
        }
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
