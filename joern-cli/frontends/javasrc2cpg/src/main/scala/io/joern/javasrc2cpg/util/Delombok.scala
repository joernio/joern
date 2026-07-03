package io.joern.javasrc2cpg.util

import io.joern.javasrc2cpg.util.Delombok.DelombokMode.*
import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.shiftleft.semanticcpg.utils.{ExternalCommand, FileUtil}
import org.slf4j.LoggerFactory

import java.io.File
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
  case class DelombokInvocation(projectDir: Path, relativePackageRoot: Path, absRoot: Path, absPeerRoots: Seq[Path])

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
    val ownEntries   = ownClasspath.split(File.pathSeparator).toList
    // Ask the classloader where lombok.launch.Main came from — this is more robust than a filename
    // heuristic, which breaks under packaging layouts that rename jars (e.g. sbt-native-packager's
    // `org.projectlombok.lombok-<version>.jar`) or fat-jar setups. `Class.forName` is used because
    // `lombok.launch.Main` is package-private and can't be referenced with `classOf`.
    val bundledLombokEntry: Option[String] = Try {
      val cls      = Class.forName("lombok.launch.Main")
      val location = cls.getProtectionDomain.getCodeSource.getLocation
      Paths.get(location.toURI).toAbsolutePath.normalize.toString
    }.toOption
    val (lombokEntries, otherEntries) = bundledLombokEntry match {
      case Some(entry) =>
        ownEntries.partition(e => Paths.get(e).toAbsolutePath.normalize.toString == entry)
      case None => (Nil, ownEntries)
    }
    if (lombokEntries.isEmpty) {
      logger.warn(
        "Could not locate joern's bundled lombok jar on the classpath. If a project dependency supplies a " +
          "lombok version incompatible with the analysis JDK, delombok may fail."
      )
    }
    // Prepend the bundled lombok so the classloader resolves it before any project-supplied lombok in
    // `dependencies`. Order: bundled lombok, project dependencies, everything else from ownClasspath.
    val fullClasspath = (lombokEntries ++ dependencies ++ otherEntries).mkString(File.pathSeparator)
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
    // `--sourcepath` can grow past Windows' command-length limit on large projects, so route it through
    // a `@argfile` the same way we do for `-cp`. The java launcher expands `@file` before dispatching
    // to `lombok.launch.Main`, so delombok itself sees an already-tokenised `--sourcepath <paths>`.
    val sourcepathArgs =
      if (peerSourceRoots.isEmpty) Nil
      else {
        val fullSourcePath = peerSourceRoots.map(_.toString).mkString(File.pathSeparator)
        Try(FileUtil.newTemporaryFile("sourcepath")) match {
          case Success(file) =>
            FileUtil.deleteOnExit(file)
            Files.writeString(file, s"--sourcepath\n$fullSourcePath")
            Seq(s"@${file.absolutePathAsString}")

          case Failure(t) =>
            logger.warn(
              s"Failed to create sourcepath file for delombok execution. Results may be missing on Windows systems",
              t
            )
            Seq("--sourcepath", fullSourcePath)
        }
      }
    val command =
      Seq(
        javaPath,
        "-cp",
        classPathArg,
        "lombok.launch.Main",
        "delombok",
        inputPath.absolutePathAsString,
        "--verbose"
      ) ++ sourcepathArgs ++ Seq("-d", outputDir.absolutePathAsString)
    logger.debug(s"Executing delombok with command ${command.mkString(" ")}")
    command
  }

  def delombokPackageRoot(
    delombokInvocation: DelombokInvocation,
    delombokTempDir: Path,
    analysisJavaHome: Option[String],
    dependencies: List[String]
  ): Try[String] = {
    val rootIsFile = Files.isRegularFile(delombokInvocation.projectDir.resolve(delombokInvocation.relativePackageRoot))
    val relativeOutputPath =
      if (rootIsFile) Option(delombokInvocation.relativePackageRoot.getParent).map(_.toString).getOrElse(".")
      else delombokInvocation.relativePackageRoot.toString
    val inputDir = delombokInvocation.projectDir.resolve(delombokInvocation.relativePackageRoot)

    val childPath = (delombokTempDir / relativeOutputPath).toAbsolutePath.normalize()

    Try(childPath.createWithParentsIfNotExists(asDirectory = true)).flatMap { packageOutputDir =>
      val result =
        ExternalCommand.run(
          delombokToTempDirCommand(
            inputDir,
            packageOutputDir,
            analysisJavaHome,
            dependencies,
            delombokInvocation.absPeerRoots
          )
        )
      if (!result.successful) {
        // Always log the unfiltered stdout/stderr if delombok exited with a non-zero code
        result.logIfFailed()
      } else {
        // Exit 0: filter out error logs that reference peer roots (these will be logged anyways when the peer is
        // delomboked), then log the remainder as a DEBUG if non-empty.
        val filteredStdErr = DelombokStderrFilter.filter(delombokInvocation.absRoot, result.stdErr)
        if (filteredStdErr.nonEmpty) {
          logger.debug(s"Delombok emitted diagnostics for $inputDir:\n${filteredStdErr.mkString("\n")}")
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
          "symbols from third-party libraries used in Lombok-annotated code. If you did not pass " +
          "--fetch-dependencies, re-run with it. If you did, dependency resolution may have failed — " +
          "check earlier logs."
      )
    }
    Try(Files.createTempDirectory("delombok")) match {
      case Failure(_) =>
        logger.warn(s"Could not create temporary directory for delombok output. Scanning original sources instead")
        DelombokRunResult(inputPath, false)

      case Success(tempDir) =>
        FileUtil.deleteOnExit(tempDir)
        val packageRoots         = PackageRootFinder.packageRootsFromFiles(inputPath, fileInfo).distinct
        val absolutePackageRoots = packageRoots.map(_.toAbsolutePath)
        packageRoots.par.foreach { case relativeRoot =>
          val absoluteRoot      = relativeRoot.toAbsolutePath
          val absolutePeerRoots = absolutePackageRoots.filterNot(_ == absoluteRoot)
          delombokPackageRoot(
            DelombokInvocation(inputPath, relativeRoot, absoluteRoot, absolutePeerRoots),
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
