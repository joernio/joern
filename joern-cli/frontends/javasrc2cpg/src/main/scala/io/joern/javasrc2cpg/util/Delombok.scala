package io.joern.javasrc2cpg.util

import better.files.File
import io.joern.x2cpg.utils.ExternalCommand
import io.joern.javasrc2cpg.util.Delombok.DelombokMode.*
import org.slf4j.LoggerFactory

import java.nio.file.Paths
import scala.util.{Failure, Success, Try}

object Delombok {

  sealed trait DelombokMode
  // Don't run delombok at all.
  object DelombokMode {
    case object NoDelombok  extends DelombokMode
    case object Default     extends DelombokMode
    case object TypesOnly   extends DelombokMode
    case object RunDelombok extends DelombokMode
  }

  private val logger = LoggerFactory.getLogger(this.getClass)

  private def systemJavaPath: String = {
    sys.env
      .get("JAVA_HOME")
      .flatMap { javaHome =>
        val javaExecutable = File(javaHome, "bin", "java")
        Option.when(javaExecutable.exists && javaExecutable.isExecutable) {
          javaExecutable.canonicalPath
        }
      }
      .getOrElse("java")
  }

  private def delombokToTempDirCommand(tempDir: File, analysisJavaHome: Option[String]) = {
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
    s"$javaPath -cp $classPathArg lombok.launch.Main delombok . -d ${tempDir.canonicalPath}"
  }

  def run(projectDir: String, analysisJavaHome: Option[String]): String = {
    Try(File.newTemporaryDirectory(prefix = "delombok").deleteOnExit()) match {
      case Success(tempDir) =>
        ExternalCommand.run(delombokToTempDirCommand(tempDir, analysisJavaHome), cwd = projectDir) match {
          case Success(_) =>
            tempDir.path.toAbsolutePath.toString

          case Failure(t) =>
            logger.warn(s"Executing delombok failed", t)
            logger.warn("Creating AST with original source instead. Some methods and type information will be missing.")
            projectDir
        }

      case Failure(e) =>
        logger.warn(s"Failed to create temporary directory for delomboked source. Methods and types may be missing", e)
        projectDir
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
