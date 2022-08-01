package io.joern.javasrc2cpg.util

import better.files.File
import io.joern.x2cpg.utils.ExternalCommand
import org.slf4j.LoggerFactory

import java.nio.file.Paths
import scala.util.{Failure, Success, Try}

object Delombok {
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
    s"$javaPath -cp ${System.getProperty("java.class.path")} lombok.launch.Main delombok . -d ${tempDir.canonicalPath}"
  }

  def run(projectDir: String, analysisJavaHome: Option[String]): String = {
    Try(File.newTemporaryDirectory(prefix = "delombok").deleteOnExit()) match {
      case Success(tempDir) =>
        ExternalCommand.run(delombokToTempDirCommand(tempDir, analysisJavaHome), cwd = projectDir) match {
          case Success(_) =>
            tempDir.path.toAbsolutePath.toString

          case Failure(t) =>
            logger.warn(s"Executing delombok failed with $t")
            logger.warn("Creating AST with original source instead. Some methods and type information will be missing.")
            projectDir
        }

      case Failure(e) =>
        logger.warn(s"Failed to create temporary directory for delomboked source. Methods and types may be missing: $e")
        projectDir
    }
  }
}
