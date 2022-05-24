package io.joern.x2cpg.utils.dependency

import io.joern.x2cpg.utils.ExternalCommand
import org.slf4j.LoggerFactory

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.util.{Failure, Success, Try}

object MavenDependencies {
  private val logger = LoggerFactory.getLogger(getClass)

  private[dependency] def get(projectDir: Path): List[String] = {
    val tmpFile = Files.createTempFile("mvnClassPass", ".txt")
    tmpFile.toFile.deleteOnExit()
    ExternalCommand.run(
      s"mvn -B dependency:build-classpath -DincludeScope=compile -Dmdep.outputFile=$tmpFile",
      projectDir.toString
    ) match {
      case Success(_) =>
      case Failure(exception) =>
        logger.warn(
          s"Retrieval of compile class path via maven return with error.\n" +
            "The compile class path may be missing or partial.\n" +
            "Results will suffer from poor type information.\n" +
            s"Error: ${exception.getMessage}"
        )
    }

    Try {
      // Regardless of the external maven comment error status we try to read the
      // output file because sometimes it contains partial results.
      val classPath = new String(Files.readAllBytes(tmpFile), StandardCharsets.UTF_8)
      if (classPath.isEmpty) {
        Nil
      } else {
        classPath.split(":").toList
      }
    }.getOrElse(Nil)
  }

}
