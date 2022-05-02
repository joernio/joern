package io.joern.x2cpg.utils.dependency

import io.joern.x2cpg.utils.ExternalCommand
import org.slf4j.LoggerFactory

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.util.{Failure, Success}

object MavenDependencies {
  private val logger = LoggerFactory.getLogger(getClass)

  private[dependency] def get(projectDir: Path): List[String] = {
    val tmpFile = Files.createTempFile("mvnClassPass", ".txt")
    tmpFile.toFile.deleteOnExit()
    ExternalCommand.run(
      s"mvn dependency:build-classpath -DincludeScope=compile -Dmdep.outputFile=$tmpFile",
      projectDir.toString
    ) match {
      case Success(_) =>
        val classPath = new String(Files.readAllBytes(tmpFile), StandardCharsets.UTF_8)
        classPath.split(":").toList
      case Failure(exception) =>
        logger.warn(
          s"Unable to retrieve compile class path from maven." +
            s"\n Results will suffer from poor type information.\n${exception.getMessage}"
        )
        Nil
    }
  }

  private[dependency] def isMavenBuild(codeDir: Path): Boolean = {
    Files.exists(codeDir.resolve("pom.xml"))
  }
}
