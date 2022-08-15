package io.joern.x2cpg.utils.dependency

import better.files.File
import io.joern.x2cpg.utils.ExternalCommand
import org.slf4j.LoggerFactory

import java.nio.file.Path
import scala.util.{Failure, Success}

object MavenDependencies {
  private val logger = LoggerFactory.getLogger(getClass)

  private def findMavenProjectRoots(currentDir: File): List[Path] = {
    val (childDirectories, childFiles) = currentDir.children.partition(_.isDirectory)
    childFiles.find(_.name == "pom.xml") match {
      case Some(pomFile) => pomFile.parent.path :: Nil

      case None if childDirectories.isEmpty => Nil

      case None => childDirectories.flatMap(findMavenProjectRoots).toList

    }
  }

  private[dependency] def get(projectDir: Path): List[String] = {
    val combinedDeps = findMavenProjectRoots(projectDir).flatMap { projectRoot =>
      // we can't use -Dmdep.outputFile because that keeps overwriting its own output for each sub-project it's running for
      val lines = ExternalCommand.run(
        s"mvn -B dependency:build-classpath -DincludeScope=compile -Dorg.slf4j.simpleLogger.defaultLogLevel=info -Dorg.slf4j.simpleLogger.logFile=System.out",
        projectRoot.toString
      ) match {
        case Success(lines) => lines
        case Failure(exception) =>
          logger.warn(
            s"Retrieval of compile class path via maven return with error.\n" +
              "The compile class path may be missing or partial.\n" +
              "Results will suffer from poor type information.\n\n" +
              exception.getMessage
          )
          // exception message is the program output - and we still want to look for potential partial results
          exception.getMessage.linesIterator.toSeq
      }

      var classPathNext = false
      val deps = lines
        .flatMap { line =>
          val isClassPathNow = classPathNext
          classPathNext = line.endsWith("Dependencies classpath:")

          if (isClassPathNow) line.split(':') else Array.empty[String]
        }
        .distinct
        .toList

      deps
    }
    logger.info("got {} Maven dependencies", combinedDeps.size)
    combinedDeps
  }

}
