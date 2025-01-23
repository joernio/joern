package io.joern.x2cpg.utils.dependency

import io.joern.x2cpg.utils.ExternalCommand
import org.slf4j.LoggerFactory

import java.nio.file.Path
import scala.util.{Failure, Success}

object MavenDependencies {
  private val logger = LoggerFactory.getLogger(getClass)

  private val MavenCliOpts = "MAVEN_CLI_OPTS"
  // we can't use -Dmdep.outputFile because that keeps overwriting its own output for each sub-project it's running for
  // also separate this from fetchCommandWithOpts to log a version that clearly separates options we provide from
  // options specified by the user via the MAVEN_CLI_OPTS environment variable, while also making it clear that this
  // environment variable is being considered.
  private val fetchArgs =
    Vector(
      "--fail-never",
      "-B",
      "dependency:build-classpath",
      "-DincludeScope=compile",
      "-Dorg.slf4j.simpleLogger.defaultLogLevel=info",
      "-Dorg.slf4j.simpleLogger.logFile=System.out"
    )

  private val fetchCommandWithOpts: Seq[String] = {
    // These options suppress output, so if they're provided we won't get any results.
    // "-q" and "--quiet" are the only ones that would realistically be used.
    val optionsToStrip = Set("-h", "--help", "-q", "--quiet", "-v", "--version")

    val cli = org.apache.commons.exec.CommandLine("mvn")
    cli.addArguments(System.getenv(MavenCliOpts), false) // a null from getenv() does not add any argument

    cli.toStrings.toIndexedSeq.filterNot(optionsToStrip.contains) ++ fetchArgs
  }

  private def logErrors(output: String): Unit = {

    logger.warn(
      s"Retrieval of compile class path via maven return with error.\n" +
        "The compile class path may be missing or partial.\n" +
        "Results will suffer from poor type information.\n" +
        "To fix this issue, please ensure that the below command can be executed successfully from the project root directory:\n" +
        s"mvn $$$MavenCliOpts " + fetchArgs.mkString(" ") + "\n\n"
    )
    logger.debug(s"Full maven error output:\n$output")
  }

  private[dependency] def get(projectDir: Path): Option[collection.Seq[String]] = {
    val lines = ExternalCommand.run(fetchCommandWithOpts, projectDir.toString).toTry match {
      case Success(lines) =>
        if (lines.contains("[INFO] Build failures were ignored.")) {
          logErrors(lines.mkString(System.lineSeparator()))
        }

        lines
      case Failure(exception) =>
        logErrors(exception.getMessage)
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

    logger.info("got {} Maven dependencies", deps.size)
    Some(deps)
  }
}
