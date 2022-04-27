package io.joern.x2cpg.utils.dependency

import better.files.File.home
import org.gradle.tooling.GradleConnector
import org.slf4j.LoggerFactory

import java.nio.file.{Files, Path}
import java.util.stream.Collectors
import scala.jdk.CollectionConverters._

// TODO: try to find out the version of gradle and generate the initScript based on that
object GradleDependencies {
  private val logger         = LoggerFactory.getLogger(getClass)
  private val initScriptName = "x2cpg.init.gradle"
  private val taskName       = "x2cpgCopyRuntimeLibs"

  // TODO: support gradle4Minus (configurations.default won't work)
  private def gradle4OrLaterInitScript(destination: String): String = {
    s"""
     |allprojects {
     |  apply plugin: 'java'
     |  task $taskName(type: Copy) {
     |    into "$destination"
     |    from configurations.default
     |  }
     |}
     |""".stripMargin
  }

  private[dependency] def downloadRuntimeLibs(projectDir: Path): collection.Seq[String] = {
    val destinationDir = Files.createTempDirectory("x2cpgRuntimeLibs")
    destinationDir.toFile.deleteOnExit()

    // TODO: check if the directories exist
    // TODO: check if the build task already exists, and insert the init script only if it does not
    logger.info(s"Attempting to download runtime libs for project at '$projectDir' into '$destinationDir'...")

    val gradleInitDDir = home / ".gradle" / "init.d"
    try {
      if (!gradleInitDDir.exists) {
        logger.info(s"Creating gradle init script directory at '$gradleInitDDir'...")
        gradleInitDDir.createDirectories()
      }
    } catch {
      case t: Throwable =>
        logger.warn(s"Caught exception while trying to create init script directory: '$t'.")
    }

    try {
      val gradleInitScript = gradleInitDDir / initScriptName
      gradleInitScript.createFileIfNotExists()
      gradleInitScript.write(
        gradle4OrLaterInitScript(destinationDir.toString)
      ) // overwrite whatever is there, dirty solution, but also least likely to cause functional problems
      gradleInitScript.deleteOnExit()
    } catch {
      case t: Throwable =>
        // TODO: make sure this doesn't run if the previous step failed
        logger.warn(s"Caught exception while trying to create init script: '$t'.")
    }

    val connectionOption =
      try {
        logger.info(s"Establishing gradle connection for project directory at '$projectDir'...")
        Some(
          GradleConnector
            .newConnector()
            .forProjectDirectory(projectDir.toFile)
            .connect()
        )
      } catch {
        case t: Throwable =>
          logger.warn(s"Caught exception while trying to establish a Gradle connection: '$t'.")
          None
      }

    if (connectionOption.isDefined) {
      val connection = connectionOption.get
      try {
        logger.info(s"Executing gradle task '$taskName'...")
        connection
          .newBuild()
          .forTasks(taskName)
          // .setStandardOutput(System.out) // uncomment for debugging
          // .setStandardError(System.err)  // uncomment for debugging
          .run()
      } catch {
        case t: Throwable =>
          logger.warn(s"Caught exception while executing Gradle task: '${t.getMessage}'.")
      } finally {
        connection.close()
      }
    }

    Files.list(destinationDir).collect(Collectors.toList[Path]).asScala.map(_.toAbsolutePath.toString)
  }

  private[dependency] def isGradleBuild(codeDir: Path): Boolean = {
    Files
      .walk(codeDir)
      .filter(file => file.toString.endsWith(".gradle") || file.toString.endsWith(".gradle.kts"))
      .count > 0
  }

}
