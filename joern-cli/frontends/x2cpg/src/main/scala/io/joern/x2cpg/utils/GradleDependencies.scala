package io.joern.x2cpg.utils

import better.files.File
import org.gradle.tooling.GradleConnector

import java.io.{File => JFile}
import java.nio.file.Files
import org.slf4j.LoggerFactory

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

  def downloadRuntimeLibs(projectDirectory: String, destinationDir: String): Unit = {
    logger.info(s"Attempting to download runtime libs for project at '$projectDirectory' into '$destinationDir'...")

    val tmpDir = File(Files.createTempDirectory("x2cpg"))
    val initDir = (tmpDir / "init.d").createDirectoryIfNotExists()
    tmpDir.deleteOnExit()

    try {
      val gradleInitScript = initDir / initScriptName
      gradleInitScript.write(
        gradle4OrLaterInitScript(destinationDir)
      ) // overwrite whatever is there, dirty solution, but also least likely to cause functional problems
    } catch {
      case t: Throwable =>
        // TODO: make sure this doesn't run if the previous step failed
        logger.warn(s"Caught exception while trying to create init script: '${t.getMessage}'.")
    }

    val connectionOption =
      try {
        logger.info(s"Establishing gradle connection for project directory at '$projectDirectory'...")
        Some(
          GradleConnector
            .newConnector()
            .forProjectDirectory(new JFile(projectDirectory))
            .useGradleUserHomeDir(tmpDir.toJava)
            .connect()
        )
      } catch {
        case t: Throwable =>
          logger.warn(s"Caught exception while trying to estabish a Gradle connection: '${t.getMessage}'.")
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
  }
}
