package io.joern.x2cpg.utils

import better.files.File.home
import org.gradle.tooling.GradleConnector

import java.io.{File => JFile}
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
    val gradleInitDDir = home / ".gradle" / "init.d"
    // TODO: check for permission errors / exceptions
    if (!gradleInitDDir.exists) {
      logger.info(s"Creating gradle init script directory at '$gradleInitDDir'...")
      gradleInitDDir.createDirectory()
    }
    val gradleInitScript = gradleInitDDir / initScriptName
    gradleInitScript.createFileIfNotExists()
    gradleInitScript.write(
      gradle4OrLaterInitScript(destinationDir)
    ) // overwrite whatever is there, dirty solution, but also least likely to cause functional problems

    logger.info(s"Establishing gradle connection for project directory at '$projectDirectory'...")
    val conn =
      GradleConnector
        .newConnector()
        .forProjectDirectory(new JFile(projectDirectory))
        .connect()
    logger.info(s"Executing gradle task '$taskName'...")
    conn
      .newBuild()
      .forTasks(taskName)
      // .setStandardOutput(System.out) // uncomment for debugging
      // .setStandardError(System.err)  // uncomment for debugging
      .run()
    conn.close()
  }
}
