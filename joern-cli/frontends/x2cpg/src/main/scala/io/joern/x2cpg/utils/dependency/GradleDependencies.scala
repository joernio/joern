package io.joern.x2cpg.utils.dependency

import better.files._
import org.gradle.tooling.{GradleConnector}
import org.gradle.tooling.model.GradleProject
import org.gradle.tooling.model.build.BuildEnvironment
import org.slf4j.LoggerFactory

import java.io.ByteArrayOutputStream
import java.nio.file.{Files, Path}
import java.util.stream.Collectors
import scala.jdk.CollectionConverters._
import scala.util.Random

case class GradleProjectInfo(
  gradleVersion: String,
  gradleHome: String,
  tasks: Seq[String],
  hasAndroidSubproject: Boolean = false
) {
  def gradleVersionMajorMinor(): (Int, Int) = {
    def isValidPart(part: String) = part.forall(Character.isDigit)
    val parts                     = gradleVersion.split('.')
    if (parts.size == 1 && isValidPart(parts(0))) {
      (parts(0).toInt, 0)
    } else if (parts.size >= 2 && isValidPart(parts(0)) && isValidPart(parts(1))) {
      (parts(0).toInt, parts(1).toInt)
    } else {
      (-1, -1)
    }
  }
}

case class GradleInitScript(contents: String, taskName: String)

object GradleDependencies {
  private val logger           = LoggerFactory.getLogger(getClass)
  private val initScriptPrefix = "x2cpg.init.gradle"
  private val taskNamePrefix   = "x2cpgCopyDeps"
  private val tempDirPrefix    = "x2cpgDependencies"

  // works with Gradle 5.1+ because the script makes use of `task.register`:
  //   https://docs.gradle.org/current/userguide/task_configuration_avoidance.html
  private def gradle5OrLaterAndroidInitScript(
    taskName: String,
    destination: String,
    gradleProjectName: String,
    gradleConfigurationName: String
  ): String = {
    s"""
       |allprojects {
       |  afterEvaluate { project ->
       |    def taskName = "$taskName"
       |    def destinationDir = "$destination"
       |    def gradleProjectName = "$gradleProjectName"
       |    def gradleConfigurationName = "$gradleConfigurationName"
       |    if (project.name.equals(gradleProjectName)) {
       |      def compileDepsCopyTaskName = taskName + "_compileDeps"
       |      tasks.register(compileDepsCopyTaskName, Copy) {
       |        duplicatesStrategy = 'include'
       |        into destinationDir
       |        from project.configurations.find { it.name.equals(gradleConfigurationName) }
       |      }
       |      def androidDepsCopyTaskName = taskName + "_androidDeps"
       |      tasks.register(androidDepsCopyTaskName, Copy) {
       |        duplicatesStrategy = 'include'
       |        into destinationDir
       |        from project.configurations.find { it.name.equals("androidApis") }
       |      }
       |      tasks.register(taskName, Copy) {
       |        dependsOn androidDepsCopyTaskName
       |        dependsOn compileDepsCopyTaskName
       |      }
       |    }
       |  }
       |}
       |""".stripMargin
  }

  // this init script _should_ work with Gradle 4-8, but has not been tested thoroughly
  // TODO: add test cases for older Gradle versions
  private def gradle5OrLaterInitScript(taskName: String, destination: String): String = {
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

  private def makeInitScript(destinationDir: Path, forAndroid: Boolean): GradleInitScript = {
    val taskName = taskNamePrefix + "_" + (Random.alphanumeric take 8).toList.mkString
    val content =
      if (forAndroid) {
        val gradleProjectName       = "app"                     // TODO: make configurable via CLI flag
        val gradleConfigurationName = "releaseCompileClasspath" // TODO: make configurable via CLI flag
        gradle5OrLaterAndroidInitScript(taskName, destinationDir.toString, gradleProjectName, gradleConfigurationName)
      } else {
        gradle5OrLaterInitScript(taskName, destinationDir.toString)
      }
    GradleInitScript(content, taskName)
  }

  // fetch the gradle project information first, then invoke a newly-defined gradle task to copy the necessary jars into
  // a destination directory.
  private[dependency] def get(projectDir: Path): collection.Seq[String] = {
    val gradleProjectInfoOption =
      try {
        logger.info(s"Attempting to fetch gradle project information from path `$projectDir`.")
        val connection = GradleConnector
          .newConnector()
          .forProjectDirectory(projectDir.toFile)
          .connect()
        val buildEnv = connection.getModel[BuildEnvironment](classOf[BuildEnvironment])
        val project  = connection.getModel[GradleProject](classOf[GradleProject])

        val hasAndroidPrefixGradleProperty =
          try {
            val out                      = new ByteArrayOutputStream()
            val gradlePropertiesTaskName = "properties"
            connection
              .newBuild()
              .forTasks(gradlePropertiesTaskName)
              .setStandardOutput(out)
              .run()
            out.close()
            val gradleAndroidPropertyPrefix = "android."
            !out.toString.split('\n').filter(_.startsWith(gradleAndroidPropertyPrefix)).isEmpty
          } catch {
            case _: Throwable =>
              logger.warn("Caught exception while executing Gradle task named `properties`.")
              false
          }
        val info = GradleProjectInfo(
          buildEnv.getGradle.getGradleVersion,
          buildEnv.getGradle.getGradleUserHome.toString,
          project.getTasks.asScala.map(_.getName).toSeq,
          hasAndroidPrefixGradleProperty
        )
        connection.close()
        Some(info)
      } catch {
        case t: Throwable =>
          logger.warn(s"Caught exception while trying fetch gradle project information: `$t`.")
          None
      }
    if (gradleProjectInfoOption.isEmpty) {
      throw new Exception("Could not fetch Gradle project information.")
    }

    val gradleProjectInfo       = gradleProjectInfoOption.get
    val (gradleVersionMajor, _) = gradleProjectInfo.gradleVersionMajorMinor()
    if (gradleVersionMajor < 5) {
      logger.warn(s"Found unsupported Gradle version `${gradleProjectInfo.gradleVersion}`.")
      throw new Exception("Unsupported Gradle version `" + gradleProjectInfo.gradleVersion + "`")
    }

    logger.info(s"Creating gradle init script...")
    val destinationDir = Files.createTempDirectory(tempDirPrefix)
    destinationDir.toFile.deleteOnExit()
    val initScript = makeInitScript(destinationDir, gradleProjectInfo.hasAndroidSubproject)

    val initScriptFile = File.newTemporaryFile(initScriptPrefix).deleteOnExit()
    initScriptFile.write(initScript.contents)

    logger.info(s"Downloading dependencies for project to '$projectDir'...")
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
        logger.info(s"Executing gradle task '${initScript.taskName}'...")
        connection
          .newBuild()
          .forTasks(initScript.taskName)
          .withArguments("--init-script", initScriptFile.pathAsString)
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

}
