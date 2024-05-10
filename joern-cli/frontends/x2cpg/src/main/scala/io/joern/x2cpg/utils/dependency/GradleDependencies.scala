package io.joern.x2cpg.utils.dependency

import better.files._
import org.gradle.tooling.{GradleConnector, ProjectConnection}
import org.gradle.tooling.model.GradleProject
import org.gradle.tooling.model.build.BuildEnvironment
import org.slf4j.LoggerFactory

import java.io.ByteArrayOutputStream
import java.nio.file.{Files, Path}
import java.io.{File => JFile}
import java.util.stream.Collectors
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Random, Success, Try, Using}

case class GradleProjectInfo(gradleVersion: String, tasks: Seq[String], hasAndroidSubproject: Boolean = false) {
  def gradleVersionMajorMinor(): (Int, Int) = {
    def isValidPart(part: String) = part.forall(Character.isDigit)
    val parts                     = gradleVersion.split('.')
    if (parts.length == 1 && isValidPart(parts(0))) {
      (parts(0).toInt, 0)
    } else if (parts.length >= 2 && isValidPart(parts(0)) && isValidPart(parts(1))) {
      (parts(0).toInt, parts(1).toInt)
    } else {
      (-1, -1)
    }
  }
}

object Constants {
  val aarFileExtension            = "aar"
  val gradleAndroidPropertyPrefix = "android."
  val gradlePropertiesTaskName    = "properties"
  val jarInsideAarFileName        = "classes.jar"
}

case class GradleDepsInitScript(contents: String, taskName: String, destinationDir: Path)

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
       |    def destinationDir = "${destination.replaceAll("\\\\", "/")}"
       |    def gradleProjectName = "$gradleProjectName"
       |    def gradleConfigurationName = "$gradleConfigurationName"
       |
       |    if (project.name.equals(gradleProjectName)) {
       |      def compileDepsCopyTaskName = taskName + "_compileDeps"
       |      tasks.register(compileDepsCopyTaskName, Copy) {
       |        def selectedConfig = project.configurations.find { it.name.equals(gradleConfigurationName) }
       |        def componentIds = []
       |        if (selectedConfig != null) {
       |          componentIds = selectedConfig.incoming.resolutionResult.allDependencies.collect { it.selected.id }
       |        }
       |        def result = dependencies.createArtifactResolutionQuery()
       |                                 .forComponents(componentIds)
       |                                 .withArtifacts(JvmLibrary, SourcesArtifact)
       |                                 .execute()
       |        duplicatesStrategy = 'include'
       |        into destinationDir
       |        from result.resolvedComponents.collect { it.getArtifacts(SourcesArtifact).collect { it.file } }
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
  private def gradle5OrLaterInitScript(
    taskName: String,
    destination: String,
    gradleConfigurationName: String
  ): String = {
    s"""
     |allprojects {
     |  apply plugin: 'java'
     |  task $taskName(type: Copy) {
     |    configurations.$gradleConfigurationName.setCanBeResolved(true)
     |    configurations.implementation.setCanBeResolved(true)
     |    configurations.default.setCanBeResolved(true)
     |    into "${destination.replaceAll("\\\\", "/")}"
     |    from configurations.default
     |  }
     |}
     |""".stripMargin
  }

  private def makeInitScript(
    destinationDir: Path,
    forAndroid: Boolean,
    gradleProjectName: String,
    gradleConfigurationName: String
  ): GradleDepsInitScript = {
    val taskName = taskNamePrefix + "_" + (Random.alphanumeric take 8).toList.mkString
    val content =
      if (forAndroid) {
        gradle5OrLaterAndroidInitScript(taskName, destinationDir.toString, gradleProjectName, gradleConfigurationName)
      } else {
        gradle5OrLaterInitScript(taskName, destinationDir.toString, gradleConfigurationName)
      }
    GradleDepsInitScript(content, taskName, destinationDir)
  }

  private[dependency] def makeConnection(projectDir: JFile): ProjectConnection = {
    GradleConnector.newConnector().forProjectDirectory(projectDir).connect()
  }

  private def getGradleProjectInfo(projectDir: Path, projectName: String): Option[GradleProjectInfo] = {
    Try(makeConnection(projectDir.toFile)) match {
      case Success(gradleConnection) =>
        Using.resource(gradleConnection) { connection =>
          try {
            val buildEnv = connection.getModel[BuildEnvironment](classOf[BuildEnvironment])
            val project  = connection.getModel[GradleProject](classOf[GradleProject])
            val hasAndroidPrefixGradleProperty =
              runGradleTask(connection, Constants.gradlePropertiesTaskName) match {
                case Some(out) =>
                  out.split('\n').exists(_.startsWith(Constants.gradleAndroidPropertyPrefix))
                case None => false
              }
            val info = GradleProjectInfo(
              buildEnv.getGradle.getGradleVersion,
              project.getTasks.asScala.map(_.getName).toSeq,
              hasAndroidPrefixGradleProperty
            )
            if (hasAndroidPrefixGradleProperty) {
              val validProjectNames = List(project.getName) ++ project.getChildren.getAll.asScala.map(_.getName)
              logger.debug(s"Found Gradle projects: ${validProjectNames.mkString(",")}")
              if (!validProjectNames.contains(projectName)) {
                val validProjectNamesStr = validProjectNames.mkString(",")
                logger.warn(
                  s"The provided Gradle project name `$projectName` is is not part of the valid project names: `$validProjectNamesStr`"
                )
                None
              } else {
                Some(info)
              }
            } else {
              Some(info)
            }
          } catch {
            case t: Throwable =>
              logger.warn(s"Caught exception while trying use Gradle connection: ${t.getMessage}")
              logger.debug(s"Full exception: ", t)
              None
          }
        }
      case Failure(t) =>
        logger.warn(s"Caught exception while trying fetch Gradle project information: ${t.getMessage}")
        logger.debug(s"Full exception: ", t)
        None
    }
  }

  private def runGradleTask(connection: ProjectConnection, taskName: String): Option[String] = {
    Using.resource(new ByteArrayOutputStream()) { out =>
      Try(
        connection
          .newBuild()
          .forTasks(taskName)
          .setStandardOutput(out)
          .run()
      ) match {
        case Success(_) => Some(out.toString)
        case Failure(ex) =>
          logger.warn(s"Caught exception while executing Gradle task named `$taskName`:", ex.getMessage)
          logger.debug(s"Full exception: ", ex)
          None
      }
    }
  }

  private def runGradleTask(
    connection: ProjectConnection,
    initScript: GradleDepsInitScript,
    initScriptPath: String
  ): Option[collection.Seq[String]] = {
    Using.resources(new ByteArrayOutputStream, new ByteArrayOutputStream) { case (stdoutStream, stderrStream) =>
      logger.info(s"Executing gradle task '${initScript.taskName}'...")
      Try(
        connection
          .newBuild()
          .forTasks(initScript.taskName)
          .withArguments("--init-script", initScriptPath)
          .setStandardOutput(stdoutStream)
          .setStandardError(stderrStream)
          .run()
      ) match {
        case Success(_) =>
          val result =
            Files
              .list(initScript.destinationDir)
              .collect(Collectors.toList[Path])
              .asScala
              .map(_.toAbsolutePath.toString)
          logger.info(s"Resolved `${result.size}` dependency files.")
          Some(result)
        case Failure(ex) =>
          logger.warn(s"Caught exception while executing Gradle task: ${ex.getMessage}")
          logger.debug(s"Gradle task execution stdout: \n$stdoutStream")
          logger.debug(s"Gradle task execution stderr: \n$stderrStream")
          None
      }
    }
  }

  private def extractClassesJarFromAar(aar: File): Option[Path] = {
    val newPath           = aar.path.toString.replaceFirst(Constants.aarFileExtension + "$", "jar")
    val aarUnzipDirSuffix = ".unzipped"
    val outDir            = File(aar.path.toString + aarUnzipDirSuffix)
    aar.unzipTo(outDir, _.getName == Constants.jarInsideAarFileName)
    val outFile = File(newPath)
    val classesJarEntries =
      outDir.listRecursively
        .filter(_.path.getFileName.toString == Constants.jarInsideAarFileName)
        .toList
    if (classesJarEntries.size != 1) {
      logger.warn(s"Found aar file without `classes.jar` inside at path ${aar.path}")
      outDir.delete()
      None
    } else {
      val classesJar = classesJarEntries.head
      logger.trace(s"Copying `classes.jar` for aar at `${aar.path.toString}` into `$newPath`")
      classesJar.copyTo(outFile)
      outDir.delete()
      aar.delete()
      Some(outFile.path)
    }
  }

  // fetch the gradle project information first, then invoke a newly-defined gradle task to copy the necessary jars into
  // a destination directory.
  private[dependency] def get(
    projectDir: Path,
    projectName: String,
    configurationName: String
  ): Option[collection.Seq[String]] = {
    logger.info(s"Fetching Gradle project information at path `$projectDir` with project name `$projectName`.")
    getGradleProjectInfo(projectDir, projectName) match {
      case Some(projectInfo) if projectInfo.gradleVersionMajorMinor()._1 < 5 =>
        logger.warn(s"Unsupported Gradle version `${projectInfo.gradleVersion}`")
        None
      case Some(projectInfo) =>
        Try(File.newTemporaryDirectory(tempDirPrefix).deleteOnExit()) match {
          case Success(destinationDir) =>
            Try(File.newTemporaryFile(initScriptPrefix).deleteOnExit()) match {
              case Success(initScriptFile) =>
                val initScript =
                  makeInitScript(destinationDir.path, projectInfo.hasAndroidSubproject, projectName, configurationName)
                initScriptFile.write(initScript.contents)

                logger.info(
                  s"Downloading dependencies for configuration `$configurationName` of project `$projectName` at `$projectDir` into `$destinationDir`..."
                )
                Try(makeConnection(projectDir.toFile)) match {
                  case Success(connection) =>
                    Using.resource(connection) { c =>
                      runGradleTask(c, initScript, initScriptFile.pathAsString) match {
                        case Some(deps) =>
                          Some(deps.map { d =>
                            if (!d.endsWith(Constants.aarFileExtension)) d
                            else
                              extractClassesJarFromAar(File(d)) match {
                                case Some(path) => path.toString
                                case None       => d
                              }
                          })
                        case None => None
                      }
                    }
                  case Failure(ex) =>
                    logger.warn(s"Caught exception while trying to establish a Gradle connection: ${ex.getMessage}")
                    logger.debug(s"Full exception: ", ex)
                    None
                }
              case Failure(ex) =>
                logger.warn(s"Could not create temporary file for Gradle init script: ${ex.getMessage}")
                logger.debug(s"Full exception: ", ex)
                None
            }
          case Failure(ex) =>
            logger.warn(s"Could not create temporary directory for saving dependency files: ${ex.getMessage}")
            logger.debug("Full exception: ", ex)
            None
        }
      case None =>
        logger.warn("Could not fetch Gradle project information")
        None
    }
  }
}
