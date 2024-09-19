package io.joern.x2cpg.utils.dependency

import better.files.*
import org.gradle.tooling.{GradleConnector, ProjectConnection}
import org.gradle.tooling.model.{GradleProject, ProjectIdentifier, Task}
import org.gradle.tooling.model.build.BuildEnvironment
import org.slf4j.LoggerFactory

import java.io.ByteArrayOutputStream
import java.nio.file.{Files, Path}
import java.io.File as JFile
import java.util.stream.Collectors
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Random, Success, Try, Using}

case class GradleProjectInfo(
  projectConfigurationInfo: Map[String, List[String]],
  gradleVersion: String,
  tasks: Seq[String],
  hasAndroidSubproject: Boolean = false
) {
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

  def projectsWithDependencies: List[String] = projectConfigurationInfo.collect {
    case (projectName, dependencies) if dependencies.nonEmpty => projectName
  }.toList

  def configurationNames: List[String] = projectConfigurationInfo.values.flatten.toSet.toList
}

object Constants {
  val aarFileExtension            = "aar"
  val gradleAndroidPropertyPrefix = "android"
  val gradlePropertiesTaskName    = "properties"
  val jarInsideAarFileName        = "classes.jar"
}

case class GradleDepsInitScript(contents: String, taskName: String, destinationDir: Path)

object GradleDependencies {
  private val logger                                     = LoggerFactory.getLogger(getClass)
  private val initScriptPrefix                           = "x2cpg.init.gradle"
  private val taskNamePrefix                             = "x2cpgCopyDeps"
  private val tempDirPrefix                              = "x2cpgDependencies"
  private val defaultAndroidConfigurationName            = "releaseRuntimeClasspath"
  private val defaultRuntimeConfigurationName            = "runtimeClasspath"
  private[dependency] val defaultGradleConfigurationName = "compileClasspath"
  private[dependency] val defaultGradleAppName           = "app"

  // works with Gradle 5.1+ because the script makes use of `task.register`:
  //   https://docs.gradle.org/current/userguide/task_configuration_avoidance.html
  private def gradle5OrLaterAndroidInitScript(
    taskName: String,
    destination: String,
    gradleProjectNames: List[String],
    gradleConfigurationNames: List[String],
    projectInfo: GradleProjectInfo
  ): String = {
    val projectNamesString = gradleProjectNames.map(name => s"\"$name\"").mkString(",")
    // TODO: This logic exists to avoid potential issues with conflicting dependencies in different configurations
    //  since only a single default or given configuration will be chosen if available. If no obvious configuration can
    //  be found, fetch dependencies for multiple.
    val allTargetsHaveDefaultConfiguration =
      projectInfo.projectConfigurationInfo.values.iterator.forall(_.contains(defaultAndroidConfigurationName))
    val configurationNamesString =
      if (allTargetsHaveDefaultConfiguration && gradleConfigurationNames.contains(defaultAndroidConfigurationName))
        defaultAndroidConfigurationName
      else
        gradleConfigurationNames.map(name => s"\"$name\"").mkString(",")

    logger.info(s"Creating gradle android init script for configurations $configurationNamesString")
    s"""
       |allprojects {
       |  afterEvaluate { project ->
       |    def taskName = "$taskName"
       |    def destinationDir = "${destination.replaceAll("\\\\", "/")}"
       |    def gradleProjectNames = [$projectNamesString]
       |    def gradleConfigurationNames = [$configurationNamesString]
       |
       |    if (gradleProjectNames.contains(project.name)) {
       |      def compileDepsCopyTaskName = taskName + "_compileDeps"
       |      tasks.register(compileDepsCopyTaskName, Copy) {
       |
       |        def selectedConfigs = project.configurations.findAll {
       |          configuration -> gradleConfigurationNames.contains(configuration.getName())
       |        }
       |
       |        def componentIds = []
       |        if (!selectedConfigs.isEmpty()) {
       |          for (selectedConfig in selectedConfigs) {
       |            componentIds = selectedConfig.incoming.resolutionResult.allDependencies.findAll {
       |              dep -> dep instanceof org.gradle.api.internal.artifacts.result.DefaultResolvedDependencyResult
       |            } .collect { it.selected.id }
       |          }
       |        }
       |
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

  // this init script _should_ work with Gradle >=4, but has not been tested thoroughly
  // TODO: add test cases for older Gradle versions
  private def gradle5OrLaterInitScript(
    taskName: String,
    destination: String,
    gradleConfigurationNames: List[String]
  ): String = {
    val into = destination.replaceAll("\\\\", "/")
    // TODO: This logic exists to avoid potential issues with conflicting dependencies in different configurations
    //  since only a single default or given configuration will be chosen if available. If no obvious configuration can
    //  be found, fetch dependencies for multiple.
    val configurationNames =
      if (gradleConfigurationNames.contains(defaultRuntimeConfigurationName))
        defaultRuntimeConfigurationName :: Nil
      else
        defaultRuntimeConfigurationName :: gradleConfigurationNames

    logger.info(s"Creating non-android gradle init script for configuration names ${configurationNames.mkString(", ")}")
    val fromConfigurations = configurationNames
      .map(configuration => s"from configurations.$configuration")
      .toSet
      .mkString("\n")
    s"""
     |allprojects {
     |  apply plugin: 'java'
     |  task $taskName(type: Copy) {
     |    $fromConfigurations
     |    into "$into"
     |  }
     |}
     |""".stripMargin
  }

  private def makeInitScript(
    destinationDir: Path,
    forAndroid: Boolean,
    gradleProjectNames: List[String],
    gradleConfigurationNames: List[String],
    projectInfo: GradleProjectInfo
  ): GradleDepsInitScript = {
    val taskName = taskNamePrefix + "_" + (Random.alphanumeric take 8).toList.mkString
    val content =
      if (forAndroid) {
        gradle5OrLaterAndroidInitScript(
          taskName,
          destinationDir.toString,
          gradleProjectNames,
          gradleConfigurationNames,
          projectInfo
        )
      } else {
        gradle5OrLaterInitScript(taskName, destinationDir.toString, gradleConfigurationNames)
      }

    GradleDepsInitScript(content, taskName, destinationDir)
  }

  private[dependency] def makeConnection(projectDir: JFile): ProjectConnection = {
    GradleConnector.newConnector().forProjectDirectory(projectDir).connect()
  }

  private def getConfigurationsWithDependencies(dependenciesOutput: String): List[String] = {
    // TODO: this is a heuristic for matching configuration names based on a sample of open source projects.
    //  either add more options to this or revise the approach completely if this turns out to miss too much.
    val configurationNameRegex = raw"(\S*([rR]elease|[rR]untime)\S*) -.+$$".r
    val lines                  = dependenciesOutput.lines.iterator().asScala
    val results                = mutable.Set[String]()

    while (lines.hasNext) {
      val line = lines.next()
      line match {
        case configurationNameRegex(configurationName, _) if lines.hasNext =>
          val next = lines.next()
          if (next != "No dependencies") {
            results.addOne(configurationName)
          }
          lines.takeWhile(_.nonEmpty)

        case _ =>
          lines.takeWhile(_.nonEmpty)
      }
    }

    results.filterNot(_.toLowerCase.contains("test")).toList
  }

  private def getGradleProjectInfo(projectDir: Path): Option[GradleProjectInfo] = {
    Try(makeConnection(projectDir.toFile)) match {
      case Success(gradleConnection) =>
        Using.resource(gradleConnection) { connection =>
          try {
            val buildEnv = connection.getModel[BuildEnvironment](classOf[BuildEnvironment])
            val project  = connection.getModel[GradleProject](classOf[GradleProject])
            val allProjectNames =
              project.getName :: project.getChildren.asScala.map(child => s":${child.getName}").toList
            val allPropertiesTasksNames = Constants.gradlePropertiesTaskName :: project.getChildren.asScala
              .map(child => s"${child.getName}:${Constants.gradlePropertiesTaskName}")
              .toList
            val hasAndroidPrefixGradleProperty = allPropertiesTasksNames.exists { taskName =>
              runGradleTask(connection, taskName) match {
                case Some(out) =>
                  out.split('\n').exists(_.startsWith(Constants.gradleAndroidPropertyPrefix))
                case None => false
              }
            }
            val configurationInfo = allProjectNames.map { projectName =>
              val dependenciesTaskName = if (projectName.startsWith(":")) {
                projectName.tail ++ ":dependencies"
              } else {
                "dependencies"
              }
              val configurations = runGradleTask(connection, dependenciesTaskName) match {
                case Some(out) =>
                  getConfigurationsWithDependencies(out)
                case None => Nil
              }

              projectName -> configurations
            }.toMap
            val info = GradleProjectInfo(
              configurationInfo,
              buildEnv.getGradle.getGradleVersion,
              project.getTasks.asScala.map(_.getName).toSeq,
              hasAndroidPrefixGradleProperty
            )
            if (hasAndroidPrefixGradleProperty) {
              val validProjectNames = List(project.getName) ++ project.getChildren.getAll.asScala.map(_.getName)
              logger.debug(s"Found Gradle projects: ${validProjectNames.mkString(",")}")
              Some(info)
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
    projectName: String,
    taskName: String,
    destinationDir: Path,
    initScriptPath: String
  ): Option[collection.Seq[String]] = {
    Using.resources(new ByteArrayOutputStream, new ByteArrayOutputStream) { case (stdoutStream, stderrStream) =>
      logger.info(s"Executing gradle task '${taskName}'...")

      Try(
        connection
          .newBuild()
          .forTasks(taskName)
          .withArguments("--init-script", initScriptPath)
          .setStandardOutput(stdoutStream)
          .setStandardError(stderrStream)
          .run()
      ) match {
        case Success(_) =>
          val result =
            Files
              .list(destinationDir)
              .collect(Collectors.toList[Path])
              .asScala
              .map(_.toAbsolutePath.toString)
          logger.info(s"Resolved `${result.size}` dependency files.")
          Some(result)
        case Failure(ex) =>
          logger.warn(s"Caught exception while executing Gradle task: ${ex.getMessage}")
          val androidSdkError = "Define a valid SDK location with an ANDROID_HOME environment variable"
          if (stderrStream.toString.contains(androidSdkError)) {
            logger.warn(
              "A missing Android SDK configuration caused gradle dependency fetching failures. Please define a valid SDK location with an ANDROID_HOME environment variable or by setting the sdk.dir path in your project's local properties file"
            )
          }
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
    projectNameOverride: Option[String],
    configurationNameOverride: Option[String]
  ): Map[String, collection.Seq[String]] = {
    logger.info(s"Fetching Gradle project information at path `$projectDir`.")
    getGradleProjectInfo(projectDir) match {
      case Some(projectInfo) if projectInfo.gradleVersionMajorMinor()._1 < 5 =>
        logger.warn(s"Unsupported Gradle version `${projectInfo.gradleVersion}`")
        Map.empty
      case Some(projectInfo) =>
        Try(File.newTemporaryDirectory(tempDirPrefix).deleteOnExit()) match {
          case Success(destinationDir) =>
            Try(File.newTemporaryFile(initScriptPrefix).deleteOnExit()) match {
              case Success(initScriptFile) =>
                val projectNames =
                  projectNameOverride
                    .map(List(_))
                    .getOrElse(projectInfo.projectsWithDependencies.map(_.stripPrefix(":")))

                val configurationNames =
                  configurationNameOverride.map(List(_)).getOrElse(projectInfo.configurationNames)
                val initScript =
                  makeInitScript(
                    destinationDir.path,
                    projectInfo.hasAndroidSubproject,
                    projectNames,
                    configurationNames,
                    projectInfo
                  )
                initScriptFile.write(initScript.contents)

                projectNameOverride.foreach(name => logger.debug(s"Found overridden project name: $name"))
                configurationNameOverride.foreach(name => logger.debug(s"Found overridden configuration name: $name"))
                logger.debug(
                  "Detected following project names and configurations with dependencies (ignored if overridden):"
                )
                projectInfo.projectConfigurationInfo.foreach { case (projectName, configurations) =>
                  logger.debug(s"- $projectName: ${configurations.mkString(", ")}")
                }
                Try(makeConnection(projectDir.toFile)) match {
                  case Success(connection) =>
                    Using.resource(connection) { c =>
                      projectNames.flatMap { projectName =>
                        val taskName =
                          if (projectInfo.projectConfigurationInfo.contains(s":$projectName"))
                            s"$projectName:${initScript.taskName}"
                          else
                            initScript.taskName

                        val x = 2
                        runGradleTask(
                          c,
                          projectName,
                          taskName,
                          initScript.destinationDir,
                          initScriptFile.pathAsString
                        ) map { deps =>
                          val depsOutput = deps.map { d =>
                            if (!d.endsWith(Constants.aarFileExtension)) d
                            else
                              extractClassesJarFromAar(File(d)) match {
                                case Some(path) => path.toString
                                case None       => d
                              }
                          }

                          projectName -> depsOutput
                        }
                      }.toMap
                    }
                  case Failure(ex) =>
                    logger.warn(s"Caught exception while trying to establish a Gradle connection: ${ex.getMessage}")
                    logger.debug(s"Full exception: ", ex)
                    Map.empty
                }
              case Failure(ex) =>
                logger.warn(s"Could not create temporary file for Gradle init script: ${ex.getMessage}")
                logger.debug(s"Full exception: ", ex)
                Map.empty
            }
          case Failure(ex) =>
            logger.warn(s"Could not create temporary directory for saving dependency files: ${ex.getMessage}")
            logger.debug("Full exception: ", ex)
            Map.empty
        }
      case None =>
        logger.warn("Could not fetch Gradle project information")
        Map.empty
    }
  }
}
