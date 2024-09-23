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

case class ProjectNameInfo(projectName: String, isSubproject: Boolean) {
  override def toString: String = {
    if (isSubproject)
      s":$projectName"
    else
      projectName
  }

  def makeGradleTaskName(taskName: String): String = {
    if (isSubproject)
      s"$projectName:$taskName"
    else
      taskName
  }
}

case class GradleProjectInfo(
  subprojects: Map[ProjectNameInfo, List[String]],
  gradleVersion: String,
  hasAndroidSubproject: Boolean
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
}

case class GradleDepsInitScript(contents: String, taskName: String, destinationDir: Path)

object GradleDependencies {
  private val aarFileExtension            = "aar"
  private val gradleAndroidPropertyPrefix = "android"
  private val gradlePropertiesTaskName    = "properties"
  private val jarInsideAarFileName        = "classes.jar"
  private val defaultConfigurationName    = "releaseRuntimeClasspath"
  private val initScriptPrefix            = "x2cpg.init.gradle"
  private val taskNamePrefix              = "x2cpgCopyDeps"
  private val tempDirPrefix               = "x2cpgDependencies"
  private val defaultGradleAppName        = "app"

  private val logger = LoggerFactory.getLogger(getClass)

  // works with Gradle 5.1+ because the script makes use of `task.register`:
  //   https://docs.gradle.org/current/userguide/task_configuration_avoidance.html
  private def getInitScriptContent(taskName: String, destination: String, projectInfo: GradleProjectInfo): String = {
    val projectConfigurationString = projectInfo.subprojects
      .map { case (projectNameInfo, configurationNames) =>
        val quotedConfigurationNames = configurationNames.map(name => s"\"$name\"").mkString(", ")
        s"\"${projectNameInfo.projectName}\": [$quotedConfigurationNames]"
      }
      .mkString(", ")

    val taskCreationFunction = projectInfo.gradleVersionMajorMinor() match {
      case (major, minor) if major >= 5 && minor >= 1 => "tasks.register"
      case _                                          => "tasks.create"
    }

    val androidTaskDefinition = Option.when(projectInfo.hasAndroidSubproject)(s"""
           |def androidDepsCopyTaskName = taskName + "_androidDeps"
           |      $taskCreationFunction(androidDepsCopyTaskName, Copy) {
           |        duplicatesStrategy = 'include'
           |        into destinationDir
           |        from project.configurations.find { it.name.equals("androidApis") }
           |      }
           |""".stripMargin)

    val dependsOnAndroidTask = Option.when(projectInfo.hasAndroidSubproject)("dependsOn androidDepsCopyTaskName")

    s"""
       |allprojects {
       |  afterEvaluate { project ->
       |    def taskName = "$taskName"
       |    def destinationDir = "${destination.replaceAll("\\\\", "/")}"
       |    def gradleProjectConfigurations = [$projectConfigurationString]
       |
       |    if (gradleProjectConfigurations.containsKey(project.name)) {
       |      def gradleConfigurationNames = gradleProjectConfigurations.get(project.name)
       |      
       |      def compileDepsCopyTaskName = taskName + "_compileDeps"
       |      $taskCreationFunction(compileDepsCopyTaskName, Copy) {
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
       |      ${androidTaskDefinition.getOrElse("")}
       |      $taskCreationFunction(taskName, Copy) {
       |        ${dependsOnAndroidTask.getOrElse("")} 
       |        dependsOn compileDepsCopyTaskName
       |      }
       |    }
       |  }
       |}
       |""".stripMargin
  }

  private def makeInitScript(destinationDir: Path, projectInfo: GradleProjectInfo): GradleDepsInitScript = {
    val taskName = taskNamePrefix + "_" + (Random.alphanumeric take 8).toList.mkString
    val content  = getInitScriptContent(taskName, destinationDir.toString, projectInfo)
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

  private def getGradleProjectInfo(
    projectDir: Path,
    projectNameOverride: Option[String],
    configurationNameOverride: Option[String]
  ): Option[GradleProjectInfo] = {
    Try(makeConnection(projectDir.toFile)) match {
      case Success(gradleConnection) =>
        Using.resource(gradleConnection) { connection =>
          try {
            val buildEnv = connection.getModel[BuildEnvironment](classOf[BuildEnvironment])
            val project  = connection.getModel[GradleProject](classOf[GradleProject])

            val availableProjectNames = ProjectNameInfo(project.getName, false) :: project.getChildren.asScala
              .map(child => ProjectNameInfo(child.getName, true))
              .toList

            val availableProjectNamesString = availableProjectNames.mkString(" ")

            logger.debug(s"Found gradle project names ${availableProjectNames.mkString(" ")}")

            val selectedProjectNames = if (projectNameOverride.isDefined) {
              val overrideName = projectNameOverride.get
              availableProjectNames.find(_.projectName == overrideName) match {
                case Some(projectInfo) =>
                  logger.debug(s"Only fetching dependencies for overridden project name $overrideName")
                  projectInfo :: Nil

                case None =>
                  logger.warn(
                    s"Project name override was specified for dependency fetching ($overrideName), but no such project found."
                  )
                  logger.warn(
                    s"Falling back to fetching dependencies for all available project names: $availableProjectNamesString"
                  )
                  availableProjectNames
              }
            } else {
              availableProjectNames.find(_.projectName == defaultGradleAppName) match {
                case Some(defaultProjectInfo) =>
                  // TODO: This is a temporary check to avoid issues that could arise from subprojects using conflicting
                  //  versions of dependencies. Ideally dependencies for all of these projects will be fetched with
                  //  any conflicts handled in the consumer.
                  logger.debug(s"Found project with default name ($defaultGradleAppName)")
                  logger.debug(s"Fetching dependencies only for default project ($defaultGradleAppName)")
                  defaultProjectInfo :: Nil

                case None =>
                  logger.debug(s"No project name override or project with default name ($defaultGradleAppName) found.")
                  logger.debug(s"Fetching dependencies for all available projects: $availableProjectNamesString")
                  availableProjectNames
              }
            }

            val selectedConfigurations = selectedProjectNames.flatMap { projectNameInfo =>
              val dependenciesTaskName = projectNameInfo.makeGradleTaskName("dependencies")

              val availableConfigurations = runGradleTask(connection, dependenciesTaskName) match {
                case Some(out) =>
                  getConfigurationsWithDependencies(out) match {
                    case Nil =>
                      logger.debug(s"No configurations with dependencies found for project $projectNameInfo")
                      Nil
                    case deps =>
                      logger.debug(
                        s"Found the following configurations with dependencies for project $projectNameInfo: ${deps.mkString(", ")}"
                      )
                      deps
                  }
                case None =>
                  logger.warn(s"Failure executing dependencies task $dependenciesTaskName")
                  Nil
              }

              val availableConfigurationsString = availableConfigurations.mkString(", ")

              val selectedConfigurations = if (availableConfigurations.isEmpty) {
                // Skip logging below, since no available configurations already logged
                Nil
              } else if (configurationNameOverride.isDefined) {
                val overrideName = configurationNameOverride.get
                availableConfigurations.find(_ == overrideName) match {
                  case Some(configurationName) =>
                    logger.debug(s"Only fetching dependencies for overridden configuration $overrideName")
                    configurationName :: Nil

                  case None =>
                    logger.warn(
                      s"Configuration name override was specified for dependency fetching ($overrideName), but no such configuration found for project $projectNameInfo."
                    )
                    logger.warn(
                      s"Falling back to fetching dependencies for all available configurations: $availableConfigurationsString"
                    )
                    availableConfigurations
                }
              } else {
                availableConfigurations.find(_ == defaultConfigurationName) match {
                  case Some(defaultConfigurationName) =>
                    // TODO: This is a temporary check to avoid issues that could arise from subprojects using conflicting
                    //  versions of dependencies. Ideally dependencies for all of these configurations will be fetched with
                    //  any conflicts handled in the consumer.
                    logger.debug(
                      s"Found default configuration name ($defaultConfigurationName) for project $projectNameInfo"
                    )
                    logger.debug(
                      s"Fetching dependencies only for default configuration ($defaultConfigurationName) for project $projectNameInfo"
                    )
                    defaultConfigurationName :: Nil

                  case None =>
                    logger.debug(
                      s"No configuration override or configuration with default name ($defaultConfigurationName) found for project $projectNameInfo."
                    )
                    logger.debug(
                      s"Fetching dependencies for all available configurations for project $projectNameInfo: $availableConfigurationsString"
                    )
                    availableConfigurations
                }
              }

              Option.when(selectedConfigurations.nonEmpty) {
                projectNameInfo -> selectedConfigurations
              }
            }.toMap

            val includesAndroidProject = selectedProjectNames.exists { projectNameInfo =>
              val propertiesTaskName = projectNameInfo.makeGradleTaskName(gradlePropertiesTaskName)

              runGradleTask(connection, propertiesTaskName) match {
                case Some(out) =>
                  out.lines().iterator().asScala.exists(_.startsWith(gradleAndroidPropertyPrefix))
                case None => false
              }
            }

            val gradleVersion = buildEnv.getGradle.getGradleVersion

            val gradleProjectInfo = GradleProjectInfo(selectedConfigurations, gradleVersion, includesAndroidProject)

            Option(gradleProjectInfo)
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
    taskName: String,
    destinationDir: Path,
    initScriptPath: String
  ): Option[collection.Seq[String]] = {
    Using.resources(new ByteArrayOutputStream, new ByteArrayOutputStream) { case (stdoutStream, stderrStream) =>
      logger.debug(s"Executing gradle task '${taskName}'...")

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
          logger.info(s"Task $taskName resolved `${result.size}` dependency files.")
          Some(result)
        case Failure(ex) =>
          logger.warn(s"Caught exception while executing Gradle task: ${ex.getMessage}")
          val androidSdkError = "Define a valid SDK location with an ANDROID_HOME environment variable"
          if (stderrStream.toString.contains(androidSdkError)) {
            logger.warn(
              "A missing Android SDK configuration caused gradle dependency fetching failures. Please define a valid SDK location with an ANDROID_HOME environment variable or by setting the sdk.dir path in your project's local properties file"
            )
          }
          if (stderrStream.toString.contains("Could not compile initialization script")) {
            val scriptContents = File(initScriptPath).contentAsString
            logger.debug(
              s"########## INITIALIZATION_SCRIPT ##########\n$scriptContents\n###########################################"
            )
          }
          logger.debug(s"Gradle task execution stdout: \n$stdoutStream")
          logger.debug(s"Gradle task execution stderr: \n$stderrStream")
          None
      }
    }
  }

  private def extractClassesJarFromAar(aar: File): Option[Path] = {
    val newPath           = aar.path.toString.replaceFirst(aarFileExtension + "$", "jar")
    val aarUnzipDirSuffix = ".unzipped"
    val outDir            = File(aar.path.toString + aarUnzipDirSuffix)
    aar.unzipTo(outDir, _.getName == jarInsideAarFileName)
    val outFile = File(newPath)
    val classesJarEntries =
      outDir.listRecursively
        .filter(_.path.getFileName.toString == jarInsideAarFileName)
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
    getGradleProjectInfo(projectDir, projectNameOverride, configurationNameOverride) match {
      case Some(projectInfo) if projectInfo.gradleVersionMajorMinor()._1 < 5 =>
        logger.warn(s"Unsupported Gradle version `${projectInfo.gradleVersion}`")
        Map.empty

      case Some(projectInfo) =>
        Try(File.newTemporaryDirectory(tempDirPrefix).deleteOnExit()) match {
          case Success(destinationDir) =>
            Try(File.newTemporaryFile(initScriptPrefix).deleteOnExit()) match {
              case Success(initScriptFile) =>
                val initScript = makeInitScript(destinationDir.path, projectInfo)
                initScriptFile.write(initScript.contents)

                Try(makeConnection(projectDir.toFile)) match {
                  case Success(connection) =>
                    Using.resource(connection) { c =>
                      projectInfo.subprojects.keys.flatMap { projectNameInfo =>
                        val taskName = projectNameInfo.makeGradleTaskName(initScript.taskName)

                        runGradleTask(c, taskName, initScript.destinationDir, initScriptFile.pathAsString) map { deps =>
                          val depsOutput = deps.map { d =>
                            if (!d.endsWith(aarFileExtension)) d
                            else
                              extractClassesJarFromAar(File(d)) match {
                                case Some(path) => path.toString
                                case None       => d
                              }
                          }

                          projectNameInfo.projectName -> depsOutput
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
