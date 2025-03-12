package io.joern.x2cpg.utils.dependency

import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.shiftleft.semanticcpg.utils.FileUtil

import java.nio.charset.Charset
import org.gradle.tooling.{GradleConnector, ProjectConnection}
import org.gradle.tooling.model.{GradleProject, ProjectIdentifier, Task}
import org.gradle.tooling.model.build.BuildEnvironment
import org.slf4j.LoggerFactory

import java.io.{ByteArrayOutputStream, IOException, File as JFile}
import java.nio.file.{Files, Path, Paths}
import java.util.stream.Collectors
import scala.collection.mutable
import scala.io.Source
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Random, Success, Try, Using}

case class ProjectNameInfo(projectName: String, parentName: Option[String]) {
  override def toString: String = {
    parentName.map(parentName => s"$parentName:$projectName").getOrElse(projectName)
  }

  def makeGradleTaskName(taskName: String): String = {
    parentName.map(parentName => s"$parentName:$projectName:$taskName").getOrElse(taskName)
  }
}

case class GradleVersion(major: Int, minor: Int)

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
  private def getInitScriptContent(
    taskName: String,
    destinationDir: String,
    gradleVersion: GradleVersion,
    projectNameOverride: Option[String],
    configurationNameOverride: Option[String]
  ): String = {
    val taskCreationFunction = gradleVersion match {
      case GradleVersion(major, minor) if major >= 5 && minor >= 1 => "tasks.register"
      case _                                                       => "tasks.create"
    }

    def addSurroundingQuotes(input: String): String = s"\"$input\""
    val projectNameOverrideString       = s"[${projectNameOverride.map(addSurroundingQuotes).getOrElse("")}]"
    val configurationNameOverrideString = s"[${configurationNameOverride.map(addSurroundingQuotes).getOrElse("")}]"

    Source
      .fromResource("io/joern/x2cpg/utils/dependency/dependency-fetcher-init.gradle")
      .getLines()
      .mkString(System.lineSeparator())
      .replaceAll("__projectNameOverrides__", projectNameOverrideString)
      .replaceAll("__configurationNameOverrides__", configurationNameOverrideString)
      .replaceAll("__taskNameString__", addSurroundingQuotes(taskName))
      .replaceAll("__destinationDirString__", addSurroundingQuotes(destinationDir))
      .replaceAll("__defaultProjectNameString__", addSurroundingQuotes(defaultGradleAppName))
      .replaceAll("tasks.register", taskCreationFunction)
  }

  private def getGradleVersionMajorMinor(connection: ProjectConnection): GradleVersion = {
    val buildEnv      = connection.getModel[BuildEnvironment](classOf[BuildEnvironment])
    val gradleVersion = buildEnv.getGradle.getGradleVersion

    def isValidPart(part: String) = part.forall(Character.isDigit)
    val parts                     = gradleVersion.split('.')
    if (parts.length == 1 && isValidPart(parts(0))) {
      GradleVersion(parts(0).toInt, 0)
    } else if (parts.length >= 2 && isValidPart(parts(0)) && isValidPart(parts(1))) {
      GradleVersion(parts(0).toInt, parts(1).toInt)
    } else {
      GradleVersion(-1, -1)
    }
  }

  private def makeInitScript(
    destinationDir: Path,
    gradleVersion: GradleVersion,
    projectNameOverride: Option[String],
    configurationNameOverride: Option[String]
  ): GradleDepsInitScript = {
    val taskName = taskNamePrefix + "_" + (Random.alphanumeric take 8).toList.mkString
    val content = getInitScriptContent(
      taskName,
      destinationDir.toString,
      gradleVersion,
      projectNameOverride,
      configurationNameOverride
    )
    GradleDepsInitScript(content, taskName, destinationDir)
  }

  private[dependency] def makeConnection(projectDir: JFile): ProjectConnection = {
    GradleConnector.newConnector().forProjectDirectory(projectDir).connect()
  }

  private def dependencyMapFromOutputDir(outputDir: Path): Map[String, List[String]] = {
    val dependencyMap = mutable.Map.empty[String, List[String]]

    Try(Files.walk(outputDir)) match {
      case Success(fileStreamResource) =>
        Using.resource(fileStreamResource) { fileStream =>
          fileStream
            .collect(Collectors.toList)
            .asScala
            .groupBy(path => path.getParent.toString)
            .foreach { case (parentStr, paths) =>
              paths.collect {
                case path if !Files.isDirectory(path) =>
                  dependencyMap.put(
                    parentStr,
                    path.toAbsolutePath.toString :: dependencyMap.getOrElseUpdate(parentStr, Nil)
                  )
              }
            }
        }
      case Failure(e: Throwable) =>
        logger.warn(s"Encountered exception while walking dependency fetcher output: ${e.getMessage}")
    }

    dependencyMap.toMap
  }

  private def runGradleTask(
    connection: ProjectConnection,
    taskName: String,
    destinationDir: Path,
    initScriptPath: String
  ): Map[String, List[String]] = {
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
          val result          = dependencyMapFromOutputDir(destinationDir)
          val dependencyCount = result.values.flatten.size
          logger.info(s"Task $taskName resolved `$dependencyCount` dependency files.")
          result
        case Failure(ex) =>
          logger.warn(s"Caught exception while executing Gradle task: ${ex.getMessage}")
          val androidSdkError = "Define a valid SDK location with an ANDROID_HOME environment variable"
          if (stderrStream.toString.contains(androidSdkError)) {
            logger.warn(
              "A missing Android SDK configuration caused gradle dependency fetching failures. Please define a valid SDK location with an ANDROID_HOME environment variable or by setting the sdk.dir path in your project's local properties file"
            )
          }
          if (stderrStream.toString.contains("Could not compile initialization script")) {

            val scriptContents = new String(Files.readAllBytes(Paths.get(initScriptPath)), Charset.defaultCharset())
            logger.debug(
              s"########## INITIALIZATION_SCRIPT ##########\n$scriptContents\n###########################################"
            )
          }
          logger.debug(s"Gradle task execution stdout: \n$stdoutStream")
          logger.debug(s"Gradle task execution stderr: \n$stderrStream")
          Map.empty
      }
    }
  }

  private def extractClassesJarFromAar(aar: Path): Option[Path] = {
    val newPath           = aar.toString.replaceFirst(aarFileExtension + "$", "jar")
    val aarUnzipDirSuffix = ".unzipped"
    val outDir            = Paths.get(aar.toString + aarUnzipDirSuffix)
    aar.unzipTo(outDir, _.getName == jarInsideAarFileName)
    val outFile = Paths.get(newPath)
    val classesJarEntries =
      Files
        .walk(outDir)
        .iterator()
        .asScala
        .filterNot(_ == outDir)
        .filter(_.fileName == jarInsideAarFileName)
        .toList
    if (classesJarEntries.size != 1) {
      logger.warn(s"Found aar file without `classes.jar` inside at path ${aar}")
      FileUtil.delete(outDir)
      None
    } else {
      val classesJar = classesJarEntries.head
      logger.trace(s"Copying `classes.jar` for aar at `${aar.toString}` into `$newPath`")
      classesJar.copyTo(outFile)
      FileUtil.delete(outDir)
      FileUtil.delete(aar)
      Some(outFile)
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
    Try(Files.createTempDirectory(tempDirPrefix)) match {
      case Success(destinationDir) =>
        FileUtil.deleteOnExit(destinationDir)
        Try(Files.createTempFile(initScriptPrefix, "")) match {
          case Success(initScriptFile) =>
            FileUtil.deleteOnExit(initScriptFile)
            Try(makeConnection(projectDir.toFile)) match {
              case Success(connection) =>
                Using.resource(connection) { c =>
                  val gradleVersion = getGradleVersionMajorMinor(connection)
                  val initScript =
                    makeInitScript(destinationDir, gradleVersion, projectNameOverride, configurationNameOverride)
                  Files.writeString(initScriptFile, initScript.contents)
                  runGradleTask(c, initScript.taskName, initScript.destinationDir, initScriptFile.toString).map {
                    case (projectName, dependencies) =>
                      projectName -> dependencies.map { dependency =>
                        if (!dependency.endsWith(aarFileExtension))
                          dependency
                        else
                          extractClassesJarFromAar(Paths.get(dependency)) match {
                            case Some(path) => path.toString
                            case None       => dependency
                          }
                      }
                  }
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
  }
}
