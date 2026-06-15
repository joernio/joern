package io.joern.x2cpg.utils.dependency

import io.shiftleft.semanticcpg.utils.FileUtil
import io.shiftleft.semanticcpg.utils.FileUtil.*
import org.gradle.tooling.GradleConnector
import org.gradle.tooling.ProjectConnection
import org.gradle.tooling.model.build.BuildEnvironment
import org.slf4j.LoggerFactory

import java.io.ByteArrayOutputStream
import java.io.File as JFile
import java.nio.charset.Charset
import java.nio.file.{Files, Path, Paths}
import scala.io.Source
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Random, Success, Try, Using}

object GradleDependenciesV2 {
  private val initScriptPrefix     = "x2cpg.init.gradle"
  private val taskNamePrefix       = "x2cpgCopyDeps"
  private val tempDirPrefix        = "x2cpgDependencies"
  private val dependencyInfoSubdir = "dependencyInfo"

  private val logger = LoggerFactory.getLogger(getClass)

  private def getGradleHome: Option[JFile] = {
    sys.env
      .get("GRADLE_HOME")
      .filterNot(_.isBlank)
      .map(new JFile(_))
      .filter(gradleHome => gradleHome.exists && gradleHome.isDirectory)
  }

  private val DefaultAndroidVariant = "release"

  // works with Gradle 5.1+ because the script makes use of `task.register`:
  //   https://docs.gradle.org/current/userguide/task_configuration_avoidance.html
  private def getInitScriptContent(
    taskName: String,
    destinationDir: String,
    gradleVersion: GradleVersion,
    configurationNameOverride: Option[String],
    androidVariant: String
  ): String = {
    val taskCreationFunction = gradleVersion match {
      case GradleVersion(major, minor) if major >= 5 && minor >= 1 => "tasks.register"
      case _                                                       => "tasks.create"
    }

    def addSurroundingQuotes(input: String): String = s"\"$input\""
    val configurationNameOverrideString = s"[${configurationNameOverride.map(addSurroundingQuotes).getOrElse("")}]"

    Source
      .fromResource("io/joern/x2cpg/utils/dependency/dependency-fetcher-init-v2.gradle")
      .getLines()
      .mkString(System.lineSeparator())
      .replaceAll("__configurationNameOverrides__", configurationNameOverrideString)
      .replaceAll("__taskNameString__", addSurroundingQuotes(taskName))
      .replaceAll("__destinationDirString__", addSurroundingQuotes(destinationDir))
      .replaceAll("__androidVariant__", addSurroundingQuotes(androidVariant))
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
    configurationNameOverride: Option[String],
    androidVariant: String
  ): GradleDepsInitScript = {
    val taskName = taskNamePrefix + "_" + (Random.alphanumeric take 8).toList.mkString
    val content =
      getInitScriptContent(taskName, destinationDir.toString, gradleVersion, configurationNameOverride, androidVariant)
    GradleDepsInitScript(content, taskName)
  }

  private[dependency] def makeConnection(projectDir: JFile): ProjectConnection = {
    val connector = GradleConnector.newConnector().forProjectDirectory(projectDir)
    getGradleHome.foreach { gradleHome =>
      logger.info(s"Using gradle distribution from GRADLE_HOME at ${gradleHome.getAbsolutePath}")
      connector.useInstallation(gradleHome)
    }
    connector.connect()
  }

  private def runGradleTask(connection: ProjectConnection, taskName: String, initScriptPath: String): Boolean = {
    Using.resources(new ByteArrayOutputStream, new ByteArrayOutputStream) { case (stdoutStream, stderrStream) =>
      Option(System.getenv("ANDROID_HOME")) match {
        case Some(androidHome) => logger.debug(s"Found ANDROID_HOME set to $androidHome")
        case None              => logger.debug("ANDROID_HOME not set")
      }
      logger.debug(s"Executing gradle task '$taskName'...")
      Try(
        connection
          .newBuild()
          .forTasks(taskName)
          .withArguments("--init-script", initScriptPath)
          .setStandardOutput(stdoutStream)
          .setStandardError(stderrStream)
          .setEnvironmentVariables(
            // pass through environment variables or otherwise no valid SDK
            // location from the ANDROID_HOME environment variable may be found
            System.getenv()
          )
          .run()
      ) match {
        case Success(_) =>
          logger.debug(s"Gradle task execution stdout: \n$stdoutStream")
          logger.debug(s"Gradle task execution stderr: \n$stderrStream")
          true
        case Failure(ex) =>
          logger.warn(s"Caught exception while executing Gradle task: ${ex.getMessage}", ex)
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
          false
      }
    }
  }

  private def listDependencyInfoFiles(dependencyInfoDir: Path): Array[Path] = {
    if (!Files.isDirectory(dependencyInfoDir)) return Array.empty
    Using.resource(Files.list(dependencyInfoDir)) { stream =>
      stream
        .iterator()
        .asScala
        .filter(p => !Files.isDirectory(p) && p.toString.endsWith(".json"))
        .toArray
    }
  }

  private def emptyGraph(cacheDir: Path): DependencyGraph =
    DependencyGraph(Map.empty, Map.empty, Map.empty, cacheDir)

  // fetch the gradle project information first, then invoke a newly-defined gradle task that
  // writes one depInfo JSON per project under `<destinationDir>/dependencyInfo/`. The JSON dir
  // is then loaded into a DependencyGraph; it also doubles as the cache directory used for lazy
  // .aar → classes.jar extraction at consumption time.
  private[dependency] def get(
    projectDir: Path,
    configurationNameOverride: Option[String],
    androidVariantOverride: Option[String] = None
  ): DependencyGraph = {
    logger.info(s"Fetching Gradle project information at path `$projectDir`.")
    Try(Files.createTempDirectory(tempDirPrefix)) match {
      case Failure(ex) =>
        logger.warn(s"Could not create temporary directory for saving dependency files: ${ex.getMessage}")
        logger.debug("Full exception: ", ex)
        // No persistent location to lazy-extract aars into either, so fall back to the system tmp dir.
        emptyGraph(Paths.get(System.getProperty("java.io.tmpdir")))

      case Success(destinationDir) =>
        FileUtil.deleteOnExit(destinationDir)
        val dependencyInfoDir = destinationDir.resolve(dependencyInfoSubdir)

        Try(Files.createTempFile(initScriptPrefix, "")) match {
          case Failure(ex) =>
            logger.warn(s"Could not create temporary file for Gradle init script: ${ex.getMessage}")
            logger.debug(s"Full exception: ", ex)
            emptyGraph(dependencyInfoDir)

          case Success(initScriptFile) =>
            FileUtil.deleteOnExit(initScriptFile)
            Try(makeConnection(projectDir.toFile)) match {
              case Failure(ex) =>
                logger.warn(s"Caught exception while trying to establish a Gradle connection: ${ex.getMessage}")
                logger.debug(s"Full exception: ", ex)
                emptyGraph(dependencyInfoDir)

              case Success(connection) =>
                Using.resource(connection) { c =>
                  val gradleVersion  = getGradleVersionMajorMinor(connection)
                  val androidVariant = androidVariantOverride.getOrElse(DefaultAndroidVariant)
                  val initScript =
                    makeInitScript(destinationDir, gradleVersion, configurationNameOverride, androidVariant)
                  Files.writeString(initScriptFile, initScript.contents)
                  val ranOk = runGradleTask(c, initScript.taskName, initScriptFile.toString)
                  if (!ranOk) {
                    logger.info(
                      s"Gradle dependency fetch did not complete cleanly for $projectDir; returning partial graph."
                    )
                  }
                  val jsonPaths = listDependencyInfoFiles(dependencyInfoDir)
                  logger.info(s"Loaded ${jsonPaths.length} project dependency info files from $dependencyInfoDir.")
                  DependencyGraph.fromJson(dependencyInfoDir, jsonPaths)
                }
            }
        }
    }
  }
}
