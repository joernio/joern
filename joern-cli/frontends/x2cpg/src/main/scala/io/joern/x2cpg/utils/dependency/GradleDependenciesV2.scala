package io.joern.x2cpg.utils.dependency

import io.shiftleft.semanticcpg.utils.FileUtil
import io.shiftleft.semanticcpg.utils.FileUtil.*
import org.apache.commons.text.StringEscapeUtils
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
  // TODO: Several private members below are duplicated verbatim from `GradleDependencies` (V1):
  // `getGradleHome`, `getGradleVersionMajorMinor`, `makeConnection`, the three prefix constants
  // (`initScriptPrefix`, `taskNamePrefix`, `tempDirPrefix`), the random task-name generator,
  // `runGradleTask`, and the replace-pipeline shape inside `getInitScriptContent`. The duplication
  // is intentional during the V2 rollout so V1 stays untouched; once V1 is deleted we should
  // consolidate these into a shared `GradleToolingApi` helper.
  private val initScriptPrefix                 = "x2cpg.init.gradle"
  private val taskNamePrefix                   = "x2cpgCopyDeps"
  private[dependency] val tempDirPrefix        = "x2cpgDependencies"
  private[dependency] val dependencyInfoSubdir = "dependencyInfo"

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

    // Emits a Groovy double-quoted string literal. Groovy accepts the same escape vocabulary
    // as Java, so `escapeJava` is the right tool — without it, backslashes in Windows paths
    // (e.g. `C:\Users\RUNNER~1\...`) would blow up Groovy's escape-sequence parser.
    def addSurroundingQuotes(input: String): String = s"\"${StringEscapeUtils.escapeJava(input)}\""
    val configurationNameOverrideString = s"[${configurationNameOverride.map(addSurroundingQuotes).getOrElse("")}]"

    // `replace` (CharSequence overload) is a literal substitution. `replaceAll` interprets the
    // replacement as a regex replacement string, which would consume backslashes in Windows
    // paths (e.g. `C:\Users\...` → `C:Users...`) and corrupt the generated init script.
    Source
      .fromResource("io/joern/x2cpg/utils/dependency/dependency-fetcher-init-v2.gradle")
      .getLines()
      .mkString(System.lineSeparator())
      .replace("__configurationNameOverrides__", configurationNameOverrideString)
      .replace("__taskNameString__", addSurroundingQuotes(taskName))
      .replace("__destinationDirString__", addSurroundingQuotes(destinationDir))
      .replace("__androidVariant__", addSurroundingQuotes(androidVariant))
      .replace("tasks.register", taskCreationFunction)
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

  private[dependency] def listDependencyInfoFiles(dependencyInfoDir: Path): Array[Path] = {
    if (!Files.isDirectory(dependencyInfoDir)) Array.empty
    else {
      Using.resource(Files.list(dependencyInfoDir)) { stream =>
        stream
          .iterator()
          .asScala
          .filter(path => !Files.isDirectory(path) && path.toString.endsWith(".json"))
          .toArray
      }
    }
  }

  // Acquire the two resources `get` needs — the init-script temp file and the Gradle connection.
  // `Try`'s for-comprehension short-circuits on the first Failure, so the connection is only
  // opened when the temp file was created successfully. Failures are logged once here; the caller
  // just checks Success/Failure.
  private def acquireInitTmpFileAndConn(projectDir: Path): Try[(Path, ProjectConnection)] = {
    def attempt[T](stage: String)(thunk: => T): Try[T] =
      Try(thunk).recoverWith { case ex =>
        logger.warn(s"$stage failed: ${ex.getMessage}")
        logger.debug("Full exception: ", ex)
        Failure(ex)
      }

    for {
      initScriptFile <- attempt("Creating Gradle init script temp file") {
        val tempFile = Files.createTempFile(initScriptPrefix, "")
        FileUtil.deleteOnExit(tempFile)
        tempFile
      }
      connection <- attempt("Establishing Gradle connection")(makeConnection(projectDir.toFile))
    } yield (initScriptFile, connection)
  }

  // fetch the gradle project information by invoking a newly-defined gradle task that writes one
  // depInfo JSON per project under `<destinationDir>/dependencyInfo/`. The caller owns
  // `destinationDir` (and its `deleteOnExit` registration) so it can be shared across multiple
  // peer-build invocations; the same directory doubles as the cache root used for lazy
  // .aar → classes.jar extraction at consumption time.
  private[dependency] def get(
    projectDir: Path,
    destinationDir: Path,
    configurationNameOverride: Option[String],
    androidVariantOverride: Option[String] = None
  ): Boolean = {
    logger.info(s"Fetching Gradle project information at path `$projectDir`.")
    acquireInitTmpFileAndConn(projectDir) match {
      case Failure(_) => false
      case Success((initScriptFile, connection)) =>
        Using.resource(connection) { conn =>
          val gradleVersion  = getGradleVersionMajorMinor(conn)
          val androidVariant = androidVariantOverride.getOrElse(DefaultAndroidVariant)
          val initScript =
            makeInitScript(destinationDir, gradleVersion, configurationNameOverride, androidVariant)
          Files.writeString(initScriptFile, initScript.contents)
          val ranOk = runGradleTask(conn, initScript.taskName, initScriptFile.toString)
          if (!ranOk) {
            logger.info(
              s"Gradle dependency fetch did not complete cleanly for $projectDir; any JSONs written so far still contribute."
            )
          }
          ranOk
        }
    }
  }
}
