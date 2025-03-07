package io.joern.x2cpg.utils.dependency

import io.joern.x2cpg.utils.FileUtil
import io.joern.x2cpg.utils.FileUtil.*

import java.nio.charset.Charset
import org.gradle.tooling.{GradleConnector, ProjectConnection}
import org.gradle.tooling.model.{GradleProject, ProjectIdentifier, Task}
import org.gradle.tooling.model.build.BuildEnvironment
import org.slf4j.LoggerFactory

import java.io.{ByteArrayOutputStream, IOException, File as JFile}
import java.nio.file.{Files, Path, Paths}
import java.util.stream.Collectors
import scala.collection.mutable
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
    val projectNameOverrideString                   = projectNameOverride.map(addSurroundingQuotes).getOrElse("")
    val configurationNameOverrideString             = configurationNameOverride.map(addSurroundingQuotes).getOrElse("")

    s"""
       |import java.nio.file.Path
       |import java.nio.file.Files
       |import java.nio.file.StandardCopyOption
       |
       |abstract class FetchDependencies extends DefaultTask {
       |
       |  FetchDependencies() {
       |    outputs.upToDateWhen { false }
       |  }
       |
       |  @Input
       |  abstract SetProperty<String> getConfigurationNameOverrides()
       |
       |  @Input
       |  abstract SetProperty<String> getProjectNameOverrides()
       |
       |  @OutputDirectory
       |  abstract Property<String> getDestinationDirString()
       |
       |  /**
       |   * If one project in the build has a different project in the build as a dependency,
       |   * then fetching jars for all dependencies will include the packaged jar for that
       |   * subproject. Since the source for that subproject is already used for dependency
       |   * information, those dependency jars should be excluded. This is done by traversing
       |   * the dependency graph and only adding dependencies with names/descriptors not starting
       |   * with `project` or `root project` to the list of dependencies to fetch.
       |   *
       |   * This is a modified version of an example from the documentation without the
       |   * rootVariant parameter as this is not supported in Gradle 7.
       |   * See https://docs.gradle.org/current/userguide/dependency_graph_resolution.html
       |   */
       |  HashSet<String> getExternalDependencyNames(
       |    ResolvedComponentResult rootComponent,
       |    String configurationName
       |  ) {
       |    Set<String> artifactsToFetch = new HashSet<>()
       |    Set<ResolvedVariantResult> seen = new HashSet<>()
       |    def maybeRootVariant =
       |      rootComponent.getVariants().stream()
       |        .filter(variant -> variant.getDisplayName() == configurationName)
       |        .findFirst()
       |
       |    if (maybeRootVariant.isPresent()) {
       |      def rootVariant = maybeRootVariant.get()
       |      seen.add(rootVariant)
       |
       |      def stack = new ArrayDeque<Tuple2<ResolvedVariantResult, ResolvedComponentResult>>()
       |      stack.addFirst(new Tuple2(rootVariant, rootComponent))
       |
       |      while (!stack.isEmpty()) {
       |        def entry = stack.removeFirst()
       |        def variant = entry.v1
       |        def component = entry.v2
       |        def variantId = variant.owner.displayName
       |        if (!(variantId.startsWith("project") || variantId.startsWith("root project"))) {
       |          artifactsToFetch.add(variantId)
       |        }
       |
       |        // Traverse this variant's dependencies
       |        for (dependency in component.getDependenciesForVariant(variant)) {
       |          if (dependency instanceof UnresolvedDependencyResult) {
       |            System.err.println("Unresolved dependency $$dependency")
       |            continue
       |          }
       |          if ((!dependency instanceof ResolvedDependencyResult)) {
       |            System.err.println("Unknown dependency type: $$dependency")
       |            continue
       |          }
       |
       |          def resolved = dependency as ResolvedDependencyResult
       |          if (!dependency.constraint) {
       |            def toVariant = resolved.resolvedVariant
       |
       |            if (seen.add(toVariant)) {
       |              stack.addFirst(new Tuple2(toVariant, resolved.selected))
       |            }
       |          }
       |        }
       |      }
       |    }
       |
       |    return artifactsToFetch
       |  }
       |
       |
       |
       |  String getProjectFullName(Project project) {
       |    def projectString = project.toString()
       |    if (projectString.startsWith("root project")) {
       |      return ""
       |    } else {
       |      return projectString.substring("project ':".length(), projectString.length() - 1)
       |    }
       |  }
       |
       |  /**
       |   * Checking if a configuration is valid:
       |   *  - If configuration overrides are set, then the configuration name must appear in the overrides list
       |   *  - If no overrides are set, the configuration name must start with release or runtime
       |   */
       |  List<Configuration> getValidConfigurations(Project project) {
       |    def configurationOverrides = configurationNameOverrides.get()
       |    def validConfigurations = []
       |
       |    for (configuration in project.configurations) {
       |      if (configuration.canBeResolved) {
       |        def configurationName = configuration.name
       |        if (configurationOverrides.isEmpty()) {
       |          if (configurationName.startsWith("release") || configurationName.startsWith("runtime")) {
       |            validConfigurations << configuration
       |          }
       |        } else if (configurationOverrides.contains(configurationName)) {
       |          validConfigurations << configuration
       |        }
       |      }
       |    }
       |
       |    return validConfigurations
       |  }
       |
       |  void linkOrCopyFileToDestination(File artifactFile, Path destinationDir) {
       |    try {
       |      Files.createSymbolicLink(
       |        destinationDir.resolve(artifactFile.name),
       |        Path.of(artifactFile.getPath())
       |      )
       |    } catch (Exception e) {
       |      Files.copy(
       |        Path.of(artifactFile.getPath()),
       |        destinationDir.resolve(artifactFile.name),
       |        StandardCopyOption.REPLACE_EXISTING
       |      )
       |    }
       |  }
       |
       |  // See https://docs.gradle.org/current/userguide/artifact_views.html
       |  // See https://docs.gradle.org/current/userguide/artifact_resolution.html#artifact-resolution for more information
       |  void fetchArtifactsForConfiguration(Configuration configuration, Path destinationDir) {
       |    for (resolvedArtifacts in configuration.incoming.getResolutionResult()) {
       |      def artifactsToFetch = getExternalDependencyNames(
       |        resolvedArtifacts.rootComponent.get(),
       |        configuration.name
       |      )
       |
       |      def filteredArtifacts = configuration.incoming.artifactView {
       |        componentFilter {
       |          artifactsToFetch.contains(it.toString())
       |        }
       |      }
       |
       |      for (artifactFile in filteredArtifacts.files) {
       |        linkOrCopyFileToDestination(artifactFile, destinationDir)
       |      }
       |    }
       |  }
       |
       |  boolean shouldFetchDependencies(String projectFullName) {
       |    return projectNameOverrides.get().isEmpty() ||
       |      projectFullName.isEmpty() ||
       |      projectNameOverrides.get().contains(projectFullName)
       |  }
       |
       |  @TaskAction
       |  void fetch() {
       |    // TODO Need to update direct project access for gradle 9:
       |    //      See https://docs.gradle.org/8.12.1/userguide/upgrading_version_7.html#task_project
       |    def project_ = project
       |    def projectFullName = getProjectFullName(project_)
       |
       |    if (shouldFetchDependencies(projectFullName)) {
       |      def validConfigurations = getValidConfigurations(project_)
       |
       |      if (!validConfigurations.isEmpty()) {
       |        def destinationDir =
       |          projectFullName.isEmpty() ? Path.of(destinationDirString.get()) : Path.of(destinationDirString.get(), projectFullName.replace(':', '/'))
       |        Files.createDirectories(destinationDir)
       |
       |        validConfigurations.forEach { fetchArtifactsForConfiguration(it, destinationDir) }
       |      }
       |    }
       |  }
       |}
       |
       |allprojects { project ->
       |  def taskName = "$taskName"
       |  def compileDepsCopyTaskName = taskName + "_compileDeps"
       |  def androidDepsCopyTaskName = taskName + "_androidDeps"
       |  def destinationDir = "$destinationDir"
       |  def defaultProjectName = "$defaultGradleAppName"
       |  // If these overrides are non-empty, only fetch dependencies for the given names
       |  Set<String> projectNameOverrides_ = [$projectNameOverrideString]
       |  Set<String> configurationNameOverrides_ = [$configurationNameOverrideString]
       |
       |  def hasAndroidProperty = false
       |  for (property in project.properties.keySet()) {
       |    if (property.startsWith("android") || property.startsWith(".android")) {
       |      hasAndroidProperty = true
       |      break
       |    }
       |  }
       |
       |  if (hasAndroidProperty) {
       |    $taskCreationFunction(androidDepsCopyTaskName, Copy) {
       |      def paths = project.configurations.find { it.name.equals("androidApis") }
       |      if (paths == null) paths = []
       |      duplicatesStrategy = 'include'
       |      into destinationDir
       |      from paths
       |    }
       |  }
       |
       |  $taskCreationFunction(compileDepsCopyTaskName, FetchDependencies) {
       |    projectNameOverrides = projectNameOverrides_
       |    configurationNameOverrides = configurationNameOverrides_
       |    destinationDirString = destinationDir
       |  }
       |
       |  if (project == project.rootProject) {
       |    def defaultSubproject = project.subprojects.stream()
       |      .filter { it.name == defaultProjectName }
       |      .findFirst()
       |
       |    if (projectNameOverrides_.isEmpty() && defaultSubproject.isPresent()) {
       |      def subproj = defaultSubproject.get()
       |      println("Only fetching dependecies for default subproject $$defaultProjectName since it is present with no overrides")
       |      $taskCreationFunction(taskName) {
       |          dependsOn subproj.getTasksByName(androidDepsCopyTaskName, false)
       |          dependsOn subproj.getTasksByName(compileDepsCopyTaskName, false)
       |      }
       |    } else {
       |      $taskCreationFunction(taskName) {
       |          dependsOn project.getTasksByName(androidDepsCopyTaskName, true)
       |          dependsOn project.getTasksByName(compileDepsCopyTaskName, true)
       |      }
       |    }
       |  }
       |}
       |
       |""".stripMargin
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
        .filter(_.getFileName.toString == jarInsideAarFileName)
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
