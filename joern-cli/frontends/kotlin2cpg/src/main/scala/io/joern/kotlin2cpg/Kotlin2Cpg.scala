package io.joern.kotlin2cpg

import io.joern.javasrc2cpg.JavaSrc2Cpg
import io.joern.kotlin2cpg.compiler.CompilerAPI
import io.joern.kotlin2cpg.compiler.ErrorLoggingMessageCollector
import io.joern.kotlin2cpg.files.SourceFilesPicker
import io.joern.kotlin2cpg.interop.JavaSrcInterop
import io.joern.kotlin2cpg.jar4import.UsesService
import io.joern.kotlin2cpg.passes.*
import io.joern.kotlin2cpg.types.{ContentSourcesPicker, TypeInfoProvider}
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.X2CpgFrontend
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.passes.frontend.MetaDataPass
import io.joern.x2cpg.passes.frontend.TypeNodePass
import io.joern.x2cpg.utils.dependency.{DependencyResolver, DependencyResolverParams, GradleConfigKeys, GradleDependencies}
import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.joern.x2cpg.SourceFiles.filterFile
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.semanticcpg.utils.FileUtil
import io.shiftleft.utils.IOUtils
import org.jetbrains.kotlin.cli.common.CLIConfigurationKeys
import org.jetbrains.kotlin.cli.common.config.KotlinSourceRoot
import org.jetbrains.kotlin.cli.jvm.compiler.{CoreEnvironmentUtilsKt, KotlinCoreEnvironment, KotlinToJVMBytecodeCompiler}
import org.jetbrains.kotlin.com.intellij.openapi.util.Disposer
import org.jetbrains.kotlin.config.{CommonConfigurationKeys, CompilerConfiguration}
import org.jetbrains.kotlin.psi.KtFile
import org.jetbrains.kotlin.resolve.BindingContext
import org.slf4j.LoggerFactory

import java.nio.file.Files
import java.nio.file.Paths
import java.util.regex.Pattern
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.jdk.CollectionConverters.EnumerationHasAsScala
import scala.jdk.StreamConverters.StreamHasToScala
import scala.util.{Failure, Try, Using}
import scala.util.matching.Regex

object Kotlin2Cpg {

  private val logger                        = LoggerFactory.getLogger(getClass)
  private val JarExtension: String          = ".jar"
  private val ImportPattern: Regex          = ".*import([^;]*).*".r
  val DefaultIgnoredFilesRegex: List[Regex] = JavaSrc2Cpg.DefaultIgnoredFilesRegex

  private val defaultKotlinStdlibContentRootJarPaths = Seq(
    DefaultContentRootJarPath("jars/kotlin-stdlib-1.9.0.jar", isResource = true),
    DefaultContentRootJarPath("jars/kotlin-stdlib-common-1.9.0.jar", isResource = true),
    DefaultContentRootJarPath("jars/kotlin-stdlib-jdk8-1.9.0.jar", isResource = true)
  )

  def postProcessingPass(cpg: Cpg): Unit = {
    new KotlinTypeRecoveryPassGenerator(cpg).generate().foreach(_.createAndApply())
    new KotlinTypeHintCallLinker(cpg).createAndApply()
  }

}

case class KtFileWithMeta(f: KtFile, relativizedPath: String, filename: String)
case class FileContentAtPath(content: String, relativizedPath: String, filename: String)

class Kotlin2Cpg extends X2CpgFrontend with UsesService {
  override type ConfigType = Config
  override val defaultConfig: Config = Frontend.defaultConfig.copy()

  import Kotlin2Cpg.*

  private def checkSourceDir(sourceDir: String): Unit = {
    if (!Files.exists(Paths.get(sourceDir))) {
      println(s"The specified input path `$sourceDir` is not a file that exists. Exiting.")
      System.exit(1)
    }
    if (!Files.isDirectory(Paths.get(sourceDir))) {
      println(s"The specified input path `$sourceDir` is not a directory. Exiting.")
      System.exit(1)
    }
  }

  private def logMaxHeapSize(): Unit = {
    val maxHeapSize          = Runtime.getRuntime.maxMemory()
    val formattedMaxHeapSize = String.format("%,.2f", maxHeapSize / (1024 * 1024 * 1024).toDouble)
    logger.info(s"Max heap size currently set to `${formattedMaxHeapSize}GB`.")
  }

  private def gatherFilesWithKtExtension(sourceDir: String, config: Config): List[String] = {
    val filesWithKtExtension = SourceFiles.determine(
      sourceDir,
      Set(".kt"),
      ignoredFilesRegex = Option(config.ignoredFilesRegex),
      ignoredFilesPath = Option(config.ignoredFiles),
      ignoredDefaultRegex = Option(config.defaultIgnoredFilesRegex)
    )
    if (filesWithKtExtension.isEmpty) {
      println(s"The provided input directory does not contain files ending in '.kt' `$sourceDir`. Exiting.")
      System.exit(1)
    }
    filesWithKtExtension
  }

  private def gatherFilesWithJavaExtension(sourceDir: String, config: Config): List[String] = {
    val filesWithJavaExtension = SourceFiles.determine(
      sourceDir,
      Set(".java"),
      ignoredFilesRegex = Option(config.ignoredFilesRegex),
      ignoredFilesPath = Option(config.ignoredFiles),
      ignoredDefaultRegex = Option(config.defaultIgnoredFilesRegex)
    )
    if (filesWithJavaExtension.nonEmpty) {
      logger.info(s"Found ${filesWithJavaExtension.size} files with the `.java` extension.")
    }
    filesWithJavaExtension
  }

  private def gatherMavenCoordinates(sourceDir: String, config: Config): Seq[String] = {
    if (config.generateNodesForDependencies) {
      logger.info(s"Fetching maven coordinates.")
      fetchMavenCoordinates(sourceDir, config)
    } else Seq()
  }

  private def gatherJarsOfDependencies(
    sourceDir: String,
    config: Config,
    filesWithJavaExtension: List[String]
  ): Seq[DefaultContentRootJarPath] = {
    val jar4ImportServiceOpt = config.jar4importServiceUrl.flatMap(reachableServiceMaybe)
    val dependencies =
      if (jar4ImportServiceOpt.isDefined) {
        val filesWithKtExtension = gatherFilesWithKtExtension(sourceDir, config)
        val importNames          = importNamesForFilesAtPaths(filesWithKtExtension ++ filesWithJavaExtension)
        logger.trace(s"Found imports: `$importNames`")
        dependenciesFromService(jar4ImportServiceOpt.get, importNames)
      } else if (config.downloadDependencies) {
        //downloadDependencies(sourceDir, config)
        Seq()
      } else {
        logger.info(s"Not downloading any dependencies.")
        Seq()
      }

    dependencies.map { path =>
      DefaultContentRootJarPath(path, isResource = false)
    }
  }

  private def gatherJarsFromStdLib(config: Config): Seq[DefaultContentRootJarPath] = {
    if (config.withStdlibJarsInClassPath) {
      defaultKotlinStdlibContentRootJarPaths
    } else {
      Seq()
    }
  }

  private def gatherJarsFromConfigClassPath(config: Config): Seq[DefaultContentRootJarPath] = {
    val jarsAtConfigClassPath = findJarsIn(config.classpath)
    if (config.classpath.nonEmpty) {
      if (jarsAtConfigClassPath.nonEmpty) {
        logger.info(s"Found ${jarsAtConfigClassPath.size} jars in the specified classpath.")
      } else {
        logger.warn("No jars found in the specified classpath.")
      }
    }
    jarsAtConfigClassPath.map { path => DefaultContentRootJarPath(path, isResource = false) }
  }

  private def gatherDirsForSourcesToCompile(sourceDir: String): Seq[String] = {
    val dirsForSourcesToCompile = ContentSourcesPicker.dirsForRoot(sourceDir)
    if (dirsForSourcesToCompile.isEmpty) {
      logger.warn("The list of directories to analyze is empty.")
    }
    dirsForSourcesToCompile
  }

  private def gatherSourceFiles(
    sourceDir: String,
    config: Config,
    environment: KotlinCoreEnvironment
  ): Iterable[KtFileWithMeta] = {
    val sourceEntries = entriesForSources(environment.getSourceFiles.asScala, sourceDir)
    val sourceFiles = sourceEntries.filter(entry =>
      SourceFiles.filterFile(
        entry.filename,
        config.inputPath,
        ignoredFilesRegex = Option(config.ignoredFilesRegex),
        ignoredFilesPath = Option(config.ignoredFiles),
        ignoredDefaultRegex = Option(config.defaultIgnoredFilesRegex)
      )
    )
    sourceFiles
  }

  private def runJavaSrcInterop(
    cpg: Cpg,
    config: Config,
    filesWithJavaExtension: List[String],
    kotlinAstCreatorTypes: Set[String]
  ): Set[String] = {
    if (config.includeJavaSourceFiles && filesWithJavaExtension.nonEmpty) {
      val javaAstCreator = JavaSrcInterop.astCreationPass(config.inputPath, filesWithJavaExtension, cpg)
      javaAstCreator.createAndApply()
      val javaAstCreatorTypes = javaAstCreator.usedTypes()

      javaAstCreator.sourceParser.cleanupDelombokOutput()
      javaAstCreator.clearJavaParserCaches()

      javaAstCreatorTypes
    } else {
      Set.empty
    }
  }

  def createCpg(config: Config): Try[Cpg] = {
    withNewEmptyCpg(config.outputPath, config) { (cpg, config) =>
      val sourceDir = config.inputPath
      logger.info(s"Starting CPG generation for input directory `$sourceDir`.")

      checkSourceDir(sourceDir)
      logMaxHeapSize()

      logger.info("P0")
      val filesWithJavaExtension = gatherFilesWithJavaExtension(sourceDir, config)
      logger.info("P1")
      val mavenCoordinates = gatherMavenCoordinates(sourceDir, config)
      logger.info("P2")
      val stdlibJars = gatherJarsFromStdLib(config)
      logger.info("P3")
      val classPathJars = gatherJarsFromConfigClassPath(config)
      logger.info("P4")
      val dependencyJars = gatherJarsOfDependencies(sourceDir, config, filesWithJavaExtension)
      val (jarDeps, srcDeps) = downloadDependencies(sourceDir, config)
      logger.info("P5")
      val dirsForSourcesToCompile = gatherDirsForSourcesToCompile(sourceDir)
      logger.info("P6")
      val dependencyContentRoots = stdlibJars ++ classPathJars ++ dependencyJars
      logger.info("P7")

      logger.info("Pre env create")
      val environment = CompilerAPI.makeEnvironment(
        dirsForSourcesToCompile ++ srcDeps.flatMap(_._2),
        filesWithJavaExtension,
        dependencyContentRoots,
        new ErrorLoggingMessageCollector
      )
      logger.info("Post env create")

      // if (false) {
      //  val dirsForSourcesToCompile2 = ContentSourcesPicker.dirsForRoot2(sourceDir)
      //  val dirsForSourcesToCompile3 = ContentSourcesPicker.dirsForRoot3(sourceDir)

      //  val intersection = dirsForSourcesToCompile2.intersect(dirsForSourcesToCompile3)
      //  val missing = dirsForSourcesToCompile2.toSet.removedAll(intersection)

      //  logger.info(s"Missing from root2:")
      //  missing.foreach(logger.info)
      //
      //  val config2 = new CompilerConfiguration()
      //  val messageCollector = new ErrorLoggingMessageCollector
      //  config2.put(CommonConfigurationKeys.MESSAGE_COLLECTOR_KEY, messageCollector)
      //  dirsForSourcesToCompile2.foreach { p =>
      //    config2.add(CLIConfigurationKeys.CONTENT_ROOTS, new KotlinSourceRoot(p, true, null))
      //  }

      //  val sourceRoots2 = CoreEnvironmentUtilsKt.getSourceRootsCheckingForDuplicates(config2, messageCollector)
      //  val x2 = CoreEnvironmentUtilsKt.createSourceFilesFromSourceRoots(config2, environment.getProject, sourceRoots2, null)
      //
      //  val config3 = new CompilerConfiguration()
      //  config3.put(CommonConfigurationKeys.MESSAGE_COLLECTOR_KEY, messageCollector)
      //  dirsForSourcesToCompile3.foreach { p =>
      //    config3.add(CLIConfigurationKeys.CONTENT_ROOTS, new KotlinSourceRoot(p, true, null))
      //  }

      //  val sourceRoots3 = CoreEnvironmentUtilsKt.getSourceRootsCheckingForDuplicates(config3, messageCollector)
      //  val x3 = CoreEnvironmentUtilsKt.createSourceFilesFromSourceRoots(config3, environment.getProject, sourceRoots3, null)
      //
      //  val x2Set = x2.asScala.map(_.getVirtualFilePath).toSet
      //  val x3Set = x3.asScala.map(_.getVirtualFilePath).toSet
      //
      //  val diff = x2Set.diff(x3Set)
      //  diff.foreach(println)
      //
      //
      //  println("hier")
      // }

      val sourceFiles = gatherSourceFiles(sourceDir, config, environment)
      val configFiles = entriesForConfigFiles(SourceFilesPicker.configFiles(sourceDir), sourceDir)

      new MetaDataPass(cpg, Languages.KOTLIN, config.inputPath).createAndApply()

      val bindingContext =
        createBindingContext(environment)
          .recoverWith { exception =>
            if (dependencyContentRoots != stdlibJars) {
              logger.warn("Kotlin compiler analysis failed with exception", exception)
              logger.warn("Fallback: Reconfiguring kotlin compiler environment without dependencies")

              Using.resource(
                CompilerAPI.makeEnvironment(
                  dirsForSourcesToCompile,
                  filesWithJavaExtension,
                  stdlibJars,
                  new ErrorLoggingMessageCollector
                )
              ) { fallbackEnvironment =>
                createBindingContext(fallbackEnvironment)
              }(fallbackEnvironment => Disposer.dispose(fallbackEnvironment.getProjectEnvironment.getParentDisposable))

            } else {
              Failure(exception)
            }
          }
          .recover { exception =>
            logger.error("Kotlin compiler analysis failed with exception", exception)
            BindingContext.EMPTY
          }
          .get

      val astCreator =
        new AstCreationPass(sourceFiles, bindingContext, cpg, config.disableFileContent)(config.schemaValidation)
      astCreator.createAndApply()

      new SamTypeDeclPass(cpg, astCreator.samInfoEntries())(config.schemaValidation).createAndApply()

      Disposer.dispose(environment.getProjectEnvironment.getParentDisposable)

      val kotlinAstCreatorTypes = astCreator.usedTypes()
      val javaAstCreatorTypes   = runJavaSrcInterop(cpg, config, filesWithJavaExtension, kotlinAstCreatorTypes)
      val allAstCreatorTypes    = kotlinAstCreatorTypes ++ javaAstCreatorTypes

      TypeNodePass.withRegisteredTypes(allAstCreatorTypes, cpg).createAndApply()

      new ConfigPass(configFiles, cpg).createAndApply()
      new DependenciesFromMavenCoordinatesPass(mavenCoordinates, cpg).createAndApply()
    }
  }

  private def importNamesForFilesAtPaths(paths: Seq[String]): Seq[String] = {
    paths.flatMap { x =>
      val file = Paths.get(x)
      Files.lines(file).filter(_.startsWith("import")).toScala(Seq).map(ImportPattern.replaceAllIn(_, "$1").trim)
    }
  }

  private def gatherGradleParams(config: Config) = {
    Map(
      GradleConfigKeys.ProjectName       -> config.gradleProjectName,
      GradleConfigKeys.ConfigurationName -> config.gradleConfigurationName
    ).collect { case (key, Some(value)) => (key, value) }
  }

  private def downloadDependencies(sourceDir: String, config: Config): GradleDependencies.DepOutType = {
    val gradleParams   = gatherGradleParams(config)
    val resolverParams = DependencyResolverParams(Map.empty, gradleParams)

    DependencyResolver.getDependencies2(Paths.get(sourceDir), resolverParams) match {
      case (jarDeps, srcDeps) =>
        logger.info(s"Using ${jarDeps.size} dependency jars.")
        (jarDeps, srcDeps)
      case _ =>
        logger.warn(s"Could not fetch dependencies for project at path $sourceDir")
        println("Could not fetch dependencies when explicitly asked to.")
        (List.empty, List.empty)
    }
  }

  private def fetchMavenCoordinates(sourceDir: String, config: Config): Seq[String] = {
    val gradleParams   = gatherGradleParams(config)
    val resolverParams = DependencyResolverParams(Map.empty, gradleParams)

    DependencyResolver.getCoordinates(Paths.get(sourceDir), resolverParams) match {
      case Some(coordinates) =>
        logger.info(s"Found ${coordinates.size} maven coordinates.")
        coordinates.toSeq
      case None =>
        logger.warn(s"Could not fetch coordinates for project at path $sourceDir")
        println("Could not fetch coordinates when explicitly asked to.")
        Seq()
    }
  }

  private def findJarsIn(dirs: Set[String]) = {
    dirs.foldLeft(Seq[String]())((acc, classpathEntry) => {
      val f = Paths.get(classpathEntry)
      val files =
        if (Files.isDirectory(f))
          f.walk().filterNot(_ == f).filter(_.extension().getOrElse("") == JarExtension).map(_.toString)
        else Seq()
      acc ++ files
    })
  }

  private def entriesForSources(files: Iterable[KtFile], relativeTo: String): Iterable[KtFileWithMeta] = {
    val filesWithMeta = for {
      file    <- files
      relPath <- Try(SourceFiles.toRelativePath(file.getVirtualFilePath, relativeTo)).toOption
    } yield KtFileWithMeta(file, relPath, file.getVirtualFilePath)
    filesWithMeta.filterNot { fwp =>
      // TODO: add test for this type of filtering
      // TODO: support Windows paths
      val willFilter = SourceFilesPicker.shouldFilter(fwp.relativizedPath)
      if (willFilter) {
        logger.debug(s"Filtered file at `${fwp.f.getVirtualFilePath}`.")
      }
      willFilter
    }
  }

  private def entriesForConfigFiles(paths: Seq[String], sourceDir: String): Seq[FileContentAtPath] = {
    for {
      fileName     <- paths
      relPath      <- Try(SourceFiles.toRelativePath(fileName, sourceDir)).toOption
      fileContents <- Try(IOUtils.readEntireFile(Paths.get(fileName))).toOption
    } yield FileContentAtPath(fileContents, relPath, fileName)
  }

  private def createBindingContext(environment: KotlinCoreEnvironment): Try[BindingContext] = {
    Try {
      logger.info("Running Kotlin compiler analysis...")
      val t0             = System.nanoTime()
      val analysisResult = KotlinToJVMBytecodeCompiler.INSTANCE.analyze(environment)
      val t1             = System.nanoTime()
      logger.info(s"Kotlin compiler analysis finished in `${(t1 - t0) / 1000000}` ms.")
      analysisResult.getBindingContext
    }
  }
}
