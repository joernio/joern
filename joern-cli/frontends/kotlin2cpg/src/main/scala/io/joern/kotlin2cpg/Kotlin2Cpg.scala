package io.joern.kotlin2cpg

import better.files.File
import io.joern.kotlin2cpg.files.SourceFilesPicker
import org.jetbrains.kotlin.psi.KtFile

import scala.jdk.CollectionConverters.{CollectionHasAsScala, EnumerationHasAsScala}
import io.joern.kotlin2cpg.passes.{AstCreationPass, ConfigPass}
import io.joern.x2cpg.passes.frontend.{MetaDataPass, TypeNodePass}

import io.joern.kotlin2cpg.compiler.{CompilerAPI, ErrorLoggingMessageCollector}
import io.joern.kotlin2cpg.types.{ContentSourcesPicker, DefaultTypeInfoProvider}
import io.joern.kotlin2cpg.utils.PathUtils
import io.shiftleft.codepropertygraph.Cpg
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.utils.dependency.{DependencyResolver, DependencyResolverParams, GradleConfigKeys}
import io.joern.x2cpg.{SourceFiles, X2CpgFrontend}
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.utils.IOUtils
import org.slf4j.LoggerFactory
import io.shiftleft.semanticcpg.language._

import java.nio.file.{Files, Paths}
import scala.util.Try

object Kotlin2Cpg {
  val language = "KOTLIN"
  case class InputPair(content: String, fileName: String)
  type InputProvider = () => InputPair
}

case class KtFileWithMeta(f: KtFile, relativizedPath: String, filename: String)
case class FileContentAtPath(content: String, relativizedPath: String, filename: String)

class Kotlin2Cpg extends X2CpgFrontend[Config] {
  private val logger = LoggerFactory.getLogger(getClass)
  val parsingError   = "KOTLIN2CPG_PARSING_ERROR"
  private val defaultKotlinStdlibContentRootJarPaths = Seq(
    DefaultContentRootJarPath("jars/kotlin-stdlib-1.6.0.jar", isResource = true),
    DefaultContentRootJarPath("jars/kotlin-stdlib-common-1.6.0.jar", isResource = true),
    DefaultContentRootJarPath("jars/kotlin-stdlib-jdk8-1.6.0.jar", isResource = true)
  )

  def createCpg(config: Config): Try[Cpg] = {
    withNewEmptyCpg(config.outputPath, config) { (cpg, config) =>
      val sourceDir = config.inputPath
      if (!Files.exists(Paths.get(sourceDir))) {
        println(s"The specified input path `$sourceDir` is not a file that exists. Exiting.")
        System.exit(1)
      }
      if (!Files.isDirectory(Paths.get(sourceDir))) {
        println(s"The specified input path `$sourceDir` is not a directory. Exiting.")
        System.exit(1)
      }

      val maxHeapSize          = Runtime.getRuntime().maxMemory()
      val formattedMaxHeapSize = String.format("%,.2f", maxHeapSize / (1024 * 1024 * 1024).toDouble)
      logger.info(s"Max heap size currently set to `${formattedMaxHeapSize}GB`.")

      val dependenciesPaths = if (config.downloadDependencies) {
        downloadDependencies(sourceDir, config)
      } else {
        logger.info(s"Not using any dependency jars.")
        Seq()
      }

      val filesWithKtExtension = SourceFiles.determine(sourceDir, Set(".kt"))
      if (filesWithKtExtension.isEmpty) {
        println(s"The provided input directory does not contain files ending in '.kt' `$sourceDir`. Exiting.")
        System.exit(1)
      }
      logger.info(s"Starting CPG generation for input directory `$sourceDir`.")

      val filesWithJavaExtension = SourceFiles.determine(sourceDir, Set(".java"))
      if (filesWithJavaExtension.nonEmpty) {
        logger.info(
          s"Found ${filesWithJavaExtension.size} files with the `.java` extension which will not be included in the result."
        )
      }

      val jarsAtConfigClassPath = findJarsIn(config.classpath)
      if (config.classpath.nonEmpty) {
        if (jarsAtConfigClassPath.nonEmpty) {
          logger.info(s"Found ${jarsAtConfigClassPath.size} jars in the specified classpath.")
        } else {
          logger.warn("No jars found in the specified classpath.")
        }
      }
      val stdlibJars =
        if (config.withStdlibJarsInClassPath) defaultKotlinStdlibContentRootJarPaths
        else Seq()
      val defaultContentRootJars = stdlibJars ++
        jarsAtConfigClassPath.map { path => DefaultContentRootJarPath(path, false) } ++
        dependenciesPaths.map { path =>
          DefaultContentRootJarPath(path, false)
        }
      val messageCollector        = new ErrorLoggingMessageCollector
      val dirsForSourcesToCompile = ContentSourcesPicker.dirsForRoot(sourceDir)
      if (dirsForSourcesToCompile.isEmpty) {
        logger.warn("The list of directories to analyze is empty.")
      }
      val plugins = Seq()
      val environment =
        CompilerAPI.makeEnvironment(dirsForSourcesToCompile, defaultContentRootJars, plugins, messageCollector)

      val sourceEntries = entriesForSources(environment.getSourceFiles.asScala, sourceDir)
      val sources = sourceEntries.filterNot { entry =>
        config.ignorePaths.exists { pathToIgnore =>
          val parent = Paths.get(pathToIgnore).toAbsolutePath()
          val child  = Paths.get(entry.filename)
          child.startsWith(parent)
        }
      }
      val configFiles      = entriesForConfigFiles(SourceFilesPicker.configFiles(sourceDir), sourceDir)
      val typeInfoProvider = new DefaultTypeInfoProvider(environment)

      new MetaDataPass(cpg, Languages.KOTLIN, config.inputPath).createAndApply()
      val astCreator = new AstCreationPass(sources, typeInfoProvider, cpg)
      astCreator.createAndApply()
      new TypeNodePass(astCreator.global.usedTypes.keys().asScala.toList, cpg).createAndApply()

      val configCreator = new ConfigPass(configFiles, cpg)
      configCreator.createAndApply()

      val hasAtLeastOneMethodNode = cpg.method.take(1).nonEmpty
      if (!hasAtLeastOneMethodNode) {
        logger.warn("Resulting CPG does not contain any METHOD nodes.")
      }
    }
  }

  private def downloadDependencies(sourceDir: String, config: Config): scala.collection.Seq[String] = {
    val gradleParams = Map(
      GradleConfigKeys.ProjectName       -> config.gradleProjectName,
      GradleConfigKeys.ConfigurationName -> config.gradleConfigurationName
    ).collect { case (key, Some(value)) => (key, value) }

    val resolverParams = DependencyResolverParams(Map.empty, gradleParams)
    DependencyResolver.getDependencies(Paths.get(sourceDir), resolverParams) match {
      case Some(deps) =>
        logger.info(s"Using ${deps.size} dependency jars.")
        deps
      case None =>
        logger.warn(s"Could not fetch dependencies for project at path $sourceDir")
        println("Could not fetch dependencies when explicitly asked to. Exiting.")
        System.exit(1)
        Seq()
    }
  }

  private def findJarsIn(dirs: Set[String]) = {
    val jarExtension = ".jar"
    dirs.foldLeft(Seq[String]())((acc, classpathEntry) => {
      val f = File(classpathEntry)
      val files =
        if (f.isDirectory) f.listRecursively.filter(_.extension.getOrElse("") == jarExtension).map(_.toString)
        else Seq()
      acc ++ files
    })
  }

  private def entriesForSources(files: Iterable[KtFile], relativeTo: String): Iterable[KtFileWithMeta] = {
    files
      .flatMap { f =>
        try {
          val relPath = PathUtils.relativize(relativeTo, f.getVirtualFilePath)
          Some(f, relPath)
        } catch {
          case _: Throwable => None
        }
      }
      .map { fwp =>
        KtFileWithMeta(fwp._1, fwp._2, fwp._1.getVirtualFilePath)
      }
      .filterNot { fwp =>
        // TODO: add test for this type of filtering
        // TODO: support Windows paths
        val willFilter = SourceFilesPicker.shouldFilter(fwp.relativizedPath)
        if (willFilter) {
          logger.debug(s"Filtered file at `${fwp.f.getVirtualFilePath}`.")
        }
        willFilter
      }
  }

  private def entriesForConfigFiles(paths: Seq[String], relativeTo: String): Seq[FileContentAtPath] = {
    paths
      .flatMap { fileName =>
        try {
          val relPath = PathUtils.relativize(relativeTo, fileName)
          Some(fileName, relPath)
        } catch {
          case _: Throwable => None
        }
      }
      .map { fnm =>
        val fileContents =
          try {
            IOUtils.readLinesInFile(Paths.get(fnm._1)).mkString("\n")
          } catch {
            case t: Throwable => parsingError + "\n" + t.toString
          }
        FileContentAtPath(fileContents, fnm._2, fnm._1)
      }
  }
}
