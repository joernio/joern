package io.joern.javasrc2cpg.passes

import com.github.javaparser.{JavaParser, ParserConfiguration}
import com.github.javaparser.ParserConfiguration.LanguageLevel
import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.Node.Parsedness
import com.github.javaparser.symbolsolver.JavaSymbolSolver
import com.github.javaparser.symbolsolver.javaparsermodel.JavaParserFacade
import com.github.javaparser.symbolsolver.resolution.typesolvers.{ClassLoaderTypeSolver, ReflectionTypeSolver}
import io.joern.javasrc2cpg.JavaSrc2Cpg.JavaSrcEnvVar
import io.joern.javasrc2cpg.astcreation.AstCreator
import io.joern.javasrc2cpg.passes.AstCreationPass.*
import io.joern.javasrc2cpg.typesolvers.{EagerSourceTypeSolver, JarTypeSolver, SimpleCombinedTypeSolver}
import io.joern.javasrc2cpg.util.Delombok.DelombokMode
import io.joern.javasrc2cpg.util.Delombok.DelombokMode.*
import io.joern.javasrc2cpg.util.{Delombok, SourceParser}
import io.joern.javasrc2cpg.{Config, JavaSrc2Cpg}
import io.joern.x2cpg.SourceFiles
import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.joern.x2cpg.passes.frontend.XTypeRecoveryConfig
import io.joern.x2cpg.utils.dependency.DependencyResolver
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.ForkJoinParallelCpgPassWithAccumulator
import io.shiftleft.semanticcpg.utils.FileUtil
import org.slf4j.LoggerFactory

import java.net.URLClassLoader
import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable
import scala.collection.parallel.CollectionConverters.*
import scala.collection.concurrent
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.RichOptional
import scala.util.{Failure, Success, Try}

class AstCreationPass(config: Config, cpg: Cpg, sourcesOverride: Option[List[String]] = None)
    extends ForkJoinParallelCpgPassWithAccumulator[String, AstCreationPass.Accumulator](cpg) {

  private val logger                = LoggerFactory.getLogger(classOf[AstCreationPass])
  private val loggedExceptionCounts = new ConcurrentHashMap[Class[?], Int]().asScala

  private var _usedTypes: Set[String] = Set.empty

  def usedTypes(): Set[String] = _usedTypes

  val (sourceParser, symbolSolver, combinedTypeSolver) = initParserAndUtils(config)

  override def createAccumulator(): AstCreationPass.Accumulator = AstCreationPass.Accumulator()

  override def mergeAccumulator(left: AstCreationPass.Accumulator, right: AstCreationPass.Accumulator): Unit = {
    left.usedTypes ++= right.usedTypes
  }

  override def onAccumulatorComplete(builder: DiffGraphBuilder, accumulator: AstCreationPass.Accumulator): Unit = {
    _usedTypes = accumulator.usedTypes.toSet
  }

  override def generateParts(): Array[String] = sourceParser.relativeFilenames.toArray

  override def runOnPart(
    diffGraph: DiffGraphBuilder,
    filename: String,
    accumulator: AstCreationPass.Accumulator
  ): Unit = {
    sourceParser.parseAnalysisFile(filename, !config.disableFileContent) match {
      case Some(compilationUnit, fileContent) =>
        symbolSolver.inject(compilationUnit)
        val contentToUse = if (!config.disableFileContent) fileContent.map(_.replaceAll("\r\n", "\n")) else None

        diffGraph.absorb(
          new AstCreator(
            filename,
            compilationUnit,
            contentToUse,
            accumulator,
            symbolSolver,
            config.keepTypeArguments,
            loggedExceptionCounts
          )(config.schemaValidation, config.disableTypeFallback)
            .createAst()
        )

      case None => logger.warn(s"Skipping AST creation for $filename")
    }
  }

  /** Clear JavaParser caches. Should only be invoked after we no longer need JavaParser, e.g. as soon as we've built
    * the AST layer for all files.
    */
  def clearJavaParserCaches(): Unit = {
    JavaParserFacade.clearInstances()
  }

  /** Close type solver resources (open JAR files, jrt: file systems). Should be called after all AST creation is done
    * and [[clearJavaParserCaches]] has been called.
    */
  def closeTypeSolvers(): Unit = combinedTypeSolver.close()

  private def initParserAndUtils(config: Config): (SourceParser, JavaSymbolSolver, SimpleCombinedTypeSolver) = {
    val dependencies                   = getDependencyList(config.inputPath)
    val sourceParser                   = SourceParser(config, sourcesOverride)
    val (symbolSolver, combinedSolver) = createSymbolSolver(config, dependencies, sourceParser)
    (sourceParser, symbolSolver, combinedSolver)
  }

  private def getDependencyList(inputPath: String): List[String] = {
    val envVarValue = Option(System.getenv(JavaSrcEnvVar.FetchDependencies.name))
    val shouldFetch = if (envVarValue.contains("no-fetch")) {
      logger.info(s"Disabling dependency fetching as envvar is set to \"no-fetch\"")
      false
    } else if (envVarValue.exists(_.nonEmpty)) {
      logger.info(s"Enabling dependency fetching: Environment variable ${JavaSrcEnvVar.FetchDependencies.name} is set")
      true
    } else if (config.fetchDependencies) {
      logger.info(s"Enabling dependency fetching: --fetch-dependencies flag was set")
      true
    } else {
      logger.info("dependency resolving not enabled")
      false
    }

    if (shouldFetch) {
      DependencyResolver.getDependencies(Paths.get(inputPath)) match {
        case Some(deps) => deps.toList
        case None =>
          logger.warn(s"Could not fetch dependencies for project at path $inputPath")
          List()
      }
    } else {
      List()
    }
  }

  private def createSymbolSolver(
    config: Config,
    dependencies: List[String],
    sourceParser: SourceParser
  ): (JavaSymbolSolver, SimpleCombinedTypeSolver) = {
    val verboseDebugLoggingEnvVarValue = Option(System.getenv(JavaSrcEnvVar.EnableVerboseTypeLogging.name))
    val enableVerboseTypeLogging       = verboseDebugLoggingEnvVarValue.isDefined || config.enableVerboseTypeLogging
    if (enableVerboseTypeLogging) {
      logger.warn(
        "Verbose type logging is enabled. This will impact performance and log size and should only be enabled for debugging specific issues!"
      )
    }
    val combinedTypeSolver = new SimpleCombinedTypeSolver(enableVerboseTypeLogging)
    val symbolSolver       = new JavaSymbolSolver(combinedTypeSolver)

    val jdkPathFromEnvVar = Option(System.getenv(JavaSrcEnvVar.JdkPath.name))
    val jdkPath = (config.jdkPath, jdkPathFromEnvVar) match {
      case (None, None) =>
        val javaHome = System.getProperty("java.home")
        logger.info(s"No explicit jdk-path set in , so using system java.home for JDK type information: $javaHome")
        javaHome

      case (None, Some(jdkPath)) =>
        logger.info(
          s"Using JDK path from environment variable ${JavaSrcEnvVar.JdkPath.name} for JDK type information: $jdkPath"
        )
        jdkPath

      case (Some(jdkPath), _) =>
        logger.info(s"Using JDK path set with jdk-path option for JDK type information: $jdkPath")
        jdkPath
    }

    combinedTypeSolver.addNonCachingTypeSolver(
      JarTypeSolver.fromPath(jdkPath, config.cacheJdkTypeSolver, enableVerboseTypeLogging)
    )

    val sourceTypeSolver =
      EagerSourceTypeSolver(sourceParser, combinedTypeSolver, symbolSolver, enableVerboseTypeLogging)

    combinedTypeSolver.addCachingTypeSolver(sourceTypeSolver)

    // Add solvers for inference jars
    val jarsList = config.inferenceJarPaths.flatMap(recursiveJarsFromPath).toList
    if (config.inferenceJarPaths.isEmpty) {
      logger.debug("No inference jar paths given")
    } else if (jarsList.isEmpty) {
      logger.warn(s"Could not find any inference jars at provided paths: ${config.inferenceJarPaths.mkString(",")}")
    } else {
      logger.debug(s"Using inference jars: ${jarsList.mkString(":")}")
    }
    (jarsList ++ dependencies)
      .foreach { path =>
        Try(JarTypeSolver.fromPath(path, useCache = true, enableVerboseTypeLogging = enableVerboseTypeLogging)) match {
          case Success(jarTypeSolver) =>
            combinedTypeSolver.addNonCachingTypeSolver(jarTypeSolver)
            if (enableVerboseTypeLogging) {
              logger.debug(s"Added JarTypeSolver for jar at $path")
            }
          case Failure(exception) =>
            logger.warn(s"Could not create JarTypeSolver for jar at $path", exception)
        }
      }

    (symbolSolver, combinedTypeSolver)
  }

  private def recursiveJarsFromPath(path: String): List[String] = {
    Try(Paths.get(path)) match {
      case Success(file) if Files.isDirectory(file) =>
        file
          .walk()
          .filterNot(_ == file)
          .map(_.absolutePathAsString)
          .filter(_.endsWith(".jar"))
          .toList

      case Success(file) if file.absolutePathAsString.endsWith(".jar") =>
        List(file.absolutePathAsString)

      case _ =>
        Nil
    }
  }
}

object AstCreationPass {
  case class Accumulator(usedTypes: mutable.HashSet[String] = mutable.HashSet.empty) {
    def registerType(typeName: String): Unit = usedTypes.add(typeName)
  }
}
