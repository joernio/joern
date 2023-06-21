package io.joern.javasrc2cpg.passes

import com.github.javaparser.symbolsolver.JavaSymbolSolver
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.ConcurrentWriterCpgPass
import io.joern.javasrc2cpg.Config
import io.joern.x2cpg.datastructures.Global
import org.slf4j.LoggerFactory
import io.joern.x2cpg.passes.frontend.XTypeRecoveryConfig
import io.joern.x2cpg.utils.dependency.DependencyResolver
import java.nio.file.Paths
import io.joern.javasrc2cpg.typesolvers.CachingReflectionTypeSolver
import better.files.File
import java.net.URLClassLoader
import io.joern.javasrc2cpg.typesolvers.SimpleCombinedTypeSolver
import io.joern.javasrc2cpg.typesolvers.EagerSourceTypeSolver
import com.github.javaparser.symbolsolver.resolution.typesolvers.JarTypeSolver
import scala.util.Try
import scala.util.Success
import io.joern.javasrc2cpg.util.SourceRootFinder
import io.joern.javasrc2cpg.JavaSrc2Cpg
import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ParserConfiguration
import com.github.javaparser.ParserConfiguration.LanguageLevel
import com.github.javaparser.JavaParser
import io.joern.x2cpg.SourceFiles
import io.joern.javasrc2cpg.passes.AstCreationPass.getSplitJavaparserAsts

import scala.jdk.OptionConverters.RichOptional
import scala.jdk.CollectionConverters._
import scala.collection.parallel.CollectionConverters._
import io.joern.javasrc2cpg.util.Delombok
import io.joern.javasrc2cpg.util.Delombok.DelombokMode
import com.github.javaparser.ast.Node.Parsedness

case class SourceDirectoryInfo(typeSolverSourceDirs: List[String], sourceFiles: List[SourceFileInfo])
case class SplitDirectories(analysisSourceDir: String, typesSourceDir: String)
case class SplitJpAsts(analysisAsts: List[JpAstWithMeta], typesAsts: List[JpAstWithMeta])
case class SourceFileInfo(analysisFileName: String, originalFilename: String)
case class JpAstWithMeta(fileInfo: SourceFileInfo, compilationUnit: CompilationUnit)

class AstCreationPass(config: Config, cpg: Cpg, preCreatedAsts: Option[SplitJpAsts] = None)
    extends ConcurrentWriterCpgPass[JpAstWithMeta](cpg) {

  val global: Global = new Global()
  private val logger = LoggerFactory.getLogger(classOf[AstCreationPass])

  private val javaparserAsts = preCreatedAsts.getOrElse {
    val sourceDirectories = getSourcePathsWithDelombok()
    getSplitJavaparserAsts(sourceDirectories)
  }
  private val symbolSolver = createAndInjectSymbolSolver(javaparserAsts)

  override def generateParts(): Array[JpAstWithMeta] = {
    val asts = javaparserAsts.analysisAsts.toArray
    if (asts.isEmpty) {
      throw new RuntimeException(s"Could not find any parseable Java files at input path ${config.inputPath}")
    }
    logger.info(s"Found ${asts.size} files.")
    asts.toArray
  }

  override def runOnPart(diffGraph: DiffGraphBuilder, astWithMeta: JpAstWithMeta): Unit = {
    val originalFilename = astWithMeta.fileInfo.originalFilename
    val result           = astWithMeta.compilationUnit
    diffGraph.absorb(new AstCreator(originalFilename, result, global, symbolSolver).createAst())
  }

  private def getDependencyList(): List[String] = {
    val codeDir = config.inputPath
    if (config.fetchDependencies) {
      DependencyResolver.getDependencies(Paths.get(codeDir)) match {
        case Some(deps) => deps.toList
        case None =>
          logger.warn(s"Could not fetch dependencies for project at path $codeDir")
          List()
      }
    } else {
      logger.info("dependency resolving disabled")
      List()
    }
  }

  private def createAndInjectSymbolSolver(jpAsts: SplitJpAsts): JavaSymbolSolver = {
    val symbolSolver = createSymbolSolver(jpAsts.typesAsts)
    javaparserAsts.analysisAsts.map(_.compilationUnit).foreach(symbolSolver.inject)
    javaparserAsts.typesAsts.map(_.compilationUnit).foreach(symbolSolver.inject)
    symbolSolver
  }

  private def createSymbolSolver(typesAsts: List[JpAstWithMeta]): JavaSymbolSolver = {
    val dependencyList = getDependencyList()

    val combinedTypeSolver   = new SimpleCombinedTypeSolver()
    val reflectionTypeSolver = new CachingReflectionTypeSolver()
    combinedTypeSolver.add(reflectionTypeSolver)

    val sourceTypeSolver = EagerSourceTypeSolver(typesAsts, combinedTypeSolver)
    // The sourceTypeSolver will often be the fastest due to there being no possibility of encountering a SOE on lookup.
    combinedTypeSolver.prepend(sourceTypeSolver)

    // Add solvers for inference jars
    val jarsList = config.inferenceJarPaths.flatMap(recursiveJarsFromPath).toList
    (jarsList ++ dependencyList)
      .flatMap { path =>
        Try(new JarTypeSolver(path)).toOption
      }
      .foreach { combinedTypeSolver.add(_) }

    new JavaSymbolSolver(combinedTypeSolver)
  }

  private def recursiveJarsFromPath(path: String): List[String] = {
    Try(File(path)) match {
      case Success(file) if file.isDirectory =>
        file.listRecursively
          .map(_.canonicalPath)
          .filter(_.endsWith(".jar"))
          .toList

      case Success(file) if file.canonicalPath.endsWith(".jar") =>
        List(file.canonicalPath)

      case _ =>
        Nil
    }
  }

  private def escapeBackslash(path: String): String = {
    path.replaceAll(raw"\\", raw"\\\\")
  }

  private def getSourcePathsWithDelombok(): SplitDirectories = {
    val dependencies        = getDependencyList()
    val delombokMode        = getDelombokMode()
    val hasLombokDependency = dependencies.exists(_.contains("lombok"))
    val originalSourcesDir  = File(config.inputPath).canonicalPath
    lazy val delombokDir    = Delombok.run(originalSourcesDir, config.delombokJavaHome)

    delombokMode match {
      case DelombokMode.Default if hasLombokDependency =>
        logger.info(s"Analysing delomboked code as lombok dependency was found.")
        SplitDirectories(delombokDir, delombokDir)

      case DelombokMode.RunDelombok =>
        SplitDirectories(delombokDir, delombokDir)

      case DelombokMode.TypesOnly =>
        SplitDirectories(originalSourcesDir, delombokDir)

      case _ => SplitDirectories(originalSourcesDir, originalSourcesDir)
    }
  }

  private def getDelombokMode(): DelombokMode = {
    config.delombokMode.map(_.toLowerCase) match {
      case None                 => DelombokMode.Default
      case Some("no-delombok")  => DelombokMode.NoDelombok
      case Some("default")      => DelombokMode.Default
      case Some("types-only")   => DelombokMode.TypesOnly
      case Some("run-delombok") => DelombokMode.RunDelombok
      case Some(value) =>
        logger.warn(s"Found unrecognised delombok mode `$value`. Using default instead.")
        DelombokMode.Default
    }
  }

}

object AstCreationPass {
  private val logger = LoggerFactory.getLogger(classOf[AstCreationPass])

  def getSplitJavaparserAsts(sourceDirectories: SplitDirectories): SplitJpAsts = {
    val analysisSources = getSourcesFromDir(sourceDirectories.analysisSourceDir)
    val typesSources    = getSourcesFromDir(sourceDirectories.typesSourceDir)

    val analysisAstsMap = analysisSources.par.flatMap { sourceFilename =>
      val originalFilename =
        Paths.get(sourceDirectories.analysisSourceDir).relativize(Paths.get(sourceFilename)).toString
      val sourceFileInfo  = SourceFileInfo(sourceFilename, originalFilename)
      val maybeParsedFile = parseFile(sourceFilename)

      maybeParsedFile.map(cu => sourceFilename -> JpAstWithMeta(sourceFileInfo, cu))
    }.toMap

    val analysisAsts = analysisAstsMap.values.toList
    val typesAsts = typesSources.par.flatMap { sourceFilename =>
      val sourceFileInfo = SourceFileInfo(sourceFilename, sourceFilename)
      analysisAstsMap
        .get(sourceFilename)
        .map(_.compilationUnit)
        .orElse(parseFile(sourceFilename))
        .map(cu => JpAstWithMeta(sourceFileInfo, cu))
    }.toList

    SplitJpAsts(analysisAsts, typesAsts)
  }

  /** Will extract source files from the given path if it is a directory, or in the case of a single file, will check
    * the file's extension and return a singleton list of the file if the file extension is supported.
    * @param sourcePath
    *   the input directory or source file.
    * @return
    *   a list of all source files.
    */
  private def getSourcesFromDir(sourcePath: String): List[String] = {
    val f = File(sourcePath)
    if (f.isDirectory)
      SourceRootFinder.getSourceRoots(sourcePath).flatMap(SourceFiles.determine(_, JavaSrc2Cpg.sourceFileExtensions))
    else if (f.hasExtension && f.extension.exists(f => JavaSrc2Cpg.sourceFileExtensions.contains(f)))
      List(sourcePath)
    else
      List.empty
  }

  private def parseFile(filename: String): Option[CompilationUnit] = {
    val config      = new ParserConfiguration().setLanguageLevel(LanguageLevel.BLEEDING_EDGE)
    val parseResult = new JavaParser(config).parse(new java.io.File(filename))

    parseResult.getProblems.asScala.toList match {
      case Nil => // Just carry on as usual
      case problems =>
        logger.warn(s"Encountered problems while parsing file $filename:")
        problems.foreach { problem =>
          logger.warn(s"- ${problem.getMessage}")
        }
    }

    parseResult.getResult.toScala match {
      case Some(result) if result.getParsed == Parsedness.PARSED => Some(result)
      case _ =>
        logger.warn(s"Failed to parse file $filename")
        None
    }
  }
}
