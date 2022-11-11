package io.joern.javasrc2cpg

import better.files.File
import com.github.javaparser.ParserConfiguration.LanguageLevel
import com.github.javaparser.{JavaParser, ParserConfiguration}
import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.Node.Parsedness
import com.github.javaparser.symbolsolver.JavaSymbolSolver
import com.github.javaparser.symbolsolver.resolution.typesolvers.JarTypeSolver
import io.joern.javasrc2cpg.passes.{AstCreationPass, ConfigFileCreationPass, TypeInferencePass}
import io.joern.javasrc2cpg.typesolvers.{CachingReflectionTypeSolver, SimpleCombinedTypeSolver, EagerSourceTypeSolver}
import io.joern.javasrc2cpg.util.{Delombok, SourceRootFinder}
import io.joern.javasrc2cpg.util.Delombok.DelombokMode
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.joern.x2cpg.passes.frontend.{MetaDataPass, TypeNodePass}
import io.joern.x2cpg.{SourceFiles, X2CpgFrontend}
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.utils.dependency.DependencyResolver
import org.slf4j.LoggerFactory

import java.nio.file.{Path, Paths}
import scala.collection.parallel.CollectionConverters._
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters.RichOptional
import scala.util.{Success, Try}

case class SourceDirectoryInfo(typeSolverSourceDirs: List[String], sourceFiles: List[SourceFileInfo])
case class SplitDirectories(analysisSourceDir: String, typesSourceDir: String)
case class SplitJpAsts(analysisAsts: List[JpAstWithMeta], typesAsts: List[JpAstWithMeta])
case class SourceFileInfo(analysisFileName: String, originalFilename: String)
case class JpAstWithMeta(fileInfo: SourceFileInfo, compilationUnit: CompilationUnit)

object JavaSrc2Cpg {
  val language: String = Languages.JAVASRC
  private val logger   = LoggerFactory.getLogger(this.getClass)

  val sourceFileExtensions: Set[String] = Set(".java")
  def apply(): JavaSrc2Cpg              = new JavaSrc2Cpg()

  def getDependencyList(config: Config): Seq[String] = {
    val codeDir = config.inputPath
    if (config.fetchDependencies) {
      DependencyResolver.getDependencies(Paths.get(codeDir)) match {
        case Some(deps) => deps.toSeq
        case None =>
          logger.warn(s"Could not fetch dependencies for project at path $codeDir")
          Seq()
      }
    } else {
      logger.info("dependency resolving disabled")
      Seq()
    }
  }
}

class JavaSrc2Cpg extends X2CpgFrontend[Config] {
  import JavaSrc2Cpg._

  private val logger = LoggerFactory.getLogger(this.getClass)
  override def createCpg(config: Config): Try[Cpg] = {
    withNewEmptyCpg(config.outputPath, config: Config) { (cpg, config) =>
      new MetaDataPass(cpg, language, config.inputPath).createAndApply()

      val sourceDirectories = getSourcePathsWithDelombok(config)
      val javaparserAsts    = getSplitJavaparserAsts(sourceDirectories, config)
      val symbolSolver      = createSymbolSolver(javaparserAsts.typesAsts, config)
      javaparserAsts.analysisAsts.map(_.compilationUnit).foreach(symbolSolver.inject)

      val astCreationPass = new AstCreationPass(javaparserAsts.analysisAsts, config, cpg, symbolSolver)
      astCreationPass.createAndApply()
      new ConfigFileCreationPass(config.inputPath, cpg).createAndApply()
      new TypeNodePass(astCreationPass.global.usedTypes.keys().asScala.toList, cpg).createAndApply()
      new TypeInferencePass(cpg).createAndApply()
    }
  }

  // private def populateCombinedTypeSolver(solver: CombinedTypeSolver, typesAsts: List[JpAstWithMeta], config: Config): Unit = {
  //   val reflectionTypeSolver = new CachingReflectionTypeSolver()
  //   val sourceTypeSolver = EagerSourceTypeSolver(typesAsts)

  //   // Source type solver should be the fastest, so check that first.
  //   solver.add(sourceTypeSolver)
  //   solver.add(reflectionTypeSolver)

  //   // Add solvers for inference jars
  //   val jarsList = config.inferenceJarPaths.flatMap(recursiveJarsFromPath).toList
  //   (jarsList ++ getDependencyList(config))
  //     .flatMap { path =>
  //       Try(new JarTypeSolver(path)).toOption
  //     }
  //     .foreach { solver.add(_) }
  // }

  private def getSourcesFromDir(sourceDir: String): List[String] = {
    SourceRootFinder.getSourceRoots(sourceDir).flatMap(SourceFiles.determine(_, sourceFileExtensions))
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

  private def getSplitJavaparserAsts(sourceDirectories: SplitDirectories, config: Config): SplitJpAsts = {
    val analysisSources = getSourcesFromDir(sourceDirectories.analysisSourceDir)
    val typesSources    = getSourcesFromDir(sourceDirectories.typesSourceDir)

    val analysisAstsMap = analysisSources.par.flatMap { sourceFilename =>
      val originalFilename = sourceFilename.replaceAll(sourceDirectories.analysisSourceDir, config.inputPath)
      val sourceFileInfo   = SourceFileInfo(sourceFilename, originalFilename)
      val maybeParsedFile  = parseFile(sourceFilename)

      maybeParsedFile.map(cu => sourceFilename -> JpAstWithMeta(sourceFileInfo, cu))
    }.toMap

    val analysisAsts = analysisAstsMap.values.toList
    val typesAsts = typesSources.par.flatMap { sourceFilename =>
      // Always want to usecombinedTypeSolver
      val sourceFileInfo = SourceFileInfo(sourceFilename, sourceFilename)
      analysisAstsMap
        .get(sourceFilename)
        .map(_.compilationUnit)
        .orElse(parseFile(sourceFilename))
        .map(cu => JpAstWithMeta(sourceFileInfo, cu))
    }.toList

    SplitJpAsts(analysisAsts, typesAsts)
  }

  private def getSourcePathsWithDelombok(config: Config): SplitDirectories = {
    val dependencies        = getDependencyList(config)
    val delombokMode        = getDelombokMode(config)
    val hasLombokDependency = dependencies.exists(_.contains("lombok"))
    val originalSourcesDir  = config.inputPath
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

  private def getDelombokMode(config: Config): DelombokMode = {
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

  private def createSymbolSolver(typesAsts: List[JpAstWithMeta], config: Config): JavaSymbolSolver = {
    val combinedTypeSolver   = new SimpleCombinedTypeSolver()
    val reflectionTypeSolver = new CachingReflectionTypeSolver()
    val sourceTypeSolver     = EagerSourceTypeSolver(typesAsts)

    combinedTypeSolver.appendSolver(sourceTypeSolver)
    combinedTypeSolver.appendSolver(reflectionTypeSolver)

    // Add solvers for inference jars
    val jarsList = config.inferenceJarPaths.flatMap(recursiveJarsFromPath).toList
    (jarsList ++ getDependencyList(config))
      .flatMap { path =>
        Try(new JarTypeSolver(path)).toOption
      }
      .foreach { combinedTypeSolver.appendSolver(_) }

    new JavaSymbolSolver(combinedTypeSolver)
  }

}
