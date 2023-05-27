package io.joern.kotlin2cpg.interop

import com.github.javaparser.symbolsolver.JavaSymbolSolver
import io.joern.javasrc2cpg.{JpAstWithMeta, SplitJpAsts, SourceFileInfo, SplitDirectories, Config => JavaSrcConfig}
import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.Node.Parsedness
import io.joern.javasrc2cpg.typesolvers.{CachingReflectionTypeSolver, EagerSourceTypeSolver, SimpleCombinedTypeSolver}
import io.joern.javasrc2cpg.JavaSrc2Cpg.sourceFileExtensions
import io.joern.javasrc2cpg.util.SourceRootFinder

import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable
import scala.jdk.OptionConverters.RichOptional
import com.github.javaparser.ParserConfiguration.LanguageLevel
import com.github.javaparser.{JavaParser, ParserConfiguration}
import org.slf4j.LoggerFactory

import scala.jdk.CollectionConverters.CollectionHasAsScala
import better.files.File
import io.joern.x2cpg.SourceFiles
import io.shiftleft.codepropertygraph.Cpg
import io.joern.javasrc2cpg.passes.{AstCreationPass => JavaSrcAstCreationPass}
import java.nio.file.Paths

object JavasrcInterop {
  private val logger = LoggerFactory.getLogger(getClass)

  val frontendConfig = JavaSrcConfig()

  def astCreationPass(paths: List[String], cpg: Cpg): JavaSrcAstCreationPass = {
    val javaSrcConfig  = JavaSrcConfig()
    val javaParserAsts = JavasrcInterop.javaParserAsts(paths, javaSrcConfig.inputPath)
    val symbolSolver   = JavasrcInterop.symbolSolver(javaParserAsts)
    new JavaSrcAstCreationPass(javaParserAsts.analysisAsts, javaSrcConfig, cpg, symbolSolver)
  }

  private def symbolSolver(javaParserAsts: SplitJpAsts): JavaSymbolSolver = {
    val symbolSolver = createSymbolSolver(javaParserAsts.typesAsts)
    javaParserAsts.analysisAsts.map(_.compilationUnit).foreach(symbolSolver.inject)
    javaParserAsts.typesAsts.map(_.compilationUnit).foreach(symbolSolver.inject)
    symbolSolver
  }

  private def javaParserAsts(paths: Seq[String], inputPath: String): SplitJpAsts = {
    paths.foldLeft(SplitJpAsts(List(), List())) { (acc, p) =>
      val splitDirectories = SplitDirectories(p, p)
      val splitParserAsts  = getSplitJavaparserAsts(splitDirectories, inputPath)
      SplitJpAsts(acc.analysisAsts ++ splitParserAsts.analysisAsts, acc.typesAsts ++ splitParserAsts.typesAsts)
    }
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

  private def getSourcesFromDir(sourcePath: String): List[String] = {
    val f = File(sourcePath)
    if (f.isDirectory)
      SourceRootFinder.getSourceRoots(sourcePath).flatMap(SourceFiles.determine(_, sourceFileExtensions))
    else if (f.hasExtension && f.extension.exists(f => sourceFileExtensions.contains(f)))
      List(sourcePath)
    else
      List.empty
  }

  private def escapeBackslash(path: String): String = {
    path.replaceAll(raw"\\", raw"\\\\")
  }

  private def getSplitJavaparserAsts(sourceDirectories: SplitDirectories, inputPath: String): SplitJpAsts = {
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

  private def createSymbolSolver(typesAsts: List[JpAstWithMeta]): JavaSymbolSolver = {
    val combinedTypeSolver   = new SimpleCombinedTypeSolver()
    val reflectionTypeSolver = new CachingReflectionTypeSolver()
    combinedTypeSolver.add(reflectionTypeSolver)

    val sourceTypeSolver = EagerSourceTypeSolver(typesAsts, combinedTypeSolver)
    combinedTypeSolver.prepend(sourceTypeSolver)

    new JavaSymbolSolver(combinedTypeSolver)
  }
}
