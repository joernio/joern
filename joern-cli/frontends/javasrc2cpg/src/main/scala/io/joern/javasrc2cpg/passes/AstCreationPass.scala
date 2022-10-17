package io.joern.javasrc2cpg.passes

import better.files.File
import com.github.javaparser.ParserConfiguration.LanguageLevel
import com.github.javaparser.ast.Node.Parsedness
import com.github.javaparser.{JavaParser, ParserConfiguration}
import com.github.javaparser.symbolsolver.JavaSymbolSolver
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.ConcurrentWriterCpgPass
import com.github.javaparser.symbolsolver.resolution.typesolvers.{
  CombinedTypeSolver,
  JarTypeSolver,
  JavaParserTypeSolver
}
import io.joern.javasrc2cpg.{Config, SourceDirectoryInfo, SourceFileInfo}
import io.joern.javasrc2cpg.util.{CachingReflectionTypeSolver, SourceRootFinder}
import io.joern.x2cpg.datastructures.Global
import io.joern.x2cpg.utils.dependency.DependencyResolver
import org.slf4j.LoggerFactory

import java.nio.file.Paths
import scala.jdk.OptionConverters.RichOptional
import scala.jdk.CollectionConverters._
import scala.util.{Success, Try}

class AstCreationPass(sourceInfo: SourceDirectoryInfo, config: Config, cpg: Cpg, dependencies: Seq[String])
    extends ConcurrentWriterCpgPass[SourceFileInfo](cpg) {

  val global: Global              = new Global()
  private val logger              = LoggerFactory.getLogger(classOf[AstCreationPass])
  lazy private val symbolResolver = createSymbolSolver()

  override def generateParts(): Array[SourceFileInfo] = sourceInfo.sourceFiles.toArray

  override def runOnPart(diffGraph: DiffGraphBuilder, fileInfo: SourceFileInfo): Unit = {
    val parserConfig =
      new ParserConfiguration().setSymbolResolver(symbolResolver).setLanguageLevel(LanguageLevel.CURRENT)
    val parser      = new JavaParser(parserConfig)
    val parseResult = parser.parse(new java.io.File(fileInfo.analysisFileName))

    parseResult.getProblems.asScala.toList match {
      case Nil => // Just carry on as usual
      case problems =>
        logger.warn(s"Encountered problems while parsing file ${fileInfo.analysisFileName}:")
        problems.foreach { problem =>
          logger.warn(s"- ${problem.getMessage}")
        }
    }

    parseResult.getResult.toScala match {
      case Some(result) if result.getParsed == Parsedness.PARSED =>
        diffGraph.absorb(new AstCreator(fileInfo.originalFileName, result, global, symbolResolver).createAst())
      case _ =>
        logger.warn("Failed to parse file " + fileInfo.analysisFileName)
        Iterator()
    }
  }

  private def jarsList: List[String] = {
    config.inferenceJarPaths.flatMap(recursiveJarsFromPath).toList
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

  private def createSymbolSolver(): JavaSymbolSolver = {
    val combinedTypeSolver   = new CombinedTypeSolver()
    val reflectionTypeSolver = new CachingReflectionTypeSolver()
    combinedTypeSolver.add(reflectionTypeSolver)

    // Add solvers for all detected sources roots
    sourceInfo.typeSolverSourceDirs.foreach { srcDir =>
      val javaParserTypeSolver = new JavaParserTypeSolver(srcDir)
      combinedTypeSolver.add(javaParserTypeSolver)
    }

    // Add solvers for inference jars
    (jarsList ++ dependencies)
      .flatMap { path =>
        Try(new JarTypeSolver(path)).toOption
      }
      .foreach { combinedTypeSolver.add(_) }

    new JavaSymbolSolver(combinedTypeSolver)
  }

}
