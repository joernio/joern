package io.joern.javasrc2cpg.passes

import better.files.File
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
import io.joern.javasrc2cpg.Config
import io.joern.javasrc2cpg.util.{CachingReflectionTypeSolver, SourceRootFinder}
import io.joern.x2cpg.datastructures.Global
import io.joern.x2cpg.utils.dependency.DependencyResolver
import org.slf4j.LoggerFactory

import java.nio.file.Paths
import scala.jdk.OptionConverters.RichOptional
import scala.jdk.CollectionConverters._
import scala.util.{Success, Try}

class AstCreationPass(codeDir: String, filenames: List[String], config: Config, cpg: Cpg)
    extends ConcurrentWriterCpgPass[String](cpg) {

  val global: Global              = new Global()
  private val logger              = LoggerFactory.getLogger(classOf[AstCreationPass])
  lazy private val symbolResolver = createSymbolSolver()

  override def generateParts(): Array[String] = filenames.toArray

  override def runOnPart(diffGraph: DiffGraphBuilder, filename: String): Unit = {
    val parserConfig = new ParserConfiguration().setSymbolResolver(symbolResolver)
    val parser       = new JavaParser(parserConfig)
    val parseResult  = parser.parse(new java.io.File(filename))

    parseResult.getProblems.asScala.toList match {
      case Nil => // Just carry on as usual
      case problems =>
        logger.warn(s"Encountered problems while parsing file $filename:")
        problems.foreach { problem =>
          logger.warn(s"- ${problem.getMessage}")
        }
    }

    parseResult.getResult.toScala match {
      case Some(result) if result.getParsed == Parsedness.PARSED =>
        diffGraph.absorb(new AstCreator(filename, result, global, symbolResolver).createAst())
      case _ =>
        logger.warn("Failed to parse file " + filename)
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

    SourceRootFinder.getSourceRoots(codeDir)

    val combinedTypeSolver   = new CombinedTypeSolver()
    val reflectionTypeSolver = new CachingReflectionTypeSolver()
    combinedTypeSolver.add(reflectionTypeSolver)

    // Add solvers for all detected sources roots
    SourceRootFinder.getSourceRoots(codeDir).foreach { srcDir =>
      val javaParserTypeSolver = new JavaParserTypeSolver(srcDir)
      combinedTypeSolver.add(javaParserTypeSolver)
    }

    val resolvedDeps = if (config.fetchDependencies) {
      DependencyResolver.getDependencies(Paths.get(codeDir)) match {
        case Some(deps) => deps
        case None =>
          logger.warn(s"Could not fetch dependencies for project at path $codeDir")
          Seq()
      }
    } else {
      Seq()
    }

    // Add solvers for inference jars
    (jarsList ++ resolvedDeps)
      .flatMap { path =>
        Try(new JarTypeSolver(path)).toOption
      }
      .foreach { combinedTypeSolver.add(_) }

    new JavaSymbolSolver(combinedTypeSolver)
  }

}
