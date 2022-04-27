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
  JavaParserTypeSolver,
  ReflectionTypeSolver
}
import io.joern.javasrc2cpg.util.{SourceRootFinder, TypeInfoProvider}
import io.joern.x2cpg.datastructures.Global
import io.joern.x2cpg.utils.dependency.{DependencyResolver, MavenDependencies}
import org.slf4j.LoggerFactory

import java.nio.file.Paths
import scala.jdk.OptionConverters.RichOptional
import scala.jdk.CollectionConverters._
import scala.util.{Success, Try}

class AstCreationPass(codeDir: String, filenames: List[String], inferenceJarPaths: Set[String], cpg: Cpg)
    extends ConcurrentWriterCpgPass[String](cpg) {

  val global: Global              = new Global()
  private val logger              = LoggerFactory.getLogger(classOf[AstCreationPass])
  lazy private val symbolResolver = createSymbolSolver()

  override def generateParts(): Array[String] = filenames.toArray

  override def runOnPart(diffGraph: DiffGraphBuilder, filename: String): Unit = {
    val parserConfig = new ParserConfiguration().setSymbolResolver(symbolResolver)
    val parser       = new JavaParser(parserConfig)
    val parseResult  = parser.parse(new java.io.File(filename))

    parseResult.getResult.toScala match {
      case Some(result) if result.getParsed == Parsedness.PARSED =>
        diffGraph.absorb(new AstCreator(filename, result, global).createAst())
      case _ =>
        logger.warn("Cannot parse: " + filename)
        logger.warn("Problems: ", parseResult.getProblems.asScala.toList.map(_.toString))
        Iterator()
    }
  }

  private def jarsList: List[String] = {
    inferenceJarPaths.flatMap(recursiveJarsFromPath).toList
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
    val reflectionTypeSolver = new ReflectionTypeSolver()
    combinedTypeSolver.add(reflectionTypeSolver)

    // Add solvers for all detected sources roots
    SourceRootFinder.getSourceRoots(codeDir).foreach { srcDir =>
      val javaParserTypeSolver = new JavaParserTypeSolver(srcDir)
      combinedTypeSolver.add(javaParserTypeSolver)
    }

    // Add solvers for inference jars
    (jarsList ++ DependencyResolver.getDependencies(Paths.get(codeDir)))
      .flatMap { path =>
        Try(new JarTypeSolver(path)).toOption
      }
      .foreach { combinedTypeSolver.add(_) }

    new JavaSymbolSolver(combinedTypeSolver)
  }

}
