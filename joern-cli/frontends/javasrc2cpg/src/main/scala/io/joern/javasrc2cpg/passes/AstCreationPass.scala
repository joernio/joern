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
    val parserConfig =
      new ParserConfiguration()
        .setSymbolResolver(symbolResolver)
        .setLanguageLevel(javaLanguageLevel(config.javaFeatureSetVersion))
    val parser      = new JavaParser(parserConfig)
    val parseResult = parser.parse(new java.io.File(filename))

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
      logger.info("dependency resolving disabled")
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

  private def javaLanguageLevel(maybeVersion: Option[String]): LanguageLevel = {
    maybeVersion match {
      case Some("0")  => LanguageLevel.JAVA_1_0
      case Some("1")  => LanguageLevel.JAVA_1_1
      case Some("2")  => LanguageLevel.JAVA_1_2
      case Some("3")  => LanguageLevel.JAVA_1_3
      case Some("4")  => LanguageLevel.JAVA_1_4
      case Some("5")  => LanguageLevel.JAVA_5
      case Some("6")  => LanguageLevel.JAVA_6
      case Some("7")  => LanguageLevel.JAVA_7
      case Some("8")  => LanguageLevel.JAVA_8
      case Some("9")  => LanguageLevel.JAVA_9
      case Some("10") => LanguageLevel.JAVA_10
      case Some("11") => LanguageLevel.JAVA_11
      case Some("12") => LanguageLevel.JAVA_12
      case Some("13") => LanguageLevel.JAVA_13
      case Some("14") => LanguageLevel.JAVA_14
      case Some("15") => LanguageLevel.JAVA_15
      case Some("16") => LanguageLevel.JAVA_16
      case Some("17") => LanguageLevel.JAVA_17
      case Some(version) =>
        logger.warn(s"Unknown Java feature set version $version. Defaulting to current version.")
        LanguageLevel.CURRENT
      case None => LanguageLevel.CURRENT
    }
  }

}
