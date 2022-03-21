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
import org.slf4j.LoggerFactory

import java.util.concurrent.ConcurrentHashMap
import scala.jdk.OptionConverters.RichOptional
import scala.jdk.CollectionConverters._
import scala.util.{Success, Try}

case class Global(usedTypes: ConcurrentHashMap[String, Boolean] = new ConcurrentHashMap[String, Boolean]())

class AstCreationPass(codeDir: String, filenames: List[String], cpg: Cpg) extends ConcurrentWriterCpgPass[String](cpg) {

  val global: Global = Global()
  private val logger = LoggerFactory.getLogger(classOf[AstCreationPass])
  // TODO: Make this configurable
  private val JarsPath = Some("/home/johannes/.m2/repository")

  override def generateParts(): Array[String] = filenames.toArray

  override def runOnPart(diffGraph: DiffGraphBuilder, filename: String): Unit = {
    val solver         = typeSolver()
    val symbolResolver = new JavaSymbolSolver(solver)

    val parserConfig = new ParserConfiguration().setSymbolResolver(symbolResolver)
    val parser       = new JavaParser(parserConfig)
    val parseResult  = parser.parse(new java.io.File(filename))

    val typeInfoProvider = TypeInfoProvider(global)

    parseResult.getResult.toScala match {
      case Some(result) if result.getParsed == Parsedness.PARSED =>
        diffGraph.absorb(new AstCreator(filename, typeInfoProvider).createAst(result))
      case _ =>
        logger.warn("Cannot parse: " + filename)
        logger.warn("Problems: ", parseResult.getProblems.asScala.toList.map(_.toString))
        Iterator()
    }
  }

  private def jarsList: List[String] = {
    JarsPath match {
      case Some(path) => recursiveJarsFromPath(path)

      case _ => Nil
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
        logger.debug(s"Could not find any jars at path ${JarsPath}")
        Nil
    }
  }

  private def typeSolver() = {
    val combinedTypeSolver   = new CombinedTypeSolver()
    val reflectionTypeSolver = new ReflectionTypeSolver()
    val javaParserTypeSolver = new JavaParserTypeSolver(codeDir)
    combinedTypeSolver.add(reflectionTypeSolver)
    combinedTypeSolver.add(javaParserTypeSolver)

    // Add solvers for inference jars
    jarsList
      .flatMap { path =>
        Try(new JarTypeSolver(path)).toOption
      }
      .foreach { combinedTypeSolver.add(_) }

    combinedTypeSolver
  }

}
