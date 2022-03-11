package io.joern.javasrc2cpg.passes

import com.github.javaparser.ast.Node.Parsedness
import com.github.javaparser.{JavaParser, ParserConfiguration}
import com.github.javaparser.symbolsolver.JavaSymbolSolver
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.ConcurrentWriterCpgPass
import com.github.javaparser.symbolsolver.resolution.typesolvers.{
  CombinedTypeSolver,
  JavaParserTypeSolver,
  ReflectionTypeSolver
}
import org.slf4j.LoggerFactory

import java.util.concurrent.ConcurrentHashMap
import scala.jdk.OptionConverters.RichOptional
import scala.jdk.CollectionConverters._

case class Global(usedTypes: ConcurrentHashMap[String, Boolean] = new ConcurrentHashMap[String, Boolean]())

class AstCreationPass(codeDir: String, filenames: List[String], cpg: Cpg) extends ConcurrentWriterCpgPass[String](cpg) {

  val global: Global = Global()
  private val logger = LoggerFactory.getLogger(classOf[AstCreationPass])

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

  private def typeSolver() = {
    val combinedTypeSolver   = new CombinedTypeSolver()
    val reflectionTypeSolver = new ReflectionTypeSolver()
    val javaParserTypeSolver = new JavaParserTypeSolver(codeDir)
    combinedTypeSolver.add(reflectionTypeSolver)
    combinedTypeSolver.add(javaParserTypeSolver)
    combinedTypeSolver
  }

}
