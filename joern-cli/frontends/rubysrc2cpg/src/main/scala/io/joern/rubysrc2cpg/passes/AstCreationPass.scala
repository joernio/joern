package io.joern.rubysrc2cpg.passes

import io.joern.rubysrc2cpg.astcreation.AstCreator
import io.joern.rubysrc2cpg.parser.{RubyParser, RubyLexer}
import org.antlr.v4.runtime.tree.ParseTreeWalker
import org.antlr.v4.runtime.{CharStream, CharStreams, CommonTokenStream}
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.datastructures.Global
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.ConcurrentWriterCpgPass
import org.slf4j.LoggerFactory

import scala.jdk.CollectionConverters._

class AstCreationPass(inputPath: String, cpg: Cpg) extends ConcurrentWriterCpgPass[String](cpg) {

  private val logger = LoggerFactory.getLogger(this.getClass)
  val global         = new Global()

  val RubySourceFileExtensions: Set[String] = Set(".rb")

  override def generateParts(): Array[String] = SourceFiles.determine(inputPath, RubySourceFileExtensions).toArray

  override def runOnPart(diffGraph: DiffGraphBuilder, filename: String): Unit = {
    /*
    RubyParser.parseFile(filename) match {
      case Some(parseResult) =>
        diffGraph.absorb(new AstCreator(filename, parseResult, global).createAst())

      case None =>
        logger.warn(s"Could not parse file $filename. Results will be missing!")
    }
    */
  }

  def allUsedTypes: List[String] = global.usedTypes.keys().asScala.toList
}
