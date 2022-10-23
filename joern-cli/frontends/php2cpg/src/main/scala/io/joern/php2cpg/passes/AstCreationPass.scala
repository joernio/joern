package io.joern.php2cpg.passes

import io.joern.php2cpg.astcreation.AstCreator
import io.joern.php2cpg.parser.PhpParser
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.datastructures.Global
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.ConcurrentWriterCpgPass
import org.slf4j.LoggerFactory

import scala.jdk.CollectionConverters._

class AstCreationPass(inputPath: String, cpg: Cpg) extends ConcurrentWriterCpgPass[String](cpg) {

  private val logger = LoggerFactory.getLogger(this.getClass)
  val global         = new Global()

  val PhpSourceFileExtensions: Set[String] = Set(".php")

  override def generateParts(): Array[String] = SourceFiles.determine(inputPath, PhpSourceFileExtensions).toArray

  override def runOnPart(diffGraph: DiffGraphBuilder, filename: String): Unit = {
    PhpParser.parseFile(filename) match {
      case Some(parseResult) =>
        diffGraph.absorb(new AstCreator(filename, parseResult, global).createAst())

      case None =>
        logger.error(s"Could not parse file $filename. Results will be missing!")
    }
  }

  def allUsedTypes: List[String] = global.usedTypes.keys().asScala.toList
}
