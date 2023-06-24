package io.joern.php2cpg.passes

import better.files.File
import io.joern.php2cpg.Config
import io.joern.php2cpg.astcreation.AstCreator
import io.joern.php2cpg.parser.PhpParser
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.datastructures.Global
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.ConcurrentWriterCpgPass
import org.slf4j.LoggerFactory

import scala.jdk.CollectionConverters._

class AstCreationPass(config: Config, cpg: Cpg, parser: PhpParser) extends ConcurrentWriterCpgPass[String](cpg) {

  private val logger = LoggerFactory.getLogger(this.getClass)

  val PhpSourceFileExtensions: Set[String] = Set(".php")

  override def generateParts(): Array[String] = SourceFiles.determine(config.inputPath, PhpSourceFileExtensions).toArray

  override def runOnPart(diffGraph: DiffGraphBuilder, filename: String): Unit = {
    val relativeFilename = File(config.inputPath).relativize(File(filename)).toString
    parser.parseFile(filename, config.phpIni) match {
      case Some(parseResult) =>
        diffGraph.absorb(new AstCreator(relativeFilename, parseResult).createAst())

      case None =>
        logger.warn(s"Could not parse file $filename. Results will be missing!")
    }
  }
}
