package io.joern.rubysrc2cpg.passes

import io.joern.rubysrc2cpg.Config
import io.joern.rubysrc2cpg.astcreation.AstCreator
import io.joern.rubysrc2cpg.parser.ResourceManagedParser
import io.joern.x2cpg.SourceFiles
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.ConcurrentWriterCpgPass
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory

class AstCreationPass(cpg: Cpg, astCreators: List[AstCreator]) extends ConcurrentWriterCpgPass[AstCreator](cpg) {

  private val logger = LoggerFactory.getLogger(getClass)

  override def generateParts(): Array[AstCreator] = astCreators.toArray

  override def runOnPart(diffGraph: DiffGraphBuilder, astCreator: AstCreator): Unit = {
    try {
      val ast = astCreator.createAst()
      diffGraph.absorb(ast)
    } catch {
      case ex: Exception =>
        logger.error(s"Error while processing AST for file - ${astCreator.fileName} - ", ex)
    }
  }
}
