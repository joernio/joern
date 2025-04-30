package io.joern.php2cpg.passes

import io.joern.php2cpg.Config
import io.joern.php2cpg.astcreation.AstCreator
import io.joern.php2cpg.parser.PhpParser
import io.joern.x2cpg.{SourceFiles, ValidationMode}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.utils.FileUtil.*
import org.slf4j.LoggerFactory

import java.nio.file.Paths

class AstCreationPass(cpg: Cpg, astCreators: List[AstCreator]) extends ForkJoinParallelCpgPass[AstCreator](cpg) {

  private val logger = LoggerFactory.getLogger(this.getClass)

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
