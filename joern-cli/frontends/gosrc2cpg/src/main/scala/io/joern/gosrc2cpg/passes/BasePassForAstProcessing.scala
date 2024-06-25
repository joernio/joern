package io.joern.gosrc2cpg.passes

import better.files.File
import io.joern.gosrc2cpg.Config
import io.joern.gosrc2cpg.astcreation.AstCreator
import io.joern.gosrc2cpg.datastructures.GoGlobal
import io.joern.gosrc2cpg.model.GoModHelper
import io.joern.x2cpg.SourceFiles
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.ForkJoinParallelCpgPass
import org.slf4j.{Logger, LoggerFactory}

abstract class BasePassForAstProcessing(
  cpg: Cpg,
  astFiles: List[String],
  config: Config,
  goMod: GoModHelper,
  goGlobal: GoGlobal,
  tmpDir: File
) extends ForkJoinParallelCpgPass[String](cpg) {
  protected val logger: Logger                = LoggerFactory.getLogger(classOf[BasePassForAstProcessing])
  override def generateParts(): Array[String] = astFiles.toArray
  override def runOnPart(diffGraph: DiffGraphBuilder, ast: String): Unit = {
    try {
      processAst(
        diffGraph,
        new AstCreator(ast, SourceFiles.toRelativePath(ast, tmpDir.pathAsString).replace(".json", ""), goMod, goGlobal)(
          config.schemaValidation
        )
      )
    } catch
      case exception: Exception =>
        logger.error(s"error while processing file $ast", exception)
  }

  def processAst(diffGraph: DiffGraphBuilder, astCreator: AstCreator): Unit
}
