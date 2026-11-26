package io.joern.gosrc2cpg.passes

import io.joern.gosrc2cpg.Config
import io.joern.gosrc2cpg.astcreation.AstCreator
import io.joern.gosrc2cpg.datastructures.GoGlobal
import io.joern.gosrc2cpg.model.GoModHelper
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.report.{Category, ProblemSink}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.ForkJoinParallelCpgPass
import org.slf4j.{Logger, LoggerFactory}

import java.nio.file.Path
import scala.util.control.NonFatal

abstract class BasePassForAstProcessing(
  cpg: Cpg,
  astFiles: List[String],
  config: Config,
  goMod: GoModHelper,
  goGlobal: GoGlobal,
  tmpDir: Path,
  problems: ProblemSink = ProblemSink.LogOnly
) extends ForkJoinParallelCpgPass[String](cpg) {
  protected val logger: Logger                = LoggerFactory.getLogger(classOf[BasePassForAstProcessing])
  override def generateParts(): Array[String] = astFiles.toArray
  override def runOnPart(diffGraph: DiffGraphBuilder, ast: String): Unit = {
    val relativeFileName = SourceFiles.toRelativePath(ast, tmpDir.toString).replace(".json", "")
    try {
      processAst(diffGraph, new AstCreator(ast, relativeFileName, goMod, goGlobal)(config.schemaValidation))
    } catch
      case NonFatal(exception) =>
        problems.warn(
          Category.FileProcessing,
          "Failed to process the file",
          Some(relativeFileName),
          Some(exception),
          logger
        )
  }

  def processAst(diffGraph: DiffGraphBuilder, astCreator: AstCreator): Unit
}
