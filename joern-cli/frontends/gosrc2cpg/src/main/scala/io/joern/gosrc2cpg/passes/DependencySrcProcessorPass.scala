package io.joern.gosrc2cpg.passes

import better.files.File
import io.joern.gosrc2cpg.Config
import io.joern.gosrc2cpg.astcreation.AstCreator
import io.joern.gosrc2cpg.datastructures.GoGlobal
import io.joern.gosrc2cpg.model.GoModHelper
import io.shiftleft.codepropertygraph.generated.Cpg
import org.slf4j.{Logger, LoggerFactory}

import scala.util.{Failure, Success, Try}

class DependencySrcProcessorPass(
  cpg: Cpg,
  astFiles: List[String],
  config: Config,
  goMod: GoModHelper,
  goGlobal: GoGlobal,
  tmpDir: File
) extends BasePassForAstProcessing(cpg, astFiles, config, goMod, goGlobal, tmpDir) {
  protected override val logger: Logger = LoggerFactory.getLogger(classOf[DependencySrcProcessorPass])

  override def processAst(diffGraph: DiffGraphBuilder, astCreator: AstCreator): Unit = {
    Try {
      astCreator.buildCacheFromDepSrc()
    } match {
      case Failure(exception) =>
        logger.warn(s"Failed to build the pre processing cache: '${astCreator.parserResult.fullPath}'", exception)
        (false, astCreator.relPathFileName)
      case Success(_) =>
        logger.info(s"Generated pre processing cache for: '${astCreator.parserResult.fullPath}'")
        (true, astCreator.relPathFileName)
    }
  }
}
