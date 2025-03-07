package io.joern.gosrc2cpg.passes

import io.joern.gosrc2cpg.Config
import io.joern.gosrc2cpg.astcreation.AstCreator
import io.joern.gosrc2cpg.datastructures.GoGlobal
import io.joern.gosrc2cpg.model.GoModHelper
import io.shiftleft.codepropertygraph.generated.Cpg
import org.slf4j.{Logger, LoggerFactory}

import java.nio.file.Path
import scala.util.{Failure, Success, Try}

class InitialMainSrcPass(
  cpg: Cpg,
  astFiles: List[String],
  config: Config,
  goMod: GoModHelper,
  goGlobal: GoGlobal,
  tmpDir: Path
) extends BasePassForAstProcessing(cpg, astFiles, config, goMod, goGlobal, tmpDir) {
  protected override val logger: Logger = LoggerFactory.getLogger(classOf[InitialMainSrcPass])

  override def processAst(diffGraph: DiffGraphBuilder, astCreator: AstCreator): Unit = {
    Try {
      diffGraph.absorb(astCreator.buildCacheFromMainSrc())
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
