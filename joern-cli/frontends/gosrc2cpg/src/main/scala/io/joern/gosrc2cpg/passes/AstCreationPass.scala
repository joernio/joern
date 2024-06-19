package io.joern.gosrc2cpg.passes

import better.files.File
import io.joern.gosrc2cpg.Config
import io.joern.gosrc2cpg.astcreation.AstCreator
import io.joern.gosrc2cpg.datastructures.GoGlobal
import io.joern.gosrc2cpg.model.GoModHelper
import io.joern.gosrc2cpg.parser.GoAstJsonParser
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.astgen.ParserResult
import io.joern.x2cpg.utils.{Report, TimeUtils}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.ConcurrentWriterCpgPass
import io.shiftleft.utils.IOUtils
import org.slf4j.{Logger, LoggerFactory}

import java.nio.file.Paths
import scala.util.{Failure, Success, Try}

class AstCreationPass(
  cpg: Cpg,
  astFiles: List[String],
  config: Config,
  goMod: GoModHelper,
  goGlobal: GoGlobal,
  tmpDir: File,
  report: Report
) extends BasePassForAstProcessing(cpg, astFiles, config, goMod, goGlobal, tmpDir) {
  private val logger: Logger = LoggerFactory.getLogger(classOf[AstCreationPass])
  override def processAst(diffGraph: DiffGraphBuilder, astCreator: AstCreator): Unit = {
    val ((gotCpg, filename), duration) = TimeUtils.time {
      val fileLOC = IOUtils.readLinesInFile(Paths.get(astCreator.parserResult.fullPath)).size
      report.addReportInfo(astCreator.relPathFileName, fileLOC, parsed = true)
      Try {
        val localDiff = astCreator.createAst()
        diffGraph.absorb(localDiff)
      } match {
        case Failure(exception) =>
          logger.warn(s"Failed to generate a CPG for: '${astCreator.parserResult.fullPath}'", exception)
          (false, astCreator.relPathFileName)
        case Success(_) =>
          logger.info(s"Generated a CPG for: '${astCreator.parserResult.fullPath}'")
          (true, astCreator.relPathFileName)
      }
    }
    report.updateReport(filename, cpg = gotCpg, duration)
  }
}
