package io.joern.csharpsrc2cpg.passes

import io.joern.csharpsrc2cpg.astcreation.AstCreator
import io.joern.x2cpg.report.{Category, ScanReport}
import io.joern.x2cpg.utils.TimeUtils
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.utils.IOUtils
import org.slf4j.{Logger, LoggerFactory}

import java.nio.file.Paths
import scala.util.{Failure, Success, Try}

class AstCreationPass(cpg: Cpg, astCreators: Seq[AstCreator], report: ScanReport)
    extends ForkJoinParallelCpgPass[AstCreator](cpg) {

  private val logger: Logger = LoggerFactory.getLogger(classOf[AstCreationPass])

  override def generateParts(): Array[AstCreator] = astCreators.toArray

  override def runOnPart(diffGraph: DiffGraphBuilder, astCreator: AstCreator): Unit = {
    val ((success, filename), duration) = TimeUtils.time {
      val fullPath = astCreator.parserResult.fullPath
      val fileLOC  = IOUtils.readLinesInFile(Paths.get(fullPath)).size
      report.addReportInfo(astCreator.relativeFileName, fileLOC, parsed = true)
      Try {
        val localDiff = astCreator.createAst()
        diffGraph.absorb(localDiff)
      } match {
        case Failure(exception) =>
          report.warn(
            Category.FileProcessing,
            "Failed to process the file",
            Some(astCreator.relativeFileName),
            Some(exception),
            logger
          )
          (false, astCreator.relativeFileName)
        case Success(_) =>
          logger.debug(s"Processed '$fullPath'")
          (true, astCreator.relativeFileName)
      }
    }
    report.updateReport(filename, cpg = success, duration)
  }

}
