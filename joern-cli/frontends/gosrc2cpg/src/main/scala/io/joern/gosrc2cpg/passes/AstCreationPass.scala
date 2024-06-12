package io.joern.gosrc2cpg.passes

import io.joern.gosrc2cpg.astcreation.AstCreator
import io.joern.x2cpg.utils.{Report, TimeUtils}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.ConcurrentWriterCpgPass
import io.shiftleft.utils.IOUtils
import org.slf4j.{Logger, LoggerFactory}

import java.nio.file.Paths
import scala.util.{Failure, Success, Try}

class AstCreationPass(cpg: Cpg, astCreators: Seq[AstCreator], report: Report)
    extends ConcurrentWriterCpgPass[AstCreator](cpg) {
  private val logger: Logger                      = LoggerFactory.getLogger(classOf[AstCreationPass])
  override def generateParts(): Array[AstCreator] = astCreators.toArray
  override def runOnPart(diffGraph: DiffGraphBuilder, astCreator: AstCreator): Unit = {
    val ((gotCpg, filename), duration) = TimeUtils.time {
      val fileLOC = IOUtils.readLinesInFile(Paths.get(astCreator.originalFilePath)).size
      report.addReportInfo(astCreator.relPathFileName, fileLOC, parsed = true)
      Try {
        val localDiff = astCreator.createAst()
        astCreator.cleanup()
        diffGraph.absorb(localDiff)
      } match {
        case Failure(exception) =>
          logger.warn(s"Failed to generate a CPG for: '${astCreator.originalFilePath}'", exception)
          (false, astCreator.relPathFileName)
        case Success(_) =>
          logger.info(s"Generated a CPG for: '${astCreator.originalFilePath}'")
          (true, astCreator.relPathFileName)
      }
    }
    report.updateReport(filename, cpg = gotCpg, duration)
  }
}
