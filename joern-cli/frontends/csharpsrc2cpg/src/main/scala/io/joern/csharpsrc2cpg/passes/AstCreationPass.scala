package io.joern.csharpsrc2cpg.passes

import io.joern.csharpsrc2cpg.Config
import io.joern.csharpsrc2cpg.astcreation.AstCreator
import io.joern.x2cpg.utils.{Report, TimeUtils}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.utils.IOUtils
import org.slf4j.{Logger, LoggerFactory}

import scala.util.{Try, Success, Failure}
import java.nio.file.Paths

class AstCreationPass(cpg: Cpg, astCreators: Seq[AstCreator], report: Report)
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
          logger.warn(s"Failed to generate a CPG for: '$fullPath'", exception)
          (false, astCreator.relativeFileName)
        case Success(_) =>
          logger.info(s"Generated a CPG for: '$fullPath'")
          (true, astCreator.relativeFileName)
      }
    }
    report.updateReport(filename, cpg = success, duration)
  }

}
