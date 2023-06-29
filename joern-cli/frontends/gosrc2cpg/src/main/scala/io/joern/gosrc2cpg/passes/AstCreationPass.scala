package io.joern.gosrc2cpg.passes

import io.joern.gosrc2cpg.Config
import io.joern.gosrc2cpg.astcreation.AstCreator
import io.joern.gosrc2cpg.parser.GoAstJsonParser
import io.joern.gosrc2cpg.utils.AstGenRunner.AstGenRunnerResult
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.utils.{Report, TimeUtils}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.ConcurrentWriterCpgPass
import io.shiftleft.utils.IOUtils
import org.slf4j.{Logger, LoggerFactory}

import java.nio.file.Paths
import scala.util.{Failure, Success, Try}

class AstCreationPass(cpg: Cpg, astGenRunnerResult: AstGenRunnerResult, config: Config, report: Report)
    extends ConcurrentWriterCpgPass[String](cpg) {
  private val logger: Logger                  = LoggerFactory.getLogger(classOf[AstCreationPass])
  override def generateParts(): Array[String] = astGenRunnerResult.parsedFiles.toArray
  override def runOnPart(diffGraph: DiffGraphBuilder, inputJsonFile: String): Unit = {
    val ((gotCpg, filename), duration) = TimeUtils.time {
      val parseResult     = GoAstJsonParser.readFile(Paths.get(inputJsonFile))
      val fileLOC         = IOUtils.readLinesInFile(Paths.get(parseResult.fullPath)).size
      val relPathFileName = SourceFiles.toRelativePath(parseResult.fullPath, config.inputPath)
      report.addReportInfo(parseResult.filename, fileLOC, parsed = true)
      Try {
        val localDiff = new AstCreator(relPathFileName, parseResult).createAst()
        diffGraph.absorb(localDiff)
      } match {
        case Failure(exception) =>
          logger.warn(s"Failed to generate a CPG for: '${parseResult.fullPath}'", exception)
          (false, relPathFileName)
        case Success(_) =>
          logger.info(s"Generated a CPG for: '${parseResult.fullPath}'")
          (true, relPathFileName)
      }
    }
    report.updateReport(filename, cpg = gotCpg, duration)
  }
}
