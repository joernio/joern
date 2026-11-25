package io.joern.php2cpg.passes

import io.joern.php2cpg.Config
import io.joern.php2cpg.astcreation.AstCreator
import io.joern.php2cpg.parser.{Domain, PhpParser}
import io.joern.php2cpg.passes.SymbolSummaryPass.SymbolSummary
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.report.ScanReport
import io.joern.x2cpg.utils.TimeUtils
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.shiftleft.utils.IOUtils

import java.nio.file.Paths
import scala.util.Try

class AstCreationPass(
  config: Config,
  cpg: Cpg,
  parser: PhpParser,
  summary: Map[String, Seq[SymbolSummary]],
  report: ScanReport = new ScanReport()
) extends ForkJoinParallelCpgPass[Array[String]](cpg)
    with AstParsingPass(config, parser, report) {

  override protected def processPart(builder: DiffGraphBuilder, fileName: String, result: Domain.PhpFile): Unit = {
    val relativeFilename = if (fileName == config.inputPath) {
      Paths.get(fileName).fileName
    } else {
      Paths.get(config.inputPath).relativize(Paths.get(fileName)).toString
    }
    val linesOfCode = Try(IOUtils.readLinesInFile(Paths.get(fileName)).size).getOrElse(-1)
    report.addReportInfo(relativeFilename, linesOfCode, parsed = true)
    val (_, duration) = TimeUtils.time {
      builder.absorb(
        new AstCreator(relativeFilename, fileName, result, config.disableFileContent, summary)(config.schemaValidation)
          .createAst()
      )
    }
    report.updateReport(relativeFilename, cpg = true, duration)
  }
}
