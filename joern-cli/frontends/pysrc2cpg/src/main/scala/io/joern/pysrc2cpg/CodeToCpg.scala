package io.joern.pysrc2cpg

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.joern.pysrc2cpg.PythonVersion.PythonV2AndV3
import io.joern.pysrc2cpg.Py2Cpg.InputProvider
import io.joern.pythonparser.PyParser
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.report.{Category, ScanReport}
import io.joern.x2cpg.utils.TimeUtils
import org.slf4j.LoggerFactory

class CodeToCpg(
  cpg: Cpg,
  inputProvider: Iterable[InputProvider],
  schemaValidationMode: ValidationMode,
  enableFileContent: Boolean,
  report: ScanReport = new ScanReport()
) extends ForkJoinParallelCpgPass[InputProvider](cpg) {
  import CodeToCpg.logger

  override def generateParts(): Array[InputProvider] = inputProvider.toArray

  override def runOnPart(diffGraph: DiffGraphBuilder, inputProvider: InputProvider): Unit = {
    val inputPair              = inputProvider()
    val parser                 = new PyParser()
    val lineBreakCorrectedCode = inputPair.content.replace("\r\n", "\n").replace("\r", "\n")
    val linesOfCode            = lineBreakCorrectedCode.linesIterator.size
    report.addReportInfo(inputPair.relFileName, linesOfCode)
    val (gotCpg, duration) = TimeUtils.time {
      try {
        val astRoot = parser.parse(lineBreakCorrectedCode)
        report.addReportInfo(inputPair.relFileName, linesOfCode, parsed = true)
        val nodeToCode = new NodeToCode(lineBreakCorrectedCode)
        val astVisitor =
          new PythonAstVisitor(inputPair.relFileName, nodeToCode, PythonV2AndV3, enableFileContent, report)(
            schemaValidationMode
          )
        astVisitor.convert(astRoot)

        diffGraph.absorb(astVisitor.createAst())
        logger.debug(s"Processed '${inputPair.relFileName}'")
        true
      } catch {
        case exception: Throwable =>
          val lineBreakWasCorrected = lineBreakCorrectedCode != inputPair.content
          handleParsingError(inputPair.relFileName, lineBreakCorrectedCode, lineBreakWasCorrected, exception, diffGraph)
          false
      }
    }
    report.updateReport(inputPair.relFileName, gotCpg, duration)
  }

  def handleParsingError(
    relFileName: String,
    code: String,
    lineBreakWasCorrected: Boolean,
    exception: Throwable,
    diffGraph: DiffGraphBuilder
  ): Unit = {
    val contentOption = if (enableFileContent) {
      Some(code)
    } else {
      None
    }

    // We add the file content maily for debugging purposes.
    val nodeBuilder = new NodeBuilder(diffGraph)
    nodeBuilder.fileNode(relFileName, contentOption)

    report.warn(Category.FileProcessing, "Failed to process the file", Some(relFileName), Some(exception), logger)
  }
}

object CodeToCpg {
  private val logger = LoggerFactory.getLogger(getClass)
}
