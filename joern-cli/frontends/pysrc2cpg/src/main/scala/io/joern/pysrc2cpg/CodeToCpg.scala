package io.joern.pysrc2cpg

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.joern.pysrc2cpg.PythonVersion.PythonV2AndV3
import io.joern.pysrc2cpg.Py2Cpg.InputProvider
import io.joern.pythonparser.PyParser
import io.joern.x2cpg.ValidationMode
import org.slf4j.LoggerFactory

class CodeToCpg(
  cpg: Cpg,
  inputProvider: Iterable[InputProvider],
  schemaValidationMode: ValidationMode,
  enableFileContent: Boolean
) extends ForkJoinParallelCpgPass[InputProvider](cpg) {
  import CodeToCpg.logger

  override def generateParts(): Array[InputProvider] = inputProvider.toArray

  override def runOnPart(diffGraph: DiffGraphBuilder, inputProvider: InputProvider): Unit = {
    val inputPair              = inputProvider()
    val parser                 = new PyParser()
    val lineBreakCorrectedCode = inputPair.content.replace("\r\n", "\n").replace("\r", "\n")
    try {
      val astRoot    = parser.parse(lineBreakCorrectedCode)
      val nodeToCode = new NodeToCode(lineBreakCorrectedCode)
      val astVisitor = new PythonAstVisitor(inputPair.relFileName, nodeToCode, PythonV2AndV3, enableFileContent)(
        schemaValidationMode
      )
      astVisitor.convert(astRoot)

      diffGraph.absorb(astVisitor.createAst())
    } catch {
      case exception: Throwable =>
        val lineBreakWasCorrected = lineBreakCorrectedCode != inputPair.content
        handleParsingError(inputPair.relFileName, lineBreakCorrectedCode, lineBreakWasCorrected, exception, diffGraph)
    }
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

    logger.warn(
      s"Failed to convert${if (lineBreakWasCorrected) " line break corrected " else " "}file ${relFileName}",
      exception
    )
  }
}

object CodeToCpg {
  private val logger = LoggerFactory.getLogger(getClass)
}
