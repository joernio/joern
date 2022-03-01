package io.joern.jssrc2cpg.passes

import io.joern.jssrc2cpg.JsSrc2Cpg
import io.joern.jssrc2cpg.astcreation.AstCreator
import io.joern.jssrc2cpg.parser.BabelJsonParser
import io.joern.jssrc2cpg.utils.Report
import io.joern.jssrc2cpg.utils.TimeUtils
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.{ConcurrentWriterCpgPass, IntervalKeyPool}
import io.shiftleft.utils.IOUtils

import java.nio.file.Paths

class AstCreationPass(
  cpg: Cpg,
  files: Set[(String, String)],
  keyPool: Option[IntervalKeyPool],
  config: JsSrc2Cpg.Config,
  report: Report = new Report()
) extends ConcurrentWriterCpgPass[(String, String)](cpg, keyPool = keyPool) {

  override def generateParts(): Array[(String, String)] = files.toArray

  override def runOnPart(diffGraph: DiffGraphBuilder, input: (String, String)): Unit = {
    val (rootPath, jsonFilename) = input
    val (gotCpg, duration) = TimeUtils.time {
      val maybeResult = BabelJsonParser.readFile(Paths.get(rootPath), Paths.get(jsonFilename))
      maybeResult match {
        case Some(parseResult) =>
          val fileLOC = IOUtils.readLinesInFile(Paths.get(parseResult.fullPath)).size
          report.addReportInfo(parseResult.filename, fileLOC, parsed = true)
          val localDiff = new DiffGraphBuilder
          new AstCreator(config, localDiff, parseResult).createAst()
          diffGraph.absorb(localDiff)
          true
        case None =>
          false
      }
    }
    report.updateReport(jsonFilename, gotCpg, duration)

  }

}
