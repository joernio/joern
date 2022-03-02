package io.joern.jssrc2cpg.passes

import io.joern.jssrc2cpg.astcreation.AstCreator
import io.joern.jssrc2cpg.parser.BabelJsonParser
import io.joern.jssrc2cpg.utils.Report
import io.joern.jssrc2cpg.utils.TimeUtils
import io.joern.jssrc2cpg.Config
import io.joern.jssrc2cpg.datastructures.Global
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.{ConcurrentWriterCpgPass, IntervalKeyPool}
import io.shiftleft.utils.IOUtils

import java.nio.file.Paths
import scala.jdk.CollectionConverters._

class AstCreationPass(
  cpg: Cpg,
  files: Set[(String, String)],
  keyPool: Option[IntervalKeyPool],
  config: Config,
  report: Report = new Report()
) extends ConcurrentWriterCpgPass[(String, String)](cpg, keyPool = keyPool) {

  private val global: Global = new Global()

  def usedTypes(): List[String] =
    global.usedTypes.keys().asScala.filterNot(_ == "ANY").toList

  override def generateParts(): Array[(String, String)] = files.toArray

  override def runOnPart(diffGraph: DiffGraphBuilder, input: (String, String)): Unit = {
    val (rootPath, jsonFilename) = input
    val (filename, duration) = TimeUtils.time {
      val parseResult = BabelJsonParser.readFile(Paths.get(rootPath), Paths.get(jsonFilename))
      val fileLOC     = IOUtils.readLinesInFile(Paths.get(parseResult.fullPath)).size
      report.addReportInfo(parseResult.filename, fileLOC, parsed = true)
      val localDiff = new DiffGraphBuilder
      new AstCreator(config, global, localDiff, parseResult).createAst()
      diffGraph.absorb(localDiff)
      parseResult.filename
    }
    report.updateReport(filename, cpg = true, duration)

  }

}
