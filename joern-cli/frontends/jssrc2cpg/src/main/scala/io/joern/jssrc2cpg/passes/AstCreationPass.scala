package io.joern.jssrc2cpg.passes

import io.joern.jssrc2cpg.JsSrc2Cpg
import io.joern.jssrc2cpg.utils.Report
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.{ConcurrentWriterCpgPass, IntervalKeyPool}

class AstCreationPass(
  cpg: Cpg,
  files: Set[String],
  keyPool: Option[IntervalKeyPool],
  config: JsSrc2Cpg.Config,
  report: Report = new Report()
) extends ConcurrentWriterCpgPass[String](cpg, keyPool = keyPool) {

  override def generateParts(): Array[String] = files.toArray

  override def runOnPart(diffGraph: DiffGraphBuilder, filename: String): Unit = {
    println(filename)
    println(config)
    println(report)

    ??? // TODO

    /** val path = Paths.get(filename) val fileLOC = IOUtils.readLinesInFile(path).size val (gotCpg, duration) =
      * TimeUtils.time { val parseResult = parser.parse(path) parseResult match { case Some(translationUnit) =>
      * report.addReportInfo(filename, fileLOC, parsed = true) val localDiff = new DiffGraphBuilder new
      * AstCreator(filename, config, global, localDiff, translationUnit).createAst() diffGraph.absorb(localDiff) true
      * case None => report.addReportInfo(filename, fileLOC) false } } report.updateReport(filename, gotCpg, duration)
      */
  }

}
