package io.joern.jssrc2cpg.passes

import better.files.File
import io.joern.jssrc2cpg.utils.Report
import io.joern.jssrc2cpg.utils.TimeUtils
import io.joern.jssrc2cpg.Config
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.NewConfigFile
import io.shiftleft.passes.ConcurrentWriterCpgPass
import io.shiftleft.utils.IOUtils
import org.slf4j.{Logger, LoggerFactory}

class ConfigPass(cpg: Cpg, files: Seq[File], config: Config, report: Report = new Report())
    extends ConcurrentWriterCpgPass[File](cpg) {

  private val logger: Logger = LoggerFactory.getLogger(getClass)

  override def generateParts(): Array[File] = files.toArray

  protected def fileContent(file: File): Seq[String] =
    IOUtils.readLinesInFile(file.path)

  override def runOnPart(diffGraph: DiffGraphBuilder, file: File): Unit = {
    val path = File(config.inputPath).path.toAbsolutePath.relativize(file.path).toString
    logger.debug(s"Adding file '$path' as config.")
    val (gotCpg, duration) = TimeUtils.time {
      val localDiff  = new DiffGraphBuilder
      val content    = fileContent(file)
      val loc        = content.size
      val configNode = NewConfigFile().name(path).content(content.mkString("\n"))
      report.addReportInfo(path, loc, parsed = true)
      localDiff.addNode(configNode)
      localDiff
    }
    diffGraph.absorb(gotCpg)
    report.updateReport(path, cpg = true, duration)
  }

}
