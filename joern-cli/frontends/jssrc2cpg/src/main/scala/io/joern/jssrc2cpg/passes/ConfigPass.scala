package io.joern.jssrc2cpg.passes

import io.joern.jssrc2cpg.Config
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.frontendspecific.jssrc2cpg.Defines
import io.joern.x2cpg.utils.{Report, TimeUtils}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.NewConfigFile
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.utils.IOUtils
import org.slf4j.{Logger, LoggerFactory}

import java.nio.file.{Files, Path, Paths}

class ConfigPass(cpg: Cpg, config: Config, report: Report = new Report()) extends ForkJoinParallelCpgPass[Path](cpg) {

  private val logger: Logger = LoggerFactory.getLogger(getClass)

  protected val allExtensions: Set[String]      = Set(".json", ".js", ".vue", ".html", ".pug")
  protected val selectedExtensions: Set[String] = Set(".json", ".config.js", ".conf.js", ".vue", ".html", ".pug")

  override def generateParts(): Array[Path] =
    configFiles(config, allExtensions).toArray

  protected def fileContent(file: Path): Seq[String] =
    IOUtils.readLinesInFile(file)

  protected def configFiles(config: Config, extensions: Set[String]): Seq[Path] =
    SourceFiles
      .determine(config.inputPath, extensions)
      .filterNot(_.contains(Defines.NodeModulesFolder))
      .filter(f => selectedExtensions.exists(f.endsWith))
      .map(Paths.get(_))

  override def runOnPart(diffGraph: DiffGraphBuilder, file: Path): Unit = {
    val path = Paths.get(config.inputPath).toAbsolutePath.relativize(file).toString
    logger.debug(s"Adding file '$path' as config.")
    val (gotCpg, duration) = TimeUtils.time {
      val localDiff  = Cpg.newDiffGraphBuilder
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
