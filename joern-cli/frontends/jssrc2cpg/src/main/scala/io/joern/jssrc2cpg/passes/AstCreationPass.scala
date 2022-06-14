package io.joern.jssrc2cpg.passes

import io.joern.jssrc2cpg.astcreation.AstCreator
import io.joern.jssrc2cpg.parser.BabelJsonParser
import io.joern.jssrc2cpg.utils.Report
import io.joern.jssrc2cpg.utils.TimeUtils
import io.joern.jssrc2cpg.Config
import io.joern.jssrc2cpg.utils.AstGenRunner.AstGenRunnerResult
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.ConcurrentWriterCpgPass
import io.shiftleft.utils.IOUtils
import org.slf4j.Logger
import org.slf4j.LoggerFactory

import java.nio.file.Paths
import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters.EnumerationHasAsScala
import scala.util.Failure
import scala.util.Success
import scala.util.Try

class AstCreationPass(cpg: Cpg, astGenRunnerResult: AstGenRunnerResult, config: Config, report: Report = new Report())
    extends ConcurrentWriterCpgPass[(String, String)](cpg) {

  private val logger: Logger = LoggerFactory.getLogger(classOf[AstCreationPass])

  private val usedTypes: ConcurrentHashMap[(String, String), Boolean] = new ConcurrentHashMap()

  override def generateParts(): Array[(String, String)] = astGenRunnerResult.parsedFiles.toArray

  def allUsedTypes(): List[(String, String)] =
    usedTypes.keys().asScala.filterNot { case (typeName, _) => typeName == Defines.ANY.label }.toList

  override def finish(): Unit = {
    astGenRunnerResult.skippedFiles.foreach { skippedFile =>
      val (rootPath, fileName) = skippedFile
      val filePath             = Paths.get(rootPath, fileName)
      val fileLOC              = IOUtils.readLinesInFile(filePath).size
      report.addReportInfo(fileName, fileLOC)
    }
  }

  override def runOnPart(diffGraph: DiffGraphBuilder, input: (String, String)): Unit = {
    val (rootPath, jsonFilename) = input
    val ((gotCpg, filename), duration) = TimeUtils.time {
      val parseResult = BabelJsonParser.readFile(Paths.get(rootPath), Paths.get(jsonFilename))
      val fileLOC     = IOUtils.readLinesInFile(Paths.get(parseResult.fullPath)).size
      report.addReportInfo(parseResult.filename, fileLOC, parsed = true)
      Try {
        val localDiff = new AstCreator(config, parseResult, usedTypes).createAst()
        diffGraph.absorb(localDiff)
      } match {
        case Failure(exception) =>
          logger.warn(s"Failed to generate a CPG for: '${parseResult.fullPath}'", exception)
          (false, parseResult.filename)
        case Success(_) =>
          logger.info(s"Generated a CPG for: '${parseResult.fullPath}'")
          (true, parseResult.filename)
      }
    }
    report.updateReport(filename, cpg = gotCpg, duration)
  }

}
