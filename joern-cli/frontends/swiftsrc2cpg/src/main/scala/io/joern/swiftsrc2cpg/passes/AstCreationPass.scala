package io.joern.swiftsrc2cpg.passes

import io.joern.swiftsrc2cpg.Config
import io.joern.swiftsrc2cpg.astcreation.AstCreator
import io.joern.swiftsrc2cpg.datastructures.SwiftGlobal
import io.joern.swiftsrc2cpg.parser.SwiftJsonParser
import io.joern.swiftsrc2cpg.utils.AstGenRunner.AstGenRunnerResult
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.datastructures.Global
import io.joern.x2cpg.utils.{Report, TimeUtils}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.ConcurrentWriterCpgPass
import io.shiftleft.utils.IOUtils
import org.slf4j.{Logger, LoggerFactory}

import java.nio.file.Paths
import scala.util.{Failure, Success, Try}
import scala.jdk.CollectionConverters.*

class AstCreationPass(cpg: Cpg, astGenRunnerResult: AstGenRunnerResult, config: Config, report: Report = new Report())(
  implicit withSchemaValidation: ValidationMode
) extends ConcurrentWriterCpgPass[(String, String)](cpg) {

  private val logger: Logger = LoggerFactory.getLogger(classOf[AstCreationPass])

  private val global = new SwiftGlobal()

  def typesSeen(): List[String] = global.usedTypes.keys().asScala.filterNot(_ == Defines.Any).toList

  override def generateParts(): Array[(String, String)] = astGenRunnerResult.parsedFiles.toArray

  override def finish(): Unit = {
    astGenRunnerResult.skippedFiles.foreach { skippedFile =>
      val (_, fileName) = skippedFile
      val filePath      = Paths.get(fileName)
      val fileLOC       = IOUtils.readLinesInFile(filePath).size
      report.addReportInfo(fileName, fileLOC)
    }
  }

  override def runOnPart(diffGraph: DiffGraphBuilder, input: (String, String)): Unit = {
    val (rootPath, jsonFilename) = input
    val ((gotCpg, filename), duration) = TimeUtils.time {
      val parseResult = SwiftJsonParser.readFile(Paths.get(rootPath), Paths.get(jsonFilename))
      val fileLOC     = IOUtils.readLinesInFile(Paths.get(parseResult.fullPath)).size
      report.addReportInfo(parseResult.filename, fileLOC, parsed = true)
      Try {
        val localDiff = new AstCreator(config, global, parseResult).createAst()
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
