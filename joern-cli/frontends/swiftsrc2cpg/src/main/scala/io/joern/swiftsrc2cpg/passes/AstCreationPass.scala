package io.joern.swiftsrc2cpg.passes

import io.joern.swiftsrc2cpg.Config
import io.joern.swiftsrc2cpg.astcreation.AstCreator
import io.joern.swiftsrc2cpg.parser.SwiftJsonParser
import io.joern.swiftsrc2cpg.utils.AstGenRunner.AstGenRunnerResult
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.datastructures.Global
import io.joern.x2cpg.frontendspecific.swiftsrc2cpg.Defines
import io.joern.x2cpg.utils.{Report, TimeUtils}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.utils.IOUtils
import org.slf4j.{Logger, LoggerFactory}

import java.nio.file.Paths
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try}

class AstCreationPass(cpg: Cpg, astGenRunnerResult: AstGenRunnerResult, config: Config, report: Report = new Report())(
  implicit withSchemaValidation: ValidationMode
) extends ForkJoinParallelCpgPass[String](cpg) {

  private val logger: Logger = LoggerFactory.getLogger(classOf[AstCreationPass])

  private val global = new Global()

  def typesSeen(): List[String] = global.usedTypes.keys().asScala.filterNot(Defines.SwiftTypes.contains).toList

  override def generateParts(): Array[String] = astGenRunnerResult.parsedFiles.toArray

  override def finish(): Unit = {
    astGenRunnerResult.skippedFiles.foreach { skippedFile =>
      val filePath = Paths.get(skippedFile)
      val fileLOC = Try(IOUtils.readLinesInFile(filePath)) match {
        case Success(fileContent) => fileContent.size
        case Failure(exception) =>
          logger.warn(s"Failed to read file: '$filePath'", exception)
          -1
      }
      report.addReportInfo(skippedFile, fileLOC)
    }
  }

  override def runOnPart(diffGraph: DiffGraphBuilder, input: String): Unit = {
    val ((gotCpg, filename), duration) = TimeUtils.time {
      SwiftJsonParser.readFile(Paths.get(input)) match {
        case Success(parseResult) =>
          report.addReportInfo(parseResult.filename, parseResult.loc, parsed = true)
          Try {
            val localDiff = new AstCreator(config, global, parseResult).createAst()
            diffGraph.absorb(localDiff)
          } match {
            case Failure(exception) =>
              logger.warn(s"Failed to generate a CPG for: '${parseResult.filename}'", exception)
              (false, parseResult.filename)
            case Success(_) =>
              logger.debug(s"Generated a CPG for: '${parseResult.filename}'")
              (true, parseResult.filename)
          }
        case Failure(exception) =>
          logger.warn(s"Failed to read '$input'", exception)
          (false, input)
      }
    }
    report.updateReport(filename, cpg = gotCpg, duration)
  }

}
