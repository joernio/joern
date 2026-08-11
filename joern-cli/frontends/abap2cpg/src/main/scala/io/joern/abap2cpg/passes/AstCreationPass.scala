package io.joern.abap2cpg.passes

import io.joern.abap2cpg.Config
import io.joern.abap2cpg.parser.AbapJsonParser
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.utils.{Report, TimeUtils}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.ForkJoinParallelCpgPass
import org.slf4j.LoggerFactory

import java.nio.file.Paths
import scala.util.{Failure, Success, Try}

class AstCreationPass(cpg: Cpg, jsonFiles: List[String], config: Config, report: Report = new Report())
    extends ForkJoinParallelCpgPass[String](cpg) {

  private val logger = LoggerFactory.getLogger(classOf[AstCreationPass])
  private val parser = AbapJsonParser()

  override def generateParts(): Array[String] = jsonFiles.toArray

  override def runOnPart(diffGraph: DiffGraphBuilder, jsonFile: String): Unit = {
    implicit val schemaValidation: ValidationMode = ValidationMode.Enabled

    val ((gotCpg, filename), duration) = TimeUtils.time {
      parser.parseFile(Paths.get(jsonFile)) match {
        case Success(program) =>
          report.addParsedFile(program.fileName, config.inputPath)
          Try {
            val astCreator = new AstCreator(program, program.fileName)
            diffGraph.absorb(astCreator.createAst())
          } match {
            case Success(_) =>
              logger.debug(s"Generated a CPG for: '${program.fileName}'")
              (true, program.fileName)
            case Failure(exception) =>
              logger.warn(s"Failed to generate a CPG for: '${program.fileName}'", exception)
              (false, program.fileName)
          }
        case Failure(exception) =>
          logger.warn(s"Failed to parse '$jsonFile': ${exception.getMessage}")
          (false, jsonFile)
      }
    }
    report.updateReport(filename, cpg = gotCpg, duration)
  }
}
